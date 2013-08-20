// {{{ GPL License 

// This file is part of gringo - a grounder for logic programs.
// Copyright (C) 2013  Roland Kaminski

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// }}}

#include "gringo/input/programbuilder.hh"
#include "gringo/input/literal.hh"
#include "gringo/input/aggregate.hh"
#include "gringo/input/statement.hh"
#include "gringo/input/program.hh"
#include "gringo/output/output.hh"

namespace Gringo { namespace Input {

// {{{ definition of NongroundProgramBuilder

NongroundProgramBuilder::NongroundProgramBuilder(Program &prg, Output::OutputBase &out, Defines &defs)
    : prg_(prg)
    , out(out)
    , defs_(defs) { }

// {{{ terms

TermUid NongroundProgramBuilder::term(Location const &loc, Value val) {
    return terms_.insert(make_locatable<ValTerm>(loc, val));
}

TermUid NongroundProgramBuilder::term(Location const &loc, FWString name) {
    if (*name == "_") { return terms_.insert(make_locatable<VarTerm>(loc, name, nullptr)); }
    else {
        auto &ret(vals_[name]);
        if (!ret) { ret = std::make_shared<Value>(); }
        return terms_.insert(make_locatable<VarTerm>(loc, name, ret));
    }
}

TermUid NongroundProgramBuilder::term(Location const &loc, UnOp op, TermUid a) {
    return term(loc, op, termvec(termvec(), a));
}

TermUid NongroundProgramBuilder::term(Location const &loc, UnOp op, TermVecUid a) {
    UTermVec vec(termvecs_.erase(a));
    if (vec.size() == 1) { return terms_.insert(make_locatable<UnOpTerm>(loc, op, std::move(vec.front()))); }
    else {
        UTermVec pool;
        for (auto &terms : vec) { pool.emplace_back(make_locatable<UnOpTerm>(loc, op, std::move(terms))); }
        return terms_.insert(make_locatable<PoolTerm>(loc, std::move(pool)));
    }
}

TermUid NongroundProgramBuilder::term(Location const &loc, BinOp op, TermUid a, TermUid b) {
    return terms_.insert(make_locatable<BinOpTerm>(loc, op, terms_.erase(a), terms_.erase(b)));
}

TermUid NongroundProgramBuilder::term(Location const &loc, TermUid a, TermUid b) {
    return terms_.insert(make_locatable<DotsTerm>(loc, terms_.erase(a), terms_.erase(b)));
}

TermUid NongroundProgramBuilder::term(Location const &loc, FWString name, TermVecVecUid a, bool lua) {
    auto create = [&lua, &name, &loc](UTermVec &&vec) -> UTerm {
        // lua terms
        if (lua) { return make_locatable<LuaTerm>(loc, name, std::move(vec)); }
        // one elementary tuples
        else if (*name == "" && vec.size() == 1) { return UTerm(std::move(vec.front())); }
        // constant symbols
        else if (vec.empty()) { return make_locatable<ValTerm>(loc, Value(name)); }
        // function terms
        else { return make_locatable<FunctionTerm>(loc, name, std::move(vec)); }
    };
    TermVecVecs::value_type vec(termvecvecs_.erase(a));
    // no pooling
    if (vec.size() == 1) { return terms_.insert(create(std::move(vec.front()))); }
    // pooling
    else {
        UTermVec pool;
        for (auto &terms : vec) { pool.emplace_back(create(std::move(terms))); }
        return terms_.insert(make_locatable<PoolTerm>(loc, std::move(pool)));
    }
}

// }}}
// {{{ termvecs

TermVecUid NongroundProgramBuilder::termvec() {
    return termvecs_.emplace();
}

TermVecUid NongroundProgramBuilder::termvec(TermVecUid uid, TermUid term) {
    termvecs_[uid].emplace_back(terms_.erase(term));
    return uid;
}

// }}}
// {{{ termvecvecs

TermVecVecUid NongroundProgramBuilder::termvecvec() {
    return termvecvecs_.emplace();
}

TermVecVecUid NongroundProgramBuilder::termvecvec(TermVecVecUid uid, TermVecUid termvecUid) {
    termvecvecs_[uid].emplace_back(termvecs_.erase(termvecUid));
    return uid;
}

// }}}
// {{{ literals

LitUid NongroundProgramBuilder::boollit(Location const &loc, bool type) {
    return rellit(loc, type ? Relation::EQ : Relation::NEQ, term(loc, Value(0)), term(loc, Value(0)));
}

LitUid NongroundProgramBuilder::predlit(Location const &loc, NAF naf, bool neg, FWString name, TermVecVecUid tvvUid) {
    if (neg) {
        for (auto &x : termvecvecs_[tvvUid]) { prg_.addClassicalNegation(Signature(name, x.size())); }
    }
    return lits_.insert(make_locatable<PredicateLiteral>(loc, naf, terms_.erase(term(loc, FWString(neg ? "-" + *name : name ), tvvUid, false))));
}

LitUid NongroundProgramBuilder::rellit(Location const &loc, Relation rel, TermUid termUidLeft, TermUid termUidRight) {
    return lits_.insert(make_locatable<RelationLiteral>(loc, rel, terms_.erase(termUidLeft), terms_.erase(termUidRight)));
}

// }}}
// {{{ literal vectors

LitVecUid NongroundProgramBuilder::litvec() {
    return litvecs_.emplace();
}

LitVecUid NongroundProgramBuilder::litvec(LitVecUid uid, LitUid literalUid) {
    litvecs_[uid].emplace_back(lits_.erase(literalUid));
    return uid;
}

// }}}
// {{{ body aggregate element vectors

BdAggrElemVecUid NongroundProgramBuilder::bodyaggrelemvec() {
    return bodyaggrelemvecs_.emplace();
}

BdAggrElemVecUid NongroundProgramBuilder::bodyaggrelemvec(BdAggrElemVecUid uid, TermVecUid termvec, LitVecUid litvec) {
    bodyaggrelemvecs_[uid].emplace_back(termvecs_.erase(termvec), litvecs_.erase(litvec));
    return uid;
}

CondLitVecUid NongroundProgramBuilder::condlitvec() {
    return condlitvecs_.emplace();
}

CondLitVecUid NongroundProgramBuilder::condlitvec(CondLitVecUid uid, LitUid lit, LitVecUid litvec) {
    condlitvecs_[uid].emplace_back(lits_.erase(lit), litvecs_.erase(litvec));
    return uid;
}

// }}}
// {{{ head aggregate element vectors

HdAggrElemVecUid NongroundProgramBuilder::headaggrelemvec() {
    return headaggrelemvecs_.emplace();
}

HdAggrElemVecUid NongroundProgramBuilder::headaggrelemvec(HdAggrElemVecUid uid, TermVecUid termvec, LitUid lit, LitVecUid litvec) {
    headaggrelemvecs_[uid].emplace_back(termvecs_.erase(termvec), lits_.erase(lit), litvecs_.erase(litvec));
    return uid;
}

// }}}
// {{{ bounds

BoundVecUid NongroundProgramBuilder::boundvec() {
    return bounds_.emplace();
}

BoundVecUid NongroundProgramBuilder::boundvec(BoundVecUid uid, Relation rel, TermUid term) {
    bounds_[uid].emplace_back(rel, std::move(terms_.erase(term)));
    return uid;
}

// }}}
// {{{ rule bodies

BdLitVecUid NongroundProgramBuilder::body() {
    return bodies_.emplace();
}

BdLitVecUid NongroundProgramBuilder::bodylit(BdLitVecUid body, LitUid bodylit) {
    bodies_[body].push_back(make_unique<SimpleBodyLiteral>(lits_.erase(bodylit)));
    return body;
}

BdLitVecUid NongroundProgramBuilder::bodyaggr(BdLitVecUid body, Location const &loc, NAF naf, AggregateFunction fun, BoundVecUid bounds, BdAggrElemVecUid bodyaggrelemvec) {
    bodies_[body].push_back(make_locatable<TupleBodyAggregate>(loc, naf, fun, bounds_.erase(bounds), bodyaggrelemvecs_.erase(bodyaggrelemvec)));
    return body;
}

BdLitVecUid NongroundProgramBuilder::bodyaggr(BdLitVecUid body, Location const &loc, NAF naf, AggregateFunction fun, BoundVecUid bounds, CondLitVecUid condlitvecuid) {
    bodies_[body].push_back(make_locatable<LitBodyAggregate>(loc, naf, fun, bounds_.erase(bounds), condlitvecs_.erase(condlitvecuid)));
    return body;
}

BdLitVecUid NongroundProgramBuilder::conjunction(BdLitVecUid body, Location const &loc, LitUid head, LitVecUid litvec) {
    bodies_[body].push_back(make_locatable<Conjunction>(loc, lits_.erase(head), litvecs_.erase(litvec)));
    return body;
}

// }}}
// {{{ rule heads

HdLitUid NongroundProgramBuilder::headlit(LitUid lit) {
    return heads_.insert(make_unique<SimpleHeadLiteral>(lits_.erase(lit)));
}

HdLitUid NongroundProgramBuilder::headaggr(Location const &loc, AggregateFunction fun, BoundVecUid bounds, HdAggrElemVecUid headaggrelemvec) {
    return heads_.insert(make_locatable<TupleHeadAggregate>(loc, fun, bounds_.erase(bounds), headaggrelemvecs_.erase(headaggrelemvec)));
}

HdLitUid NongroundProgramBuilder::headaggr(Location const &loc, AggregateFunction fun, BoundVecUid bounds, CondLitVecUid condlitvec) {
    return heads_.insert(make_locatable<LitHeadAggregate>(loc, fun, bounds_.erase(bounds), condlitvecs_.erase(condlitvec)));
}

HdLitUid NongroundProgramBuilder::disjunction(Location const &loc, CondLitVecUid condlitvec) {
    return heads_.insert(make_locatable<Disjunction>(loc, condlitvecs_.erase(condlitvec)));
}

// }}}
// {{{ statements

void NongroundProgramBuilder::rule(Location const &loc, HdLitUid head) {
    rule(loc, head, body());
}

void NongroundProgramBuilder::rule(Location const &loc, HdLitUid head, BdLitVecUid body) {
    prg_.add(make_locatable<Rule>(loc, heads_.erase(head), bodies_.erase(body)));
}

void NongroundProgramBuilder::define(Location const &loc, FWString name, TermUid value) {
    defs_.add(loc, name, terms_.erase(value), defaultDef);
}

void NongroundProgramBuilder::optimize(Location const &loc, TermUid weight, TermUid priority, TermVecUid cond, BdLitVecUid body) {
    prg_.add(make_locatable<WeakConstraint>(loc, terms_.erase(weight), terms_.erase(priority), termvecs_.erase(cond), bodies_.erase(body)));
}

void NongroundProgramBuilder::showsig(Location const &loc, FWSignature x) {
    out.outPreds.emplace_back(loc, *x);
}

void NongroundProgramBuilder::show(Location const &loc, TermUid t, BdLitVecUid body) {
    rule(loc, headlit(predlit(loc, NAF::POS, false, "#show", termvecvec(termvecvec(), termvec(termvec(), t)))), body);
}

void NongroundProgramBuilder::incr(FWString name) { prg_.incr = name; }
// }}}

NongroundProgramBuilder::~NongroundProgramBuilder() { }

// }}}

} } // namespace Input Gringo

