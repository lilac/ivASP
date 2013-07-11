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

#include "gringo/bug.hh"
#include "gringo/input/statement.hh"
#include "gringo/safetycheck.hh"

namespace Gringo { namespace Input {

// {{{ definition of Statement::Statement

Rule::Rule(UHeadAggr &&head, UBodyAggrVec &&body)
    : head(std::move(head))
    , body(std::move(body)) { }

WeakConstraint::WeakConstraint(UTerm &&weight, UTerm &&prioriy, UTermVec &&tuple, UBodyAggrVec &&body)
    : body(std::move(body)) { 
    this->tuple.emplace_back(std::move(weight));
    this->tuple.emplace_back(std::move(prioriy));
    std::move(tuple.begin(), tuple.end(), std::back_inserter(this->tuple));
}

WeakConstraint::WeakConstraint(UTermVec &&tuple, UBodyAggrVec &&body)
    : tuple(std::move(tuple))
    , body(std::move(body)) { assert(tuple.size() > 1); }

// }}}
// {{{ definition of Statement::print

void Rule::print(std::ostream &out) const {
    out << *head;
    if (!body.empty()) { 
        out << ":-"; 
        auto f = [](std::ostream &out, UBodyAggr const &x) { out << *x; };
        print_comma(out, body, ";", f);
    }
    out << ".";
}

void WeakConstraint::print(std::ostream &out) const {
    out << ":~"; 
    print_comma(out, body, ";", [](std::ostream &out, UBodyAggr const &x) { out << *x; });
    out << ".[";
    out << *tuple.front() << "@" << *tuple[1];
    for (auto it(tuple.begin() + 2), ie(tuple.end()); it != ie; ++it) { out << "," << **it; }
    out << "]";
}

// }}}
// {{{ definition of Statement::isEDB

Value Statement::isEDB() const { return Value(); }
Value Rule::isEDB() const      { return body.empty() ? head->isEDB() : Value(); }

// }}}
// {{{ definition of Statement::unpool

void Rule::unpool(UStmVec &x) {
    UBodyAggrVec body;
    for (auto &y : this->body) { y->unpool(body); }
    UHeadAggrVec head;
    this->head->unpool(head);
    for (auto &y : head) { x.emplace_back(make_locatable<Rule>(loc(), std::move(y), get_clone(body))); }
}
void WeakConstraint::unpool(UStmVec &x) {
    UBodyAggrVec body;
    for (auto &y : this->body) { y->unpool(body); }
    Term::unpool(tuple.begin(), tuple.end(), Gringo::unpool, [&](UTermVec &&tuple) { x.emplace_back(make_locatable<WeakConstraint>(loc(), std::move(tuple), get_clone(body))); });
}

// }}}
// {{{ definition of Statement::hasPool

bool Rule::hasPool() const {
    for (auto &x : body) {
        if (x->hasPool()) { return true; }
    }
    return head->hasPool();
}
bool WeakConstraint::hasPool() const {
    for (auto &x : tuple) {
        if (x->hasPool()) { return true; }
    }
    for (auto &x : body) {
        if (x->hasPool()) { return true; }
    }
    return false;
}

// }}}
// {{{ definition of Statement::rewrite

namespace {

void rewriteSimplify(UBodyAggrVec &body, unsigned &auxVars, Term::DotsMap &dots, Literal::ProjectionMap &project) {
    // TODO: generic for a body
    for (auto &y : body) { y->simplify(project, dots, auxVars); }
    for (auto &y : dots) { body.emplace_back(make_unique<SimpleBodyLiteral>(RangeLiteral::make(y))); }
}
void rewriteAggregates(UBodyAggrVec &body) {
    UBodyAggrVec aggr;
    auto jt = body.begin();
    for (auto it = jt, ie = body.end(); it != ie; ++it) {
        if ((*it)->rewriteAggregates(aggr)) {
            if (it != jt) { *jt = std::move(*it); }
            ++jt;
        }
    }
    body.erase(jt, body.end());
    std::move(aggr.begin(), aggr.end(), std::back_inserter(body));
}
void rewriteArithmetics(UBodyAggrVec &body, unsigned &auxVars, Term::ArithmeticsMap &arith, Literal::AssignVec &assign) {
    for (auto &y : body) { y->rewriteArithmetics(arith, assign, auxVars); }
    for (auto &y : arith.back()) { body.emplace_back(make_unique<SimpleBodyLiteral>(RelationLiteral::make(y))); }
    for (auto &y : assign) { body.emplace_back(make_unique<SimpleBodyLiteral>(RelationLiteral::make(y))); }
}
void rewriteArithmetics(UBodyAggrVec &body, Location const &loc, Statement::SplitVec &splits) {
    using LitDep = SafetyChecker<VarTerm*, UBodyAggr>;
    using VarMap = std::unordered_map<FWString, LitDep::VarNode*>;
    unsigned numAssign = 0;
    for (auto &y : body) {
        if (y->isAssignment()) { numAssign++; }
    }
    if (numAssign) {
        LitDep dep;
        VarMap map;
        for (auto &y : body) { 
            auto &ent = dep.insertEnt(std::move(y));
            VarTermBoundVec vars;
            ent.data->collect(vars);
            for (auto &occ : vars) {
                if (occ.first->level == 0) {
                    auto &var(map[occ.first->name]);
                    if (!var)  { var = &dep.insertVar(occ.first); }
                    if (occ.second) { dep.insertEdge(ent, *var); }
                    else            { dep.insertEdge(*var, ent); }
                }
            }
        }
        LitDep::VarVec bound;
        LitDep::EntVec open;
        open.reserve(body.size()); // Note: keeps iterators valid
        body.clear();
        dep.init(open);
        UBodyAggrVec sorted;
        for (auto it = open.begin(); it != open.end(); ) {
            LitDep::EntVec assign;
            for (; it != open.end(); ++it) {
                if (!(*it)->data->isAssignment()) { 
                    dep.propagate(*it, open, &bound);
                    sorted.emplace_back(std::move((*it)->data));
                }
                else { assign.emplace_back(*it); }
            }
            LitDep::EntVec nextAssign;
            while (!assign.empty()) {
                for (auto &x : assign) { 
                    bool allBound = true;
                    for (auto &y : x->provides) {
                        if (!y->bound) {
                            allBound = false;
                            break;
                        }
                    }
                    if (!allBound) { nextAssign.emplace_back(x); }
                    else {
                        x->data->removeAssignment();
                        dep.propagate(x, open, &bound); 
                        sorted.emplace_back(std::move(x->data));
                    }
                }
                if (!nextAssign.empty()) {
                    dep.propagate(nextAssign.back(), open, &bound); 
                    sorted.emplace_back(std::move(nextAssign.back()->data));
                    nextAssign.pop_back();
                }
                assign = std::move(nextAssign);
            }
        }
        UBodyAggrVec done;
        for (auto it = sorted.begin(); it != sorted.end(); ++it) {
            done.emplace_back(std::move(*it));
            if (done.back()->isAssignment() && it + 1 != sorted.end()) { 
                VarTermBoundVec boundVec;
                for (auto &x : done) { x->collect(boundVec); }
                auto lvl = [](VarTermBoundVec::value_type const &a) { return a.first->level != 0; };
                boundVec.erase(std::remove_if(boundVec.begin(), boundVec.end(), lvl), boundVec.end());
                auto cmp = [](VarTermBoundVec::value_type const &a, VarTermBoundVec::value_type const &b) { return *a.first->name < *b.first->name; };
                std::sort(boundVec.begin(), boundVec.end(), cmp);
                auto eq  = [](VarTermBoundVec::value_type const &a, VarTermBoundVec::value_type const &b) { return a.first->name == b.first->name; };
                boundVec.erase(std::unique(boundVec.begin(), boundVec.end(), eq), boundVec.end());
                UTermVec args;
                for (auto &x : boundVec) { args.emplace_back(x.first->clone()); }
                FWString name = "#s" + std::to_string(splits.size());
                ULit split(make_locatable<PredicateLiteral>(loc, NAF::POS, make_locatable<FunctionTerm>(loc, name, std::move(args))));
                splits.emplace_back(get_clone(split), std::move(done));
                done.emplace_back(make_unique<SimpleBodyLiteral>(std::move(split)));
            }
        }
        body = std::move(done);
    }
}

} // namespace

void Rule::rewrite(Literal::ProjectionMap &project, SplitVec &splits) {
    unsigned auxVars = 0;
    { // simplify
        Term::DotsMap dots;
        head->simplify(project, dots, auxVars);
        rewriteSimplify(body, auxVars, dots, project);
    }
    { // rewrite aggregates
        Term::replace(head, head->rewriteAggregates(body));
        rewriteAggregates(body);
    }
    { // assign levels
        AssignLevel c;
        head->assignLevels(c);
        for (auto &y : body) { y->assignLevels(c); }
        c.assignLevels();
    }
    { // rewrite arithmetics
        Term::ArithmeticsMap arith;
        Literal::AssignVec assign;
        arith.emplace_back();
        head->rewriteArithmetics(arith, auxVars);
        rewriteArithmetics(body, auxVars, arith, assign);
        arith.pop_back();
    }
    rewriteArithmetics(body, loc(), splits);
}
void WeakConstraint::rewrite(Literal::ProjectionMap &project, SplitVec &splits) {
    unsigned auxVars = 0;
    { // simplify
        Term::DotsMap dots;
        for (auto &x : tuple) { x->simplify(dots, auxVars, false, false).update(x); }
        rewriteSimplify(body, auxVars, dots, project);
    }
    rewriteAggregates(body);
    { // assign levels
        AssignLevel c;
        VarTermBoundVec vars;
        for (auto &x : tuple) { x->collect(vars, false); }
        c.add(vars);
        for (auto &y : body) { y->assignLevels(c); }
        c.assignLevels();
    }
    { // rewrite arithmetics
        Term::ArithmeticsMap arith;
        Literal::AssignVec assign;
        arith.emplace_back();
        for (auto &x : tuple) { Term::replace(x, x->rewriteArithmetics(arith, auxVars)); }
        rewriteArithmetics(body, auxVars, arith, assign);
        arith.pop_back();
    }
    rewriteArithmetics(body, loc(), splits);
}

// }}}
// {{{ definition of Statement::check

bool Rule::check() const {
    bool ret = true;
    ChkLvlVec levels;
    levels.emplace_back(loc(), *this);
    ret = head->check(levels) && ret;
    for (auto &y : body) { ret = y->check(levels) && ret; }
    return levels.back().check() && ret;
}
bool WeakConstraint::check() const {
    bool ret = true;
    ChkLvlVec levels;
    levels.emplace_back(loc(), *this);
    VarTermBoundVec vars;
    levels.back().current = &levels.back().dep.insertEnt();
    for (auto &x : tuple)  { x->collect(vars, false); }
    for (auto &x : vars) { 
        auto &lvl(levels[x.first->level]);
        bool bind = x.second && levels.size() == x.first->level + 1;
        if (bind) { lvl.dep.insertEdge(*lvl.current, lvl.var(*x.first)); }
        else      { lvl.dep.insertEdge(lvl.var(*x.first), *lvl.current); }
    }
    for (auto &y : body) { ret = y->check(levels) && ret; }
    return levels.back().check() && ret;
}

// }}}
// {{{ definition of Statement::replace

void Rule::replace(Defines &x) { 
    head->replace(x);
    for (auto &y : body) { y->replace(x); }
}
void WeakConstraint::replace(Defines &x) { 
    for (auto &y : tuple) { y->replace(x); }
    for (auto &y : body) { y->replace(x); }
}

// }}}
// {{{ definition of Statement::toGround

namespace {
    
void toGround(CreateHead &&head, UBodyAggrVec const &body, ToGroundArg &x, Ground::UStmVec &stms) {
    // TODO: remove auxLits during head creation
    CreateBodyVec createVec;
    createVec.emplace_back(std::move(head.second));
    for (auto &y : body) { createVec.emplace_back(y->toGround(x, stms)); }
    Ground::ULitVec lits;
    for (auto current = createVec.begin(), end = createVec.end(); current != end; ++current) {
        current->first(lits, true);
        for (auto &z : current->second) {
            Ground::ULitVec splitLits;
            for (auto it = createVec.begin(); it != end; ++it) {
                if (it != current) { it->first(splitLits, false); }
            }
            stms.emplace_back(z(std::move(splitLits)));
        }
    }
    stms.emplace_back(head.first(std::move(lits)));
}

} // namespace

void Rule::toGround(ToGroundArg &x, Ground::UStmVec &stms) const { 
    Gringo::Input::toGround(head->toGround(x), body, x, stms);
}
void WeakConstraint::toGround(ToGroundArg &x, Ground::UStmVec &stms) const {
    CreateHead hd
        {[this](Ground::ULitVec &&lits) -> Ground::UStm { return make_unique<Ground::WeakConstraint>(get_clone(tuple), std::move(lits)); },
        { [](Ground::ULitVec &, bool) { }, { } }};
    Gringo::Input::toGround(std::move(hd), body, x, stms);
}

// }}}
// {{{ definition of Statement::~Statement

Rule::~Rule()                     { }
WeakConstraint::~WeakConstraint() { }

// }}}

} } // namespace Input Gringo

