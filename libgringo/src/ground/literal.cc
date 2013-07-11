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

#include "gringo/ground/literal.hh"
#include "gringo/ground/binders.hh"
#include "gringo/logger.hh"
#include <cmath>

namespace Gringo { namespace Ground {

namespace {

// {{{ declaration of RangeBinder

struct RangeBinder : Binder {
    RangeBinder(UTerm &&assign, RangeLiteralShared &range) 
        : assign(std::move(assign))
        , range(range) { }
    virtual IndexUpdater *getUpdater() { return nullptr; }
    virtual void match() {
        Value l{range.first->eval()}, r{range.second->eval()};
        if (l.type() == Value::NUM && r.type() == Value::NUM) {
            current = l.num();
            end     = r.num();
        }
        else {
            current = 1;
            end     = 0;
        }
    }
    virtual bool next() {
        // Note: if assign does not match it is not a variable and will not match at all
        //       if assign is just a number, then this is handled in the corresponding Binder
        return current <= end && assign->match(Value(current++));
    }
    virtual void start()  { }
    virtual void finish() { }
    virtual void print(std::ostream &out) const { out << *assign << "=" << *range.first << ".." << *range.second; }
    virtual ~RangeBinder() { }

    UTerm               assign;
    RangeLiteralShared &range;
    int                 current = 0;
    int                 end     = 0;
    
};

// }}}
// {{{ declaration of RangeMatcher

struct RangeMatcher : Binder {
    RangeMatcher(Term &assign, RangeLiteralShared &range) 
        : assign(assign)
        , range(range) { }
    virtual IndexUpdater *getUpdater() { return nullptr; }
    virtual void match() {
        Value l{range.first->eval()}, r{range.second->eval()}, a{assign.eval()};
        firstMatch = 
            l.type() == Value::NUM && 
            r.type() == Value::NUM && 
            a.type() == Value::NUM &&
            l.num() <= a.num() && a.num() <= r.num();
    }
    virtual bool next() {
        bool m = firstMatch;
        firstMatch = false;
        return m;
    }
    virtual void start()  { }
    virtual void finish() { }
    virtual void print(std::ostream &out) const { out << assign << "=" << *range.first << ".." << *range.second; }
    Term               &assign;
    RangeLiteralShared &range;
    bool                firstMatch = false;
};

// }}}

// {{{ declaration of RelationMatcher

struct RelationMatcher : Binder {
    RelationMatcher(RelationShared &shared)
        : shared(shared) { }
    virtual IndexUpdater *getUpdater() { return nullptr; }
    virtual void match() {
        Value l(std::get<1>(shared)->eval());
        Value r(std::get<2>(shared)->eval());
        switch (std::get<0>(shared)) {
            case Relation::GT:     { firstMatch = l >  r; break; }
            case Relation::LT:     { firstMatch = l <  r; break; }
            case Relation::GEQ:    { firstMatch = l >= r; break; }
            case Relation::LEQ:    { firstMatch = l <= r; break; }
            case Relation::NEQ:    { firstMatch = l != r; break; }
            case Relation::ASSIGN:
            case Relation::EQ:     { firstMatch = l == r; break; }
        }
    }
    virtual bool next() { 
        bool ret = firstMatch;
        firstMatch = false;
        return ret;
    }
    virtual void start()  { }
    virtual void finish() { }
    virtual void print(std::ostream &out) const { out << *std::get<1>(shared) << std::get<0>(shared) << *std::get<2>(shared); }
    virtual ~RelationMatcher() { }
    
    RelationShared &shared;
    bool firstMatch = false;
};

// }}}
// {{{ declaration of AssignBinder

struct AssignBinder : Binder {
    AssignBinder(UTerm &&lhs, Term &rhs)
        : lhs(std::move(lhs))
        , rhs(rhs) { }
    virtual IndexUpdater *getUpdater() { return nullptr; }
    virtual void match()               { firstMatch = lhs->match(rhs.eval()); }
    virtual bool next() { 
        bool ret = firstMatch;
        firstMatch = false;
        return ret;
    }
    virtual void start()  { }
    virtual void finish() { }
    virtual void print(std::ostream &out) const { out << *lhs << "=" << rhs; }
    UTerm lhs;
    Term &rhs;
    bool firstMatch = false;
};


// }}}

} // namespace

// {{{ definition of PredicateLiteral::BodyOccurrence

UGTerm PredicateLiteral::getRepr() const          { return repr->gterm(); }
bool PredicateLiteral::isPositive() const         { return gLit.naf == NAF::POS; }
bool PredicateLiteral::isNegative() const         { return gLit.naf != NAF::POS; }
void PredicateLiteral::setType(OccurrenceType x)  { type = x; }
OccurrenceType PredicateLiteral::getType() const  { return type; }
BodyOcc::DefinedBy &PredicateLiteral::definedBy() { return defs; }
void PredicateLiteral::checkDefined(LocSet &done, SigSet const &edb) const { 
    if (defs.empty() && done.find(repr->loc()) == done.end() && edb.find(repr->getSig()) == edb.end()) {
        done.insert(repr->loc());
        GRINGO_REPORT(W_ATOM_UNDEFINED)
            << repr->loc() << ": warning: atom is undefined:\n"
            << "  " << *this << "\n";
    }
}

// }}}

// {{{ definition of *Literal::*Literal

RangeLiteral::RangeLiteral(Term const &assign, Term const &lower, Term const &upper)
    : assign(assign.clone())
    , range(UTerm(lower.clone()), UTerm(upper.clone())) { }

RelationLiteral::RelationLiteral(Relation rel, Term const &left, Term const &right)
    : shared(rel, UTerm(left.clone()), UTerm(right.clone())) { }

PredicateLiteral::PredicateLiteral(PredicateDomain &domain, NAF naf, UTerm &&repr)
    : repr(std::move(repr))
    , domain(domain) { gLit.naf = naf; }

// }}}
// {{{ definition of *Literal::print

void RangeLiteral::print(std::ostream &out) const     { out << *assign << "=" << *range.first << ".." << *range.second; }
void RelationLiteral::print(std::ostream &out) const  { out << *std::get<1>(shared) << std::get<0>(shared) << *std::get<2>(shared); }
void PredicateLiteral::print(std::ostream &out) const { 
    out << gLit.naf << *repr;
    switch (type) {
        case OccurrenceType::POSITIVELY_STRATIFIED: { break; }
        case OccurrenceType::STRATIFIED:            { out << "!"; break; }
        case OccurrenceType::UNSTRATIFIED:          { out << "?"; break; }
    }
}

// }}}
// {{{ definition of *Literal::isRecursive

bool RangeLiteral::isRecursive() const     { return false; }
bool RelationLiteral::isRecursive() const  { return false; }
bool PredicateLiteral::isRecursive() const { return type == OccurrenceType::UNSTRATIFIED; }

// }}}
// {{{ definition of *Literal::occurrence

BodyOcc *RangeLiteral::occurrence()     { return nullptr; }
BodyOcc *RelationLiteral::occurrence()  { return nullptr; }
BodyOcc *PredicateLiteral::occurrence() { return this; }

// }}}
// {{{ definition of *Literal::collect

void RangeLiteral::collect(VarTermBoundVec &vars) const {
    assign->collect(vars, true);
    range.first->collect(vars, false);
    range.second->collect(vars, false);
}
void RelationLiteral::collect(VarTermBoundVec &vars) const { 
    std::get<1>(shared)->collect(vars, std::get<0>(shared) == Relation::ASSIGN);
    std::get<2>(shared)->collect(vars, false);
}
void PredicateLiteral::collect(VarTermBoundVec &vars) const { repr->collect(vars, gLit.naf == NAF::POS); }

// }}}
// {{{ definition of Literal::collectImportant

void Literal::collectImportant(Term::VarSet &vars) {
    auto occ(occurrence());
    if (occ && occ->getType() != OccurrenceType::POSITIVELY_STRATIFIED) {
        VarTermBoundVec x;
        collect(x);
        for (auto &occ : x) { vars.emplace(occ.first->name); }
    }
}

// }}}
// {{{ definition of *Literal::index

UIdx RangeLiteral::index(BinderType, Term::VarSet &bound) {
    if (assign->bind(bound)) { return make_unique<RangeBinder>(get_clone(assign), range); }
    else                     { return make_unique<RangeMatcher>(*assign, range); }
}
UIdx RelationLiteral::index(BinderType, Term::VarSet &bound) {
    if (std::get<0>(shared) == Relation::ASSIGN) {
        UTerm clone(std::get<1>(shared)->clone());
        VarTermVec occBound;
        if (clone->bind(bound)) { return make_unique<AssignBinder>(std::move(clone), *std::get<2>(shared)); }
    }
    return make_unique<RelationMatcher>(shared);
}
UIdx PredicateLiteral::index(BinderType type, Term::VarSet &bound) {
    return make_binder(domain, gLit.naf, *repr, gLit.repr, type, isRecursive(), bound);
}

// }}}
// {{{ definition of *Literal::score

Literal::Score RangeLiteral::score(Term::VarSet const &) { 
    if (range.first->getInvertibility() == Term::CONSTANT && range.second->getInvertibility() == Term::CONSTANT) {
        Value l(range.first->eval());
        Value r(range.second->eval());
        return (l.type() == Value::NUM && r.type() == Value::NUM) ? r.num() - l.num() : -1;
    }
    return 0;
}
Literal::Score RelationLiteral::score(Term::VarSet const &) { 
    return -1;
}
Literal::Score PredicateLiteral::score(Term::VarSet const &bound) {
    return gLit.naf == NAF::POS ? repr->estimate(domain.exports.size(), bound) : 0;
}

// }}}
// {{{ definition of *Literal::toOutput

Output::Literal *RangeLiteral::toOutput()     { return nullptr; }
Output::Literal *RelationLiteral::toOutput()  { return nullptr; }
Output::Literal *PredicateLiteral::toOutput() { 
    switch (gLit.naf) {
        case NAF::POS:
        case NAF::NOTNOT: { return !gLit.repr->second.fact(isRecursive()) ? &gLit : nullptr; }
        case NAF::NOT:    { return !gLit.repr->second.isFalse() ? &gLit : nullptr; }
    }
    assert(false);
    return nullptr;
}

// }}}
// {{{ definition of *Literal::~*Literal

RangeLiteral::~RangeLiteral() { }
RelationLiteral::~RelationLiteral() { }
PredicateLiteral::~PredicateLiteral() { }

// }}}

} } // namespace Ground Gringo
