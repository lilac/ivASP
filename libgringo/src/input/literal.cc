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

#include "gringo/input/literal.hh"
#include "gringo/output/output.hh"
#include "gringo/utility.hh"

namespace Gringo { namespace Input {

// {{{ definition of Literal::print

inline void PredicateLiteral::print(std::ostream &out) const { out << naf << *repr; }
inline void RelationLiteral::print(std::ostream &out) const  { out << *left << rel << *right; }
inline void RangeLiteral::print(std::ostream &out) const     { out << "#range(" << *assign << "," << *lower << "," << *upper << ")"; }
inline void FalseLiteral::print(std::ostream &out) const     { out << "#false"; }

// }}}
// {{{ definition of Literal::clone

inline PredicateLiteral *PredicateLiteral::clone() const {
    return make_locatable<PredicateLiteral>(loc(), naf, get_clone(repr)).release();
}
inline RelationLiteral *RelationLiteral::clone() const {
    return make_locatable<RelationLiteral>(loc(), rel, get_clone(left), get_clone(right)).release();
}
inline RangeLiteral *RangeLiteral::clone() const {
    return make_locatable<RangeLiteral>(loc(), get_clone(assign), get_clone(lower), get_clone(upper)).release();
}
inline FalseLiteral *FalseLiteral::clone() const {
    return make_locatable<FalseLiteral>(loc()).release();
}

// }}}
// {{{ definition of Literal::simplify

void PredicateLiteral::simplify(ProjectionMap &project, Term::DotsMap &dots, unsigned &auxNum, bool positional) {
    if (repr->simplify(dots, auxNum, positional, false).update(repr).project) {
        unsigned projectNum(0);
        auto ret(repr->project(projectNum));
        Term::replace(repr, std::move(std::get<0>(ret)));
        auto retName = std::get<2>(project).emplace(std::get<2>(ret).get(), FWString(0u));
        if (retName.second) { retName.first->second = Term::uniqueName(std::get<0>(project), "#p"); }
        repr->rename(retName.first->second);
        std::get<1>(ret)->rename(retName.first->second);
        if (retName.second) { std::get<1>(project).emplace_back(std::move(std::get<1>(ret)), std::move(std::get<2>(ret))); }
    }
}
void RelationLiteral::simplify(ProjectionMap &, Term::DotsMap &dots, unsigned &auxNum, bool) {
    left->simplify(dots, auxNum, false, false).update(left);
    right->simplify(dots, auxNum, false, false).update(right);
}
void RangeLiteral::simplify(ProjectionMap &, Term::DotsMap &, unsigned &, bool) {
    throw std::logic_error("RangeLiteral::simplify should never be called  if used properly");
}
void FalseLiteral::simplify(ProjectionMap &, Term::DotsMap &, unsigned &, bool) { }

// }}}
// {{{ definition of Literal::collect

void PredicateLiteral::collect(VarTermBoundVec &vars, bool bound) const {
    repr->collect(vars, bound && naf == NAF::POS);
}
void RelationLiteral::collect(VarTermBoundVec &vars, bool bound) const { 
    left->collect(vars, bound && rel == Relation::ASSIGN);
    right->collect(vars, false);
}
void RangeLiteral::collect(VarTermBoundVec &vars, bool bound) const {
    assign->collect(vars, bound);
    lower->collect(vars, false);
    upper->collect(vars, false);
}
void FalseLiteral::collect(VarTermBoundVec &, bool) const { }

// }}}
// {{{ definition of Literal::operator==

inline bool PredicateLiteral::operator==(Literal const &x) const {
    auto t = dynamic_cast<PredicateLiteral const *>(&x);
    return t && naf == t->naf && is_value_equal_to(repr, t->repr);
}
inline bool RelationLiteral::operator==(Literal const &x) const {
    auto t = dynamic_cast<RelationLiteral const *>(&x);
    return t && rel == t->rel && is_value_equal_to(left, t->left) && is_value_equal_to(right, t->right);
}
inline bool RangeLiteral::operator==(Literal const &x) const {
    auto t = dynamic_cast<RangeLiteral const *>(&x);
    return t && is_value_equal_to(assign, t->assign) && is_value_equal_to(lower, t->lower) && is_value_equal_to(upper, t->upper);
}
inline bool FalseLiteral::operator==(Literal const &x) const {
    return dynamic_cast<FalseLiteral const *>(&x);
}

// }}}
// {{{ definition of Literal::rewriteArithmetics

void PredicateLiteral::rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &, unsigned &auxNum) {
    if (naf == NAF::POS) { Term::replace(repr, repr->rewriteArithmetics(arith, auxNum)); }
}
void RelationLiteral::rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &assign, unsigned &auxNum) {
    if (rel == Relation::ASSIGN) {
        if (right->hasVar()) {
            assign.emplace_back(get_clone(right), get_clone(left));
            Term::replace(assign.back().first, assign.back().first->rewriteArithmetics(arith, auxNum));
        }
        Term::replace(left, left->rewriteArithmetics(arith, auxNum));
    }
}
void RangeLiteral::rewriteArithmetics(Term::ArithmeticsMap &arith, AssignVec &, unsigned &auxNum) {
    Term::replace(this->assign, this->assign->rewriteArithmetics(arith, auxNum));
}
void FalseLiteral::rewriteArithmetics(Term::ArithmeticsMap &, AssignVec &, unsigned &) { }

// }}}
// {{{ definition of Literal::hash

inline size_t PredicateLiteral::hash() const {
    return get_value_hash(typeid(PredicateLiteral).hash_code(), size_t(naf), repr);
}
inline size_t RelationLiteral::hash() const {
    return get_value_hash(typeid(RelationLiteral).hash_code(), size_t(rel), left, right);
}
inline size_t RangeLiteral::hash() const {
    return get_value_hash(typeid(RangeLiteral).hash_code(), assign, lower, upper);
}
inline size_t FalseLiteral::hash() const {
    return get_value_hash(typeid(FalseLiteral).hash_code());
}

// }}}
// {{{ definition of Literal::unpool

inline ULitVec PredicateLiteral::unpool() const {
    ULitVec value;
    auto f = [&](UTerm &&y){ value.emplace_back(make_locatable<PredicateLiteral>(loc(), naf, std::move(y))); };
    Term::unpool(repr, Gringo::unpool, f);
    return value;
}
inline ULitVec RelationLiteral::unpool() const {
    ULitVec value;
    auto f = [&](UTerm &&l, UTerm &&r) { value.emplace_back(make_locatable<RelationLiteral>(loc(), rel, std::move(l), std::move(r))); };
    Term::unpool(left, right, Gringo::unpool, Gringo::unpool, f);
    return value;
}
inline ULitVec RangeLiteral::unpool() const {
    throw std::logic_error("RangeLiteral::simplify should never be called  if used properly");
}
inline ULitVec FalseLiteral::unpool() const {
    return ULitVec();
}

// }}}
// {{{ definition of Literal::toTuple

void PredicateLiteral::toTuple(UTermVec &tuple, int &) {
    int id = 0;
    switch (naf) {
        case NAF::POS:    { id = 0; break; }
        case NAF::NOT:    { id = 1; break; }
        case NAF::NOTNOT: { id = 2; break; }
    }
    tuple.emplace_back(make_locatable<ValTerm>(loc(), Value(id)));
    tuple.emplace_back(get_clone(repr));
}
void RelationLiteral::toTuple(UTermVec &tuple, int &id) {
    tuple.emplace_back(make_locatable<ValTerm>(loc(), Value(id+3)));
    tuple.emplace_back(get_clone(left));
    tuple.emplace_back(get_clone(right));
    id++;
}
void RangeLiteral::toTuple(UTermVec &tuple, int &id) {
    tuple.emplace_back(make_locatable<ValTerm>(loc(), Value(id+3)));
    tuple.emplace_back(get_clone(assign));
    tuple.emplace_back(get_clone(lower));
    tuple.emplace_back(get_clone(upper));
    id++;
}
void FalseLiteral::toTuple(UTermVec &tuple, int &id) {
    tuple.emplace_back(make_locatable<ValTerm>(loc(), Value(id+3)));
    id++;
}

// }}}
// {{{ definition of Literal::isEDB

Value Literal::isEDB() const          { return {}; }
Value PredicateLiteral::isEDB() const { return naf == NAF::POS ? repr->isEDB() : Value(); }

// }}}
// {{{ definition of Literal::hasPool

inline bool PredicateLiteral::hasPool() const { return repr->hasPool(); }
inline bool RelationLiteral::hasPool() const  { return left->hasPool() || right->hasPool(); }
inline bool RangeLiteral::hasPool() const     { return assign->hasPool() || lower->hasPool() || upper->hasPool(); }
inline bool FalseLiteral::hasPool() const     { return false; }

// }}}
// {{{ definition of Literal::replace

inline void PredicateLiteral::replace(Defines &x) { Term::replace(repr, repr->replace(x, false)); }
inline void RelationLiteral::replace(Defines &x) {
    Term::replace(left, left->replace(x)); 
    Term::replace(left, right->replace(x));
}
inline void RangeLiteral::replace(Defines &x) {
    Term::replace(assign, assign->replace(x));
    Term::replace(lower, lower->replace(x));
    Term::replace(upper, upper->replace(x));
}
inline void FalseLiteral::replace(Defines &) { }

// }}}
// {{{ definition of Literal::toGround

inline Ground::ULit PredicateLiteral::toGround(PredDomMap &x) const {
    return make_unique<Ground::PredicateLiteral>(add(x, repr->getSig()), naf, get_clone(repr));
}
inline Ground::ULit RelationLiteral::toGround(PredDomMap &) const {
    return make_unique<Ground::RelationLiteral>(rel, *left, *right);
}
inline Ground::ULit RangeLiteral::toGround(PredDomMap &) const {
    return make_unique<Ground::RangeLiteral>(*assign, *lower, *upper);
}
inline Ground::ULit FalseLiteral::toGround(PredDomMap &) const {
    throw std::runtime_error("think!!!");
}

// }}}
// {{{ definition of Literal::shift

ULit PredicateLiteral::shift(bool negate) {
    if (naf == NAF::POS) { return nullptr; }
    else { 
        NAF inv = (naf == NAF::NOT) == negate ? NAF::NOTNOT : NAF::NOT;
        return make_locatable<PredicateLiteral>(loc(), inv, std::move(repr));
    }
}
ULit RelationLiteral::shift(bool negate) {
    return make_locatable<RelationLiteral>(loc(), negate ? neg(rel) : rel, std::move(left), std::move(right));
}
ULit RangeLiteral::shift(bool) {
    throw std::logic_error("cannot happen");
}
ULit FalseLiteral::shift(bool) {
    return nullptr;
}

// }}}
// {{{ definition of Literal::headRepr

UTerm PredicateLiteral::headRepr() const {
    assert(naf == NAF::POS);
    return get_clone(repr);
}
UTerm RelationLiteral::headRepr() const {
    throw std::logic_error("cannot happen");
}
UTerm RangeLiteral::headRepr() const {
    throw std::logic_error("cannot happen");
}
UTerm FalseLiteral::headRepr() const {
    return nullptr;
}

// }}}

// {{{ definition of PredicateLiteral

PredicateLiteral::PredicateLiteral(NAF naf, UTerm &&repr)
    : naf(naf)
    , repr(std::move(repr)) { }

PredicateLiteral::~PredicateLiteral() { }

// }}}
// {{{ definition of RelationLiteral

RelationLiteral::RelationLiteral(Relation rel, UTerm &&left, UTerm &&right)
    : rel(rel)
    , left(std::move(left))
    , right(std::move(right)) { }

ULit RelationLiteral::make(Term::ArithmeticsMap::value_type::value_type &x) {
    Location loc(x.first->loc());
    return make_locatable<RelationLiteral>(loc, Relation::ASSIGN, std::move(x.second), get_clone(x.first));
}

ULit RelationLiteral::make(Literal::AssignVec::value_type &x) {
    Location loc(x.first->loc() + x.second->loc());
    return make_locatable<RelationLiteral>(loc, Relation::ASSIGN, std::move(x.first), get_clone(x.second));
}

RelationLiteral::~RelationLiteral() { }

// }}}
// {{{ definition of RangeLiteral

RangeLiteral::RangeLiteral(UTerm &&assign, UTerm &&lower, UTerm &&upper)
    : assign(std::move(assign))
    , lower(std::move(lower))
    , upper(std::move(upper)) { }

ULit RangeLiteral::make(Term::DotsMap::value_type &dot) {
    Location loc(std::get<0>(dot)->loc());
    return make_locatable<RangeLiteral>(loc, std::move(std::get<0>(dot)), std::move(std::get<1>(dot)), std::move(std::get<2>(dot)));
}

RangeLiteral::~RangeLiteral() { }

// }}}
// {{{ definition of RangeLiteral

FalseLiteral::FalseLiteral() { }
FalseLiteral::~FalseLiteral() { }

// }}}

} } // namespace Gringo Input

