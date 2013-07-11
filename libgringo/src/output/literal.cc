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

#include <gringo/output/literal.hh>
#include <gringo/output/statement.hh>
#include <gringo/logger.hh>

namespace Gringo { namespace Output {

// {{{ definition of AuxLiteral

AuxAtom::AuxAtom(unsigned name) : name(name) { }
int AuxAtom::lparseUid(LparseOutputter &out) {
    if (!uid) { uid = out.newUid(); }
    return uid;
}

std::ostream &operator<<(std::ostream &out, AuxAtom const &x) { 
    out << "#aux(" << x.name << ")";
    return out;
}

AuxLiteral::AuxLiteral(SAuxAtom atom, bool negative) : atom(atom), negative(negative) { }
AuxLiteral *AuxLiteral::clone() const                                         { return new AuxLiteral(*this); }
ULit AuxLiteral::toLparse(unsigned &, StmHandler const &)                     { return nullptr; }
void AuxLiteral::makeEqual(ULit &&lit, unsigned &auxAtoms, StmHandler const &x) const {
    if (!negative) { LparseRule(atom, std::move(lit), nullptr).toLparse(auxAtoms, x); }
}
void AuxLiteral::printPlain(std::ostream &out) const                          { out << (negative ? "not " : "") << *atom; }
bool AuxLiteral::isIncomplete() const                                         { return false; }
int AuxLiteral::lparseUid(LparseOutputter &out) const                         { return !negative ? atom->lparseUid(out) : -atom->lparseUid(out); }
size_t AuxLiteral::hash() const                                               { return get_value_hash(typeid(AuxLiteral).hash_code(), atom->name); }
bool AuxLiteral::operator==(Literal const &x) const { 
    AuxLiteral const *t{dynamic_cast<AuxLiteral const*>(&x)};
    return negative == t->negative && atom->name == t->atom->name;
}
AuxLiteral::~AuxLiteral() { }

// }}}
// {{{ definition of PredicateLiteral

PredicateLiteral::PredicateLiteral() = default;
PredicateLiteral::PredicateLiteral(NAF naf, PredicateDomain::element_type &repr) : naf(naf), repr(&repr) { }
void PredicateLiteral::printPlain(std::ostream &out) const {
    out << naf << repr->first;
}
bool PredicateLiteral::isIncomplete() const { return false; }
PredicateLiteral *PredicateLiteral::clone() const { return new PredicateLiteral(*this); }
ULit PredicateLiteral::toLparse(unsigned &, StmHandler const &) { return nullptr; }
void PredicateLiteral::makeEqual(ULit &&lit, unsigned &auxAtoms, StmHandler const &x) const {
    if (naf == NAF::POS) { 
        ULitVec lits;
        lits.emplace_back(std::move(lit));
        Rule(repr, std::move(lits)).toLparse(auxAtoms, x);
    }
}
int PredicateLiteral::lparseUid(LparseOutputter &out) const {
    if (!repr->second.hasUid()) { repr->second.uid(out.newUid()); }
    switch (naf) {
        case NAF::POS: { return +repr->second.uid(); }
        case NAF::NOT: { return -int(repr->second.uid()); }
        case NAF::NOTNOT: { 
            int aux(out.newUid());
            LparseOutputter::LitVec lits;
            lits.emplace_back(-int(repr->second.uid()));
            out.printBasicRule(aux, lits);
            return -aux;
        }
    }
    assert(false);
    return 0;
}
size_t PredicateLiteral::hash() const { return get_value_hash(typeid(PredicateLiteral).hash_code(), (unsigned)naf, repr->first); }
bool PredicateLiteral::operator==(Literal const &x) const { 
    PredicateLiteral const *t{dynamic_cast<PredicateLiteral const*>(&x)};
    return naf == t->naf && repr == t->repr;
}
PredicateLiteral::~PredicateLiteral() { }

// }}}
// {{{ definition of BodyAggregateState

int clamp(int64_t x) {
    if (x > std::numeric_limits<int>::max()) { return std::numeric_limits<int>::max(); }
    if (x < std::numeric_limits<int>::min()) { return std::numeric_limits<int>::min(); }
    return int(x);
}

bool neutral(ValVec const &tuple, AggregateFunction fun, Location const &loc) {
    if (tuple.empty()) { 
        if (fun == AggregateFunction::COUNT) { return false; }
        else {
            GRINGO_REPORT(W_TERM_UNDEFINED) 
                << loc << ": warning: empty tuple in " << fun << " aggregate, tuple is ignored\n";
            return true;
        }
    }
    else if (tuple.front().type() != Value::SPECIAL) {
        bool ret = true;
        switch (fun) {
            case AggregateFunction::MIN:   { return tuple.front() == Value(false); }
            case AggregateFunction::MAX:   { return tuple.front() == Value(true); }
            case AggregateFunction::COUNT: { return false; }
            case AggregateFunction::SUM:   { ret = tuple.front().type() != Value::NUM || tuple.front() == 0; break; }
            case AggregateFunction::SUMP:  { ret = tuple.front().type() != Value::NUM || tuple.front() <= 0; break; }
        }
        if (ret && tuple.front() != 0) {
            GRINGO_REPORT(W_TERM_UNDEFINED) 
                << loc << ": warning: " << fun <<  " aggregate not defined for weight, tuple is ignored:\n"
                << "  " << tuple.front() << "\n";
        }
        return ret;
    }
    return true;
}

int toInt(IntervalSet<Value>::LBound const &x) {
    if (x.bound.type() == Value::NUM) {
        return x.inclusive ? x.bound.num() : x.bound.num() + 1;
    }
    else {
        if (x.bound < 0) { return std::numeric_limits<int>::min(); }
        else             { return std::numeric_limits<int>::max(); }
    }
}

int toInt(IntervalSet<Value>::RBound const &x) {
    if (x.bound.type() == Value::NUM) {
        return x.inclusive ? x.bound.num() : x.bound.num() - 1;
    }
    else {
        if (x.bound < 0) { return std::numeric_limits<int>::min(); }
        else             { return std::numeric_limits<int>::max(); }
    }
}

Value getWeight(AggregateFunction fun, FWValVec const &x) {
    return fun == AggregateFunction::COUNT ? Value(1) : *x.begin();
}

bool BodyAggregateState::fact(bool recursive) const { return _fact && (_positive || !recursive); }
unsigned BodyAggregateState::generation() const { return _generation; }
bool BodyAggregateState::isFalse() { return state != DEFINED; }
BodyAggregateState::element_type &BodyAggregateState::ignore() {
    static element_type x{std::piecewise_construct, std::forward_as_tuple(Value("#false")), std::forward_as_tuple()};
    return x;
}
void BodyAggregateState::accumulate(ValVec const &tuple, AggregateFunction fun, bool fact, bool remove) {
    switch (fun) {
        case AggregateFunction::MIN: {
            Value val = tuple.front();
            if (fact) { valMax = std::min<Value>(valMax, val); }
            valMin = std::min<Value>(valMin, val);
            break;
        }
        case AggregateFunction::MAX: {
            Value val = tuple.front();
            if (fact) { valMin = std::max<Value>(valMin, val); }
            valMax = std::max<Value>(valMax, val);
            break;
        }
        default: {
            int val = fun == AggregateFunction::COUNT ? 1 : tuple.front().num();
            if (fact) {
                if (remove) {
                    if (val < 0) { intMax+= val; }
                    else         { intMin+= val; }
                }
                else {
                    intMin+= val;
                    intMax+= val;
                }
            }
            else {
                if (val < 0) { intMin+= val; }
                else         { intMax+= val; }
            }
            break;
        }
    }
}
void BodyAggregateState::init(AggregateFunction fun) {
    switch (fun) {
        case AggregateFunction::MIN: {
            valMin = Value(false);
            valMax = Value(false);
            break;
        }
        case AggregateFunction::MAX: {
            valMin = Value(true);
            valMax = Value(true);
            break;
        }
        default: {
            intMin = 0;
            intMax = 0;
            break;
        }
    }
}
bool BodyAggregateState::defined() const { return state == DEFINED; }
void BodyAggregateState::generation(unsigned x) { _generation = x; }
BodyAggregateState::Bounds::Interval BodyAggregateState::range(AggregateFunction fun) const { 
    if (fun != AggregateFunction::MIN && fun != AggregateFunction::MAX) {
        return {{clamp(intMin), true}, {clamp(intMax), true}};
    }
    else { return {{valMin, true}, {valMax, true}}; }
}
BodyAggregateState::~BodyAggregateState() { }

// }}}
// {{{ definition of BodyAggregate

BodyAggregate::BodyAggregate(Location const *&loc) : loc(loc) { }

namespace {

auto print_elem = [](std::ostream &out, BdAggrElemSet::value_type const &x) {
    if (x.second->empty()) { print_comma(out, x.first, ","); }
    else {
        for (auto &y : *x.second) {
            print_comma(out, x.first, ",");
            out << ":";
            using namespace std::placeholders;
            print_comma(out, y, ",", std::bind(&Literal::printPlain, _2, _1));
        }
    }
};

} // namespace

void BodyAggregate::printPlain(std::ostream &out) const {
    out << naf;
    auto it = bounds.begin(), ie = bounds.end();
    if (it != ie) { out << it->second << inv(it->first); ++it; }
    out << fun << "{";
    print_comma(out, repr->second.elems, ";", print_elem);
    out << "}";
    for (; it != ie; ++it) { out << it->first << it->second; }

}
ULit BodyAggregate::toLparse(unsigned &auxAtoms, StmHandler const &x) {
    // TODO: this algorithm suffers from severe uglyness!!!
    // TODO: check if false
    auto rng(repr->second.range(fun));
    SAuxAtom auxAtom(std::make_shared<AuxAtom>(auxAtoms++));
    if (!repr->second.bounds.contains(rng)) {
        using ULitValVec = std::vector<std::pair<ULit,Value>>;
        ULitValVec elemVec;
        for (auto &y : repr->second.elems) { 
            Value weight(getWeight(fun, y.first));
            if (y.second->size() != 1 || y.second->front().size() != 1) {
                SAuxAtom atom(std::make_shared<AuxAtom>(auxAtoms++));
                elemVec.emplace_back(make_unique<AuxLiteral>(atom, false), weight);
                // atom :- cond
                if (!y.second->empty()) {
                    std::vector<ULitVec> &conds(*y.second);
                    for (ULitVec &y : conds) {
                        LparseRule(atom, get_clone(y)).toLparse(auxAtoms, x);
                    }
                }
                else {
                    LparseRule(atom, {}).toLparse(auxAtoms, x);
                }
            }
            else { elemVec.emplace_back(get_clone(y.second->front().front()), weight); }
        }
        if (incomplete && loc && repr->second.bounds.vec.size() > 1) {
            GRINGO_REPORT(W_NONMONOTONE_AGGREGATE) 
                << *loc << ": warning: holes in range of (potentially) recursive " << fun << " aggregate:\n"
                << "  (the applied translation might produce counter-intuitive results)\n";
            loc = nullptr;
        }
        switch (fun) {
            case AggregateFunction::COUNT:
            case AggregateFunction::SUMP:
            case AggregateFunction::SUM: {
                int  shift = 0;
                WeightRule::ULitBoundVec elemPos;
                for (auto &y : elemVec) {
                    int weight = y.second.num();
                    if (weight < 0) {
                        shift+= -weight;
                        SAuxAtom neg(std::make_shared<AuxAtom>(auxAtoms++));
                        LparseRule(neg, get_clone(y.first), nullptr).toLparse(auxAtoms, x);
                        elemPos.emplace_back(make_unique<AuxLiteral>(neg, true), -weight);
                        if (incomplete && loc) {
                            GRINGO_REPORT(W_NONMONOTONE_AGGREGATE) 
                                << *loc << ": warning: negative weight in (potentially) recursive " << fun << " aggregate:\n"
                                << "  (the applied translation might produce counter-intuitive results)\n";
                            loc = nullptr;
                        }
                    }
                    else { elemPos.emplace_back(get_clone(y.first), weight); }
                }
                for (auto &y : repr->second.bounds.vec) {
                    ULitVec aggrLits;
                    if (rng.left < y.left) {
                        // aggr :- L { }.
                        SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                        WeightRule wr(aggr, std::max(0, toInt(y.left) + shift), get_clone(elemPos));
                        wr.toLparse(auxAtoms, x);
                        aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, naf == NAF::NOTNOT));
                    }
                    if (y.right < rng.right) {
                        // aggr :- U+1 { }.
                        SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                        WeightRule wr(aggr, std::max(0, toInt(y.right) + 1 + shift), get_clone(elemPos));
                        wr.toLparse(auxAtoms, x);
                        aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, naf != NAF::NOTNOT));
                    }
                    LparseRule(auxAtom, std::move(aggrLits)).toLparse(auxAtoms, x);
                }
                break;
            }
            case AggregateFunction::MIN:
            case AggregateFunction::MAX: {
                for (auto &y : repr->second.bounds) {
                    ULitVec aggrLits;
                    if (fun == AggregateFunction::MIN ? y.right < rng.right : rng.left < y.left) {
                        SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                        for (auto &z : elemVec) {
                            // aggr :- 1 { between }
                            if (y.contains(z.second)) { LparseRule(aggr, get_clone(z.first), nullptr).toLparse(auxAtoms, x); }
                        }
                        aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, naf == NAF::NOTNOT));
                    }
                    if (fun == AggregateFunction::MIN ? rng.left < y.left : y.right < rng.right) {
                        SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                        for (auto &z : elemVec) {
                            if (fun == AggregateFunction::MIN ? z.second < y : y < z.second) {
                                // if min:  aggr :- 1 { below }
                                // eif max: aggr :- 1 { above }
                                LparseRule(aggr, get_clone(z.first), nullptr).toLparse(auxAtoms, x);
                            }
                        }
                        aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, naf != NAF::NOTNOT));
                    }
                    LparseRule(auxAtom, std::move(aggrLits)).toLparse(auxAtoms, x);
                }
                break;
            }
        }
    }
    return std::make_unique<AuxLiteral>(auxAtom, naf != NAF::POS);
}
void BodyAggregate::makeEqual(ULit &&, unsigned &, StmHandler const &) const {
    throw std::runtime_error("BodyAggregate::makeEqual: must not happen!!!");
}
int BodyAggregate::lparseUid(LparseOutputter &) const {
    throw std::runtime_error("BodyAggregate::lparseUid must be called after BodyAggregate::toLparse");
}
bool BodyAggregate::isIncomplete() const { return incomplete; }
BodyAggregate *BodyAggregate::clone() const { return new BodyAggregate(*this); }
size_t BodyAggregate::hash() const { throw std::runtime_error("BodyAggregate::hash: implement me if necessary!"); }
bool BodyAggregate::operator==(Literal const &) const { throw std::runtime_error("BodyAggregate::operator==: implement me if necessary!"); }
BodyAggregate::~BodyAggregate() { }

// }}}
// {{{ definition of AssignmentAggregateState

AssignmentAggregateState::AssignmentAggregateState(Data *data, unsigned generation)
    : data(data)
    , _generation(generation)   { }
bool AssignmentAggregateState::fact(bool recursive) const { return data->fact && !recursive; }
unsigned AssignmentAggregateState::generation() const     { return _generation; }
void AssignmentAggregateState::generation(unsigned x)     { _generation = x; }
bool AssignmentAggregateState::isFalse()                  { return data; }
AssignmentAggregateState::element_type &AssignmentAggregateState::ignore()   { throw std::logic_error("AssignmentAggregateState::ignore must not be called"); }
bool AssignmentAggregateState::defined() const            { return true; }

// }}}
// {{{ definition of AssignmentAggregate

AssignmentAggregate::AssignmentAggregate(Location const *&loc) : loc(loc) { }
void AssignmentAggregate::printPlain(std::ostream &out) const {
    out << *(repr->first.args().end()-1) << "=" << fun << "{";
    print_comma(out, repr->second.data->elems, ";", print_elem);
    out << "}";
}
ULit AssignmentAggregate::toLparse(unsigned &auxAtoms, StmHandler const &x) {
    // TODO: this algorithm suffers from severe uglyness!!!
    // TODO: check if false
    SAuxAtom auxAtom(std::make_shared<AuxAtom>(auxAtoms++));
    using ULitValVec = std::vector<std::pair<ULit,Value>>;
    ULitValVec elemVec;
    for (auto &y : repr->second.data->elems) { 
        Value weight(getWeight(fun, y.first));
        if (y.second->size() != 1 || y.second->front().size() != 1) {
            SAuxAtom atom(std::make_shared<AuxAtom>(auxAtoms++));
            elemVec.emplace_back(make_unique<AuxLiteral>(atom, false), weight);
            // atom :- cond
            if (!y.second->empty()) {
                std::vector<ULitVec> &conds(*y.second);
                for (auto &y : conds) {
                    LparseRule br(atom, get_clone(y));
                    br.toLparse(auxAtoms, x);
                }
            }
            else {
                LparseRule br(atom, {});
                br.toLparse(auxAtoms, x);
            }
        }
        else { elemVec.emplace_back(get_clone(y.second->front().front()), weight); }
    }
    switch (fun) {
        case AggregateFunction::COUNT:
        case AggregateFunction::SUMP:
        case AggregateFunction::SUM: {
            int  shift = 0;
            WeightRule::ULitBoundVec elemPos;
            for (auto &y : elemVec) {
                int weight = y.second.num();
                if (weight < 0) {
                    shift+= -weight;
                    SAuxAtom neg(std::make_shared<AuxAtom>(auxAtoms++));
                    ULitVec lits;
                    lits.emplace_back(get_clone(y.first));
                    LparseRule br(neg, std::move(lits));
                    br.toLparse(auxAtoms, x);
                    elemPos.emplace_back(make_unique<AuxLiteral>(neg, true), -weight);
                    if (incomplete && loc) {
                        GRINGO_REPORT(W_NONMONOTONE_AGGREGATE) 
                            << *loc << ": warning: negative weight in (potentially) recursive " << fun << " aggregate:\n"
                            << "  (the applied translation might produce counter-intuitive results)\n";
                        loc = nullptr;
                    }
                }
                else { elemPos.emplace_back(get_clone(y.first), weight); }
            }
            int assign((*(repr->first.args().end()-1)).num());
            ULitVec aggrLits;
            {
                // aggr :- L { }.
                SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                WeightRule wr(aggr, std::max(0, assign + shift), get_clone(elemPos));
                wr.toLparse(auxAtoms, x);
                aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, false));
            }
            {
                // aggr :- U+1 { }.
                SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                WeightRule wr(aggr, std::max(0, assign + 1 + shift), get_clone(elemPos));
                wr.toLparse(auxAtoms, x);
                aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, true));
            } 
            LparseRule br(auxAtom, std::move(aggrLits));
            br.toLparse(auxAtoms, x);
            break;
        }
        case AggregateFunction::MIN:
        case AggregateFunction::MAX: {
            Value assign(*(repr->first.args().end()-1));
            ULitVec aggrLits;
            {
                // aggr :- 1 { between }
                if (assign != (fun == AggregateFunction::MIN ? Value(false) : Value(true))) {
                    SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                    for (auto &z : elemVec) {
                        if (z.second == assign) {
                            ULitVec lits;
                            lits.emplace_back(get_clone(z.first));
                            LparseRule br(aggr, std::move(lits));
                            br.toLparse(auxAtoms, x);
                        }
                    }
                    aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, false));
                }
            }
            {
                SAuxAtom aggr(std::make_shared<AuxAtom>(auxAtoms++));
                for (auto &z : elemVec) {
                    if (fun == AggregateFunction::MIN ? z.second < assign : assign < z.second) {
                        // if min:  aggr :- 1 { below }
                        // eif max: aggr :- 1 { above }
                        ULitVec lits;
                        lits.emplace_back(get_clone(z.first));
                        LparseRule br(aggr, std::move(lits));
                        br.toLparse(auxAtoms, x);
                    }
                }
                aggrLits.emplace_back(make_unique<AuxLiteral>(aggr, true));
            } 
            LparseRule br(auxAtom, std::move(aggrLits));
            br.toLparse(auxAtoms, x);
            break;
        }
    }
    return std::make_unique<AuxLiteral>(auxAtom, false);
}
void AssignmentAggregate::makeEqual(ULit &&, unsigned &, StmHandler const &) const {
    throw std::runtime_error("AssignmentAggregate::makeEqual: must not happen!!!");
}
int AssignmentAggregate::lparseUid(LparseOutputter &) const {
    throw std::runtime_error("AssignmentAggregate::lparseUid must be called after AssignmentAggregate::toLparse");
}
bool AssignmentAggregate::isIncomplete() const { return incomplete; }
AssignmentAggregate *AssignmentAggregate::clone() const { return new AssignmentAggregate(*this); }
size_t AssignmentAggregate::hash() const { throw std::runtime_error("AssignmentAggregate::hash: implement me if necessary!"); }
bool AssignmentAggregate::operator==(Literal const &) const { throw std::runtime_error("AssignmentAggregate::operator==: implement me if necessary!"); }
AssignmentAggregate::~AssignmentAggregate() { }

// }}}
// {{{ definition of ConjunctionState

bool ConjunctionState::fact(bool recursive) const                 { return _fact && !recursive; } // TODO: likely not what I want!!!
unsigned ConjunctionState::generation() const                     { return _generation - 2; }
void ConjunctionState::generation(unsigned x)                     { _generation = x + 2; }
bool ConjunctionState::isFalse()                                  { throw std::logic_error("ConjunctionState::isFalse must not be called"); }
ConjunctionState::element_type &ConjunctionState::ignore() { throw std::logic_error("ConjunctionState::ignore must not be called"); }
bool ConjunctionState::defined() const                            { return _generation > 1; }

// }}}
// {{{ definition of Conjunction

void Conjunction::printElem(std::ostream &out, ConjunctionState::Elem const &x) {
    if (x.first) { out << x.first->first; }
    else         { out << "#false"; }
    if (!x.second.empty()) {
        out << ":";
        using namespace std::placeholders;
        print_comma(out, x.second, ",", std::bind(&Literal::printPlain, _2, _1));
    }
}
void Conjunction::printPlain(std::ostream &out) const {
    if (!repr->second.elems.empty()) {
        print_comma(out, repr->second.elems, ";", &Conjunction::printElem);
    }
    else { out << "#true"; }
}
ULit Conjunction::toLparse(unsigned &auxAtoms, StmHandler const &x) {
    // TODO: at some point I should try to prove this translation
    SAuxAtom bdLit(std::make_shared<AuxAtom>(auxAtoms++));
    ULitVec bd;
    for (ConjunctionState::Elem &y : repr->second.elems) {
        if (!y.second.empty()) {
            SAuxAtom aux(std::make_shared<AuxAtom>(auxAtoms++));
            if (y.first && y.first->second.defined()) {
                // aux :- x.first.
                ULitVec lits;
                lits.emplace_back(make_unique<PredicateLiteral>(NAF::POS, *y.first));
                LparseRule(aux, std::move(lits)).toLparse(auxAtoms, x);
            }
            SAuxAtom chk(std::make_shared<AuxAtom>(auxAtoms++));
            // chk :- y.second.
            ULitVec lits;
            for (auto &z : y.second) { lits.emplace_back(get_clone(z)); }
            LparseRule(chk, std::move(lits)).toLparse(auxAtoms, x);
            // aux :- ~chk.
            lits.emplace_back(make_unique<AuxLiteral>(chk, true));
            LparseRule(aux, std::move(lits)).toLparse(auxAtoms, x);
            if (incomplete && y.first && y.first->second.defined()) {
                // aux | chk :- ~~x.first.
                SAuxAtomVec head;
                head.emplace_back(aux);
                head.emplace_back(chk);
                lits.emplace_back(make_unique<PredicateLiteral>(NAF::NOTNOT, *y.first));
                LparseRule(std::move(head), std::move(lits)).toLparse(auxAtoms, x);
                // y.second :- chk.
                for (auto &z : y.second) { z->makeEqual(make_unique<AuxLiteral>(chk, false), auxAtoms, x); }
            }
            // body += aux
            bd.emplace_back(make_unique<AuxLiteral>(aux, false));
        }
        // body += x.first
        else if (y.first) { bd.emplace_back(make_unique<PredicateLiteral>(NAF::POS, *y.first)); }
        else              { return make_unique<AuxLiteral>(bdLit, false); }
    }
    // bdLit :- body.
    LparseRule(bdLit, std::move(bd)).toLparse(auxAtoms, x);
    return make_unique<AuxLiteral>(bdLit, false);
}
void Conjunction::makeEqual(ULit &&, unsigned &, StmHandler const &) const {
    throw std::runtime_error("AssignmentAggregate::makeEqual: must not happen!!!");
}
int Conjunction::lparseUid(LparseOutputter &) const { throw std::logic_error("Conjunction::toLparse: must be called before Conjunction::lparseUid"); }
bool Conjunction::isIncomplete() const              { return incomplete; }
Conjunction *Conjunction::clone() const             { return new Conjunction(*this); }
size_t Conjunction::hash() const                    { throw std::runtime_error("Conjunction::hash: implement me if necessary!"); }
bool Conjunction::operator==(Literal const &) const { throw std::runtime_error("Conjunction::operator==: implement me if necessary!"); }
Conjunction::~Conjunction()                         { }

// }}}

} } // namespace Output Gringo
