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

#include "gringo/output/output.hh"
#include "gringo/logger.hh"

namespace Gringo { namespace Output {

// {{{ definition of Minimize

void Minimize::toLparse(unsigned &auxAtoms, StmHandler const &x) {
    for (auto &y : elems) {
        for (auto &z : y.second) { Term::replace(z, z->toLparse(auxAtoms, x)); }
    }
    x(*this);
}
void Minimize::printPlain(std::ostream &out) const {
    for (auto &x : elems) {
        out << ":~";
        using namespace std::placeholders;
        print_comma(out, x.second, ";", std::bind(&Literal::printPlain, _2, _1));
        out << ".[";
        auto it(x.first.begin());
        out << *it++ << "@";
        out << *it++;
        for (auto ie(x.first.end()); it != ie; ++it) { out << "," << *it; }
        out << "]\n";
    }
}
void Minimize::printLparse(LparseOutputter &out) const {
    using GroupByPriority = std::map<Value, std::unique_list<FWValVec, std::vector<LparseOutputter::LitVec>>>;
    GroupByPriority groupBy;
    for (auto &y : elems) {
        LparseOutputter::LitVec cond;
        for (auto &z : y.second) { cond.emplace_back(z->lparseUid(out)); }
        auto &ret(groupBy.insert(GroupByPriority::value_type(*(y.first.begin() + 1), GroupByPriority::mapped_type())).first->second);
        ret.emplace_back(
            std::piecewise_construct, 
            std::forward_as_tuple(y.first), 
            std::forward_as_tuple()).first->second->emplace_back(std::move(cond));

    }
    for (auto &y : groupBy) {
        LparseOutputter::LitWeightVec lits;
        for (auto &z : y.second) {
            int weight((*z.first.begin()).num());
            if (z.second->size() == 1) {
                int lit;
                if (z.second->front().size() == 1) {
                    lit = z.second->front().front();
                }
                else {
                    lit = out.newUid();
                    out.printBasicRule(lit, z.second->front());
                }
                lits.emplace_back(weight < 0 ? -lit : lit, std::abs(weight));
            }
            else {
                throw std::logic_error("Statement::printLparse: implement me!!!");
            }

        }
        out.printMinimize(lits);
    }
}
bool Minimize::isIncomplete() const { throw std::logic_error("Minimize::isIncomplete must not be called."); }
Minimize *Minimize::clone() const { throw std::logic_error("Minimize::clone must not be called."); }
Minimize::~Minimize() { }

// }}}
// {{{ definition of PlainLparseOutputter

PlainLparseOutputter::PlainLparseOutputter(std::ostream &out) : out(out) { }
void PlainLparseOutputter::printBasicRule(unsigned head, LitVec const &body) {
    out << "1 " << head << " " << body.size();
    unsigned neg(0);
    for (auto &x : body) { neg+= x < 0; }
    out << " " << neg;
    for (auto &x : body) { if (x < 0) { out << " " << -x; } }
    for (auto &x : body) { if (x > 0) { out << " " << +x; } }
    out << "\n";
}
void PlainLparseOutputter::printChoiceRule(AtomVec const &head, LitVec const &body) {
    out << "3 " << head.size();
    for (auto &x : head) { out << " " << x; }
    out << " " << body.size();
    unsigned neg(0);
    for (auto &x : body) { neg+= x < 0; }
    out << " " << neg;
    for (auto &x : body) { if (x < 0) { out << " " << -x; } }
    for (auto &x : body) { if (x > 0) { out << " " << +x; } }
    out << "\n";
}
void PlainLparseOutputter::printCardinalityRule(unsigned head, unsigned lower, LitVec const &body) {
    out << "2 " << head << " " << body.size();
    unsigned neg(0);
    for (auto &x : body) { neg+= x < 0; }
    out << " " << neg << " " << lower;
    for (auto &x : body) { if (x < 0) { out << " " << -x; } }
    for (auto &x : body) { if (x > 0) { out << " " << +x; } }
    out << "\n";
}
void PlainLparseOutputter::printWeightRule(unsigned head, unsigned lower, LitWeightVec const &body) {
    out << "5 " << head << " " << lower << " " << body.size();
    unsigned neg(0);
    for (auto &x : body) { neg+= x.first < 0; }
    out << " " << neg;
    for (auto &x : body) { if (x.first < 0) { out << " " << -x.first; } }
    for (auto &x : body) { if (x.first > 0) { out << " " << +x.first; } }
    for (auto &x : body) { if (x.first < 0) { out << " " << x.second; } }
    for (auto &x : body) { if (x.first > 0) { out << " " << x.second; } }
    out << "\n";
}
void PlainLparseOutputter::printMinimize(LitWeightVec const &body) {
    out << "6 0 " << body.size();
    unsigned neg(0);
    for (auto &x : body) { neg+= x.first < 0; }
    out << " " << neg;
    for (auto &x : body) { if (x.first < 0) { out << " " << -x.first; } }
    for (auto &x : body) { if (x.first > 0) { out << " " << +x.first; } }
    for (auto &x : body) { if (x.first < 0) { out << " " << x.second; } }
    for (auto &x : body) { if (x.first > 0) { out << " " << x.second; } }
    out << "\n";
}
void PlainLparseOutputter::printDisjunctiveRule(AtomVec const &head, LitVec const &body) {
    out << "8 " << head.size();
    for (auto &x : head) { out << " " << x; }
    out << " " << body.size();
    unsigned neg(0);
    for (auto &x : body) { neg+= x < 0; }
    out << " " << neg;
    for (auto &x : body) { if (x < 0) { out << " " << -x; } }
    for (auto &x : body) { if (x > 0) { out << " " << +x; } }
    out << "\n";
}
unsigned PlainLparseOutputter::falseUid()                                   { return 1; }
unsigned PlainLparseOutputter::newUid()                                     { return uids++; }
void PlainLparseOutputter::finishRules()                                    { out << "0\n"; }
void PlainLparseOutputter::printSymbol(unsigned atomUid, Value v)           { out << atomUid << " " << v << "\n"; }
void PlainLparseOutputter::finishSymbols()                                  { out << "0\nB+\n0\nB-\n" << falseUid() << "\n0\n1\n"; }
PlainLparseOutputter::~PlainLparseOutputter()                               { }

// }}}
// {{{ definition of OutputBase

OutputBase::OutputBase(StmHandler handler) : handler(handler) { }
OutputBase::OutputBase(std::ostream &out, bool lparse) {
    if (lparse) {
        std::shared_ptr<unsigned> auxAtoms{std::make_shared<unsigned>(0)};
        handler = [&out, auxAtoms](Statement &x) { 
            x.toLparse(*auxAtoms, [&out](Statement &x) { x.printPlain(out); });
        };
    }
    else { handler = [&out](Statement &x) { x.printPlain(out); }; }
}

OutputBase::OutputBase(LparseOutputter &out) { setOutputter(out); }

void OutputBase::setOutputter(LparseOutputter &out) {
    std::shared_ptr<unsigned> auxAtoms{std::make_shared<unsigned>(0)};
    handler = [&out, auxAtoms](Statement &x) { 
        x.toLparse(*auxAtoms, [&out](Statement &x) { x.printLparse(out); });
    };
}
void OutputBase::output(Value const &val) {
    auto it(domains.find(val.sig()));
    assert(it != domains.end());
    auto ret(it->second->insert(val, true));
    if (!std::get<2>(ret) || !std::get<1>(ret)) {
        tempRule.head = std::get<0>(ret);
        tempRule.body.clear();
        handler(tempRule);
    }
}
void OutputBase::output(UStm &&x) {
    if (!x->isIncomplete()) { handler(*x); }
    else { stms.emplace_back(std::move(x)); }
}
void OutputBase::output(Statement &x) {
    if (!x.isIncomplete()) { handler(x); }
    else { stms.emplace_back(x.clone()); }
}
void OutputBase::flush() {
    for (auto &x : stms) { handler(*x); }
    stms.clear();
}
void OutputBase::finish() {
    handler(minimize);
}
void OutputBase::checkOutPreds() {
    std::sort(outPreds.begin(), outPreds.end(), [](OutputPredicates::value_type const &x, OutputPredicates::value_type const &y){ return x.second < y.second; });
    outPreds.erase(std::unique(outPreds.begin(), outPreds.end(), [](OutputPredicates::value_type const &x, OutputPredicates::value_type const &y){ return x.second == y.second; }), outPreds.end());
    for (auto &x : outPreds) {
        if (x.second != Signature("", 0)) {
            auto it(domains.find(x.second));
            if (it == domains.end()) {
                GRINGO_REPORT(W_ATOM_UNDEFINED) 
                    << x.first << ": warning: no matching occurrence for signature:\n"
                    << "  " << x.second << "\n";
            }
        }
    }
}

// }}}

} } // namespace Output Gringo
