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

#ifndef _GRINGO_OUTPUT_INTERFACES_HH
#define _GRINGO_OUTPUT_INTERFACES_HH

#include <gringo/value.hh>
#include <gringo/clonable.hh>
#include <gringo/comparable.hh>
#include <gringo/hashable.hh>
#include <gringo/domain.hh>

namespace Gringo { namespace Output {

// {{{ declaration of LparseOutputter

using OutputPredicates = std::vector<std::pair<Location, Signature>>;

struct LparseOutputter {
    using AtomVec          = std::vector<unsigned>;
    using LitVec           = std::vector<int>;
    using LitWeightVec     = std::vector<std::pair<int, unsigned>>;

    virtual unsigned falseUid() = 0;
    virtual unsigned newUid() = 0;
    virtual void printBasicRule(unsigned head, LitVec const &body) = 0;
    virtual void printChoiceRule(AtomVec const &atoms, LitVec const &body) = 0;
    virtual void printCardinalityRule(unsigned head, unsigned lower, LitVec const &body) = 0;
    virtual void printWeightRule(unsigned head, unsigned lower, LitWeightVec const &body) = 0;
    virtual void printMinimize(LitWeightVec const &body) = 0;
    virtual void printDisjunctiveRule(AtomVec const &atoms, LitVec const &body) = 0;
    virtual void finishRules() = 0;
    virtual void printSymbol(unsigned atomUid, Value v, bool lr = false) = 0;
    virtual void finishSymbols() = 0;
    void finish(PredDomMap &domains, OutputPredicates &outPreds) {
        finishRules();
        if (!outPreds.empty()) { 
            for (auto &x : outPreds) {
                auto it(domains.find(x.second));
                if (it != domains.end()) {
                    for (auto &y : it->second->exports) {
                        if (!y.get().second.hasUid()) { y.get().second.uid(newUid()); }
                        printSymbol(y.get().second.uid(), y.get().first);
                    }
                }
            }
        }
        else {
            for (auto &x : domains) {
                std::string const &name(*(*x.first).name());
                if (name.front() != '#' && outPreds.empty()) {
                    for (auto &y : x.second->exports) {
                        if (!y.get().second.hasUid()) { y.get().second.uid(newUid()); }
                        printSymbol(y.get().second.uid(), y.get().first);
                    }
                }
            }
        }
        auto it(domains.find(Signature("#show", 1)));
        if (it != domains.end()) {
            for (auto &y : it->second->exports) {
                if (!y.get().second.hasUid()) { y.get().second.uid(newUid()); }
                printSymbol(y.get().second.uid(), *y.get().first.args().begin());
            }
        }
        finishSymbols();
    }
    virtual ~LparseOutputter() { }
};

// }}}
// {{{ declaration of Statement

struct Statement;
using UStm       = std::unique_ptr<Statement>;
using UStmVec    = std::vector<UStm>;
using StmHandler = std::function<void (Statement &)>;

struct Statement : Clonable<Statement> {
    virtual void toLparse(unsigned &auxAtoms, StmHandler const &x) = 0;
    virtual void printPlain(std::ostream &out) const = 0;
    virtual void printLparse(LparseOutputter &out) const = 0;
    virtual bool isIncomplete() const = 0;
    virtual ~Statement() { }
};

// }}}
// {{{ declaration of Literal

struct Literal;
using ULit = std::unique_ptr<Literal>;
struct Literal : Clonable<Literal>, Hashable, Comparable<Literal> {
    virtual ULit toLparse(unsigned &auxAtoms, StmHandler const &x) = 0;
    virtual void makeEqual(ULit &&lit, unsigned &auxAtoms, StmHandler const &x) const = 0;
    virtual void printPlain(std::ostream &out) const = 0;
    virtual bool isIncomplete() const = 0;
    virtual int lparseUid(LparseOutputter &out) const = 0;
    virtual ~Literal() { }
};
using LitVec  = std::vector<std::reference_wrapper<Literal>>;
using ULitVec = std::vector<ULit>;

// }}}

} } // namespace Output Gringo

GRINGO_HASH(Gringo::Output::Literal)

#endif // _GRINGO_OUTPUT_INTERFACES_HH
