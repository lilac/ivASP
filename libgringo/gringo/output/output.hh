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

#ifndef _GRINGO_OUTPUT_OUTPUT_HH
#define _GRINGO_OUTPUT_OUTPUT_HH

#include <gringo/output/statement.hh>

namespace Gringo { namespace Output {

struct Minimize : Statement {
    using MinimizeList = std::vector<std::pair<FWValVec, ULitVec>>;

    virtual void toLparse(unsigned &auxAtoms, StmHandler const &x);
    virtual void printPlain(std::ostream &out) const;
    virtual void printLparse(LparseOutputter &out) const;
    virtual bool isIncomplete() const;
    virtual Minimize *clone() const;
    virtual ~Minimize();

    MinimizeList elems;
};

struct PlainLparseOutputter : LparseOutputter {
    PlainLparseOutputter(std::ostream &out);
    virtual void printBasicRule(unsigned head, LitVec const &body);
    virtual void printChoiceRule(AtomVec const &head, LitVec const &body);
    virtual void printCardinalityRule(unsigned head, unsigned lower, LitVec const &body);
    virtual void printWeightRule(unsigned head, unsigned lower, LitWeightVec const &body);
    virtual void printMinimize(LitWeightVec const &body);
    virtual void printDisjunctiveRule(AtomVec const &head, LitVec const &body);
    virtual unsigned falseUid();
    virtual unsigned newUid();
    virtual void finishRules();
    virtual void printSymbol(unsigned atomUid, Value v);
    virtual void finishSymbols();
    virtual ~PlainLparseOutputter();

    std::ostream &out;
    unsigned      uids = 2;
};

struct OutputBase {
    OutputBase(StmHandler handler);
    OutputBase(std::ostream &out, bool lparse = false);
    OutputBase(LparseOutputter &out);
    void output(Value const &val);
    void output(UStm &&x);
    void output(Statement &x);
    void flush();
    void finish();
    void checkOutPreds();

    ValVec            tempVals;
    LitVec            tempLits;
    RuleRef           tempRule;   // Note: performance
    PredDomMap        domains;
    UStmVec           stms;
    Minimize          minimize;
    StmHandler        handler;
    OutputPredicates  outPreds;
};

} } // namespace Output Gringo

#endif // _GRINGO_OUTPUT_OUTPUT_HH

