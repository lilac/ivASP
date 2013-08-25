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

#ifndef _GRINGO_INPUT_PROGRAM_HH
#define _GRINGO_INPUT_PROGRAM_HH

#include <gringo/input/statement.hh>
#include <gringo/ground/program.hh>
#include <gringo/unique_list.hh>

namespace Gringo { namespace Input {

// {{{ declaration of Program

class Program {
public:
    struct Empty {};
    using ClassicalNegationList = unique_list<FWSignature, Empty>;

    Program();
    Program(Program &&x);
    void add(UStm &&stm);
    void addClassicalNegation(FWSignature x);
    void rewrite(Defines &defs);
    bool check();
    void print(std::ostream &out) const;
    Ground::Program toGround(PredDomMap &domains);
    ~Program();

    FWString  incr;
    std::unique_ptr<VarTerm> incrVar;

private:
    void rewriteDots();
    void rewriteArithmetics();
    void unpool();


    ValVec                edb_;
    UStmVec               stms_;
    ClassicalNegationList neg_;
};

std::ostream &operator<<(std::ostream &out, Program const &p);

// }}}

} } // namespace Input Gringo

#endif //_GRINGO_INPUT_PROGRAM_HH
