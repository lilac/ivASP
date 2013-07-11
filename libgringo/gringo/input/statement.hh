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

#ifndef _GRINGO_INPUT_STATEMENT_HH
#define _GRINGO_INPUT_STATEMENT_HH

#include <gringo/input/literal.hh>
#include <gringo/input/aggregate.hh>
#include <gringo/output/output.hh>
#include <gringo/ground/statement.hh>

namespace Gringo { namespace Input {

// {{{ declaration of Statement

struct Statement;
struct StatementVisitor;
struct StatementConstVisitor;

typedef std::unique_ptr<Statement> UStm;
typedef std::vector<UStm> UStmVec;

struct Statement : Printable, Locatable {
    typedef std::vector<std::pair<ULit, UBodyAggrVec>> SplitVec;

    virtual Value isEDB() const;
    virtual void rewrite(Literal::ProjectionMap &project, SplitVec &splits) = 0;
    virtual void unpool(UStmVec &x) = 0;
    virtual bool hasPool() const = 0;
    virtual bool check() const = 0;
    virtual void replace(Defines &dx) = 0;
    virtual void toGround(ToGroundArg &x, Ground::UStmVec &stms) const = 0;
    virtual ~Statement() { }
};

// }}}

// {{{ declaration of Rule

struct Rule : Statement {
    Rule(UHeadAggr &&head, UBodyAggrVec &&body);
    virtual void unpool(UStmVec &x);
    virtual void rewrite(Literal::ProjectionMap &project, SplitVec &splits);
    virtual Value isEDB() const;
    virtual void print(std::ostream &out) const;
    virtual bool hasPool() const;
    virtual bool check() const;
    virtual void replace(Defines &dx);
    virtual void toGround(ToGroundArg &x, Ground::UStmVec &stms) const;
    virtual ~Rule();

    UHeadAggr head;
    UBodyAggrVec body;
};

// }}}
// {{{ declaration of WeakConstraint

struct WeakConstraint : Statement {
    WeakConstraint(UTerm &&weight, UTerm &&prioriy, UTermVec &&tuple, UBodyAggrVec &&body);
    WeakConstraint(UTermVec &&tuple, UBodyAggrVec &&body);
    virtual void unpool(UStmVec &x);
    virtual void rewrite(Literal::ProjectionMap &project, SplitVec &splits);
    virtual void print(std::ostream &out) const;
    virtual bool hasPool() const;
    virtual bool check() const;
    virtual void replace(Defines &dx);
    virtual void toGround(ToGroundArg &x, Ground::UStmVec &stms) const;
    virtual ~WeakConstraint();

    UTermVec     tuple;
    UBodyAggrVec body;
};

// }}}

} } // namespace Input Gringo

#endif // _GRINGO_INPUT_STATEMENT_HH
