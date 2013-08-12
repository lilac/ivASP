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

#include "gringo/ground/program.hh"
#include "gringo/output/output.hh"

namespace Gringo { namespace Ground {

// {{{ definition of Program

Program::Program(ValVec &&edb, Statement::Dep::ComponentVec &&stms)
    : edb(std::move(edb))
    , stms(std::move(stms)) { }

std::ostream &operator<<(std::ostream &out, Program const &p) {
    bool comma = false;
    for (auto &component : p.stms) {
        if (comma) { out << "\n"; }
        else       { comma = true; }
        out << "%" << (component.second ? " positive" : "") <<  " component";
        for (auto &stm : component.first) { out << "\n" << *stm; }
    }
    return out;
}

void Program::linearize(Output::OutputBase &out) {
    out.checkOutPreds();
    for (auto &x : edb) { out.output(x); }
    for (auto &x : out.domains) {
        x.second->mark();
        x.second->unmark();
        x.second->expire();
    }
    for (auto &x : stms) {
        for (auto &y : x.first) { y->startLinearize(true); }
        for (auto &y : x.first) { y->linearize(x.second); }
        for (auto &y : x.first) { y->startLinearize(false); }
    }
    linearized = true;
}

void Program::ground(Output::OutputBase &out) {
    Queue q;
    for (auto &x : stms) {
        // std::cerr << "============= component ===========" << std::endl;
        for (auto &y : x.first) { 
            // std::cerr << "  enqueue: " << *y << std::endl;
            y->enqueue(q);
        }
        q.process(out);
    }
    Output::PredicateLiteral pPos, pNeg;
    for (auto &x : negate) {
        for (auto it(std::get<1>(x).exports.begin() + std::get<2>(x)), ie(std::get<1>(x).exports.end()); it != ie; ++it) {
            PredicateDomain::element_type &neg(*it);
            Value v = neg.first.type() == Value::ID
                ? Value((*neg.first.string()).substr(1))
                : Value((*neg.first.name()).substr(1), neg.first.args());
            auto pos(std::get<0>(x).domain.find(v));
            if (pos != std::get<0>(x).domain.end() && pos->second.defined()) {
                pPos.repr = &*pos;
                pNeg.repr = &neg;
                pPos.naf  = pNeg.naf = NAF::POS;
                out.tempRule.head = nullptr;
                out.tempRule.body.clear();
                out.tempRule.body.emplace_back(pPos);
                out.tempRule.body.emplace_back(pNeg);
                out.output(out.tempRule);
            }
        }
        std::get<2>(x) = std::get<1>(x).exports.size();
    }
    out.flush();
    out.finish();
}

// }}}

} } // namespace Ground Gringo
