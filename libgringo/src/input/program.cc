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

#include "gringo/input/program.hh"
#include "gringo/ground/literal.hh"
#include "gringo/term.hh"
#include "gringo/logger.hh"
#include "gringo/graph.hh"
#include "gringo/safetycheck.hh"

namespace Gringo { namespace Input {

// {{{ definition of Program

Program::Program() = default;

Program::Program(Program &&) = default;

void Program::add(UStm &&stm) {
    edb_.emplace_back(stm->isEDB());
    if (edb_.back().type() == Value::SPECIAL) {
        stms_.emplace_back(std::move(stm));
        edb_.pop_back();
    }
}

void Program::rewrite(Defines &defs) {
    defs.init();
    if (!defs.empty()) {
        auto it = edb_.begin();
        for (auto jt = it, je = edb_.end(); jt != je; ++jt) {
            UTerm rt;
            Value rv;
            defs.apply(*jt, rv, rt);
            if (rt) {
                Location loc{rt->loc()};
                stms_.emplace_back(make_locatable<Rule>(loc, make_unique<SimpleHeadLiteral>(make_locatable<PredicateLiteral>(loc, NAF::POS, std::move(rt))), UBodyAggrVec{}));
            }
            else {
                if (rv.type() != Value::SPECIAL) { *it = rv;  }
                else if (it != jt)               { *it = *jt; }
                ++it;
            }
        }
        edb_.erase(it, edb_.end());
    }
    UStmVec stms;
    Literal::ProjectionMap project;
    Statement::SplitVec splits;
    for (auto &x : stms_) {
        x->replace(defs);
        if (x->hasPool()) {
            UStmVec pool;
            x->unpool(pool);
            for (auto &y : pool) {
                y->rewrite(project, splits);
                edb_.emplace_back(y->isEDB());
                if (edb_.back().type() == Value::SPECIAL) {
                    stms.emplace_back(std::move(y));
                    edb_.pop_back();
                }
            }
        }
        else {
            x->rewrite(project, splits);
            edb_.emplace_back(x->isEDB());
            if (edb_.back().type() == Value::SPECIAL) {
                stms.emplace_back(std::move(x));
                edb_.pop_back();
            }
        }
    }
    for (auto &x : std::get<1>(project)) {
        Location loc(x.first->loc());
        UBodyAggrVec body;
        body.emplace_back(make_locatable<SimpleBodyLiteral>(loc, make_locatable<PredicateLiteral>(loc, NAF::POS, std::move(x.second))));
        stms.emplace_back(make_locatable<Rule>(
            loc,
            make_unique<SimpleHeadLiteral>(make_locatable<PredicateLiteral>(loc, NAF::POS, std::move(x.first))),
            std::move(body)));
    }
    for (auto &x : splits) {
        Location loc(x.first->loc());
        stms.emplace_back(make_locatable<Rule>(
            loc,
            make_unique<SimpleHeadLiteral>(std::move(x.first)),
            std::move(x.second)));
    }
    stms_ = std::move(stms);
}

bool Program::check() {
    bool ret = true;
    for (auto &stm : stms_) { ret = stm->check() && ret; }
    return ret;
}

void Program::addClassicalNegation(FWSignature x) {
    neg_.emplace_back(x, Empty());
}

void Program::print(std::ostream &out) const {
    for (auto &x : edb_)  { out << x << "." << "\n"; }
    for (auto &x : stms_) { out << *x << "\n"; }
}

Ground::Program Program::toGround(PredDomMap &domains) {
    Ground::UStmVec stms;
    ToGroundArg arg(domains);
    for (auto &x : stms_) { 
        x->toGround(arg, stms);
        x = nullptr;
    }
    stms_ = UStmVec();
    Ground::Statement::Dep dep;
    for (auto &x : stms) { 
        bool normal(x->isNormal());
        auto &node(dep.add(std::move(x), normal));
        node.stm->analyze(node, dep);
    }
    Ground::LocSet locs;
    Ground::SigSet sigs;
    for (auto &x : edb_) { 
        if (sigs.emplace(x.sig()).second && domains.find(x.sig()) == domains.end()) { 
            domains.emplace_back(std::piecewise_construct, std::forward_as_tuple(x.sig()), std::forward_as_tuple());
        }
    }
    Ground::Program prg(std::move(edb_), dep.analyze(), std::move(incrVar));
    // TODO: an iterator for this little monstrosity would be nice
    for (auto &x : dep.depend.occs) {
        for (auto &y : x.second.first->depend) { (*std::get<0>(y)).checkDefined(locs, sigs); }
    }
    for (auto &x : neg_) { prg.negate.emplace_back(Gringo::add(domains, x.first), Gringo::add(domains, Signature("-" + *(*x.first).name(), (*x.first).length())), 0); }
    return prg;
}

Program::~Program() { }

std::ostream &operator<<(std::ostream &out, Program const &p) {
    p.print(out);
    return out;
}

// }}}

} } // namespace Input Gringo
