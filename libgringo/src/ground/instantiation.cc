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

#include <gringo/ground/instantiation.hh>
#include <gringo/output/output.hh>

namespace Gringo { namespace Ground {

// {{{ definition of SolutionBinder

IndexUpdater *SolutionBinder::getUpdater()   { return nullptr; }
void SolutionBinder::match()                 { }
bool SolutionBinder::next()                  { return false; }
void SolutionBinder::print(std::ostream &out) const { out << "#end"; }
SolutionBinder::~SolutionBinder()             { }

// }}}
// {{{ definition of BackjumpBinder

BackjumpBinder::BackjumpBinder(UIdx &&index, DependVec &&depends)
    : index(std::move(index))  
    , depends(std::move(depends)) { }
BackjumpBinder::BackjumpBinder(BackjumpBinder &&) noexcept = default;
void BackjumpBinder::match() { index->match(); }
bool BackjumpBinder::next()  { return index->next(); }
bool BackjumpBinder::first() {
    index->match();
    return next();
}
void BackjumpBinder::print(std::ostream &out) const { 
    out << *index;
    out << ":[";
    print_comma(out, depends, ",");
    out << "]";
}
BackjumpBinder::~BackjumpBinder() { }

// }}}
// {{{ definition of Instantiator

Instantiator::Instantiator(SolutionCallback &callback)
    : callback(callback) { }
Instantiator::Instantiator(Instantiator &&) noexcept = default;
void Instantiator::add(UIdx &&index, DependVec &&depends) {
    binders.emplace_back(std::move(index), std::move(depends));
}
void Instantiator::finalize(DependVec &&depends) {
    binders.emplace_back(std::make_unique<SolutionBinder>(), std::move(depends));
}
void Instantiator::enqueue(Queue &queue) {
    if (!enqueued) {
        enqueued = true;
        queue.enqueue(*this);
    }
}
void Instantiator::instantiate(Output::OutputBase &out) {
//    std::cerr << "  instantiate: ";
//    print(std::cerr);
//    std::cerr << std::endl;
    enqueued = false;
    auto ie = binders.rend(), it = ie - 1, ib = binders.rbegin();
    it->match();
    do {
//        std::cerr << "    start at: ";
//        it->print(std::cerr);
//        std::cerr << std::endl;
        it->backjumpable = true;
        if (it->next()) {
            for (--it; it->first(); --it) { it->backjumpable = true; }
        }
//        std::cerr << "    advanced to: ";
//        it->print(std::cerr);
//        std::cerr << std::endl;
        if (it == ib) { callback.report(out); }
        for (auto &x : it->depends) { binders[x].backjumpable = false; }
        for (++it; it != ie && it->backjumpable; ++it) { }
//        std::cerr << "    backfumped to: ";
//        if (it != ie) { it->print(std::cerr); }
//        else          { std::cerr << "the head :)"; }
//        std::cerr << std::endl;
    } 
    while (it != ie);
}
void Instantiator::print(std::ostream &out) const {
    using namespace std::placeholders;
    // Note: consider adding something to callback
    callback.printHead(out);
    out << " <~ ";
    print_comma(out, binders.begin(), binders.end(), std::bind(&BackjumpBinder::print, _2, _1), " , ");
    out << ".";
}
Instantiator::~Instantiator() { }

// }}}
// {{{ definition of Queue

void Queue::process(Output::OutputBase &out) {
    while (!queue.empty()) {
//        std::cerr << "************start step" << std::endl;
        queue.swap(current);
        for (Instantiator &x : current) {
            x.instantiate(out);
            x.callback.mark();
        }
        for (Instantiator &x : current) { x.callback.unmark(*this); }
        current.clear();
        auto jt(domains.begin());
        for (auto it(jt), ie(domains.end()); it != ie; ++it) {
            if (it->get().expire()) { *jt++ = *it; }
            else                    { it->get().setEnqueued(false); }
        }
        domains.erase(jt, domains.end());
    }
    for (Domain &x : domains) { 
        x.expire();
        x.setEnqueued(false);
    }
    domains.clear();
}
void Queue::enqueue(Instantiator &inst) { queue.emplace_back(inst); }
void Queue::enqueue(Domain &x) {
    if (!x.isEnqueued()) { 
        x.setEnqueued(true);
        domains.emplace_back(x);
    }
}
Queue::~Queue() { }

// }}}

} } // namespace Ground Gringo
