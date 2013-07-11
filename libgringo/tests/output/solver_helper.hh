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

#ifndef _GRINGO_OUTPUT_TEST_SOLVER_HELPER_HH
#define _GRINGO_OUTPUT_TEST_SOLVER_HELPER_HH

#include "gringo/logger.hh"
#include "gringo/ground/dependency.hh"
#include "gringo/input/nongroundparser.hh"
#include "gringo/input/program.hh"
#include "gringo/output/output.hh"

#include <clasp/reader.h>
#include <clasp/program_builder.h>
#include <clasp/solver.h>
#include <clasp/minimize_constraint.h>

#include <clasp/model_enumerators.h>
#include <clasp/solve_algorithms.h>

namespace Gringo { namespace Output { namespace Test {

// {{{ definition of AspOutPrinter

using Model  = std::vector<std::string>;
using Models = std::vector<Model>;

struct AspOutPrinter : public Clasp::Enumerator::Report {
    AspOutPrinter(std::initializer_list<std::string> filter) : filter(filter) { }
	virtual void reportModel(const Clasp::Solver& s, const Clasp::Enumerator&) {
        models.emplace_back();
        const Clasp::SymbolTable& symTab = s.sharedContext()->symTab();
        for (Clasp::SymbolTable::const_iterator it = symTab.begin(); it != symTab.end(); ++it) {
            if (s.isTrue(it->second.lit) && !it->second.name.empty()) {
                std::string atom(it->second.name.c_str());
                for (auto &x : filter) {
                    if (!atom.compare(0, x.size(), x)) { 
                        models.back().emplace_back(std::move(atom));
                        break;
                    }
                }
            }
        }
        std::sort(models.back().begin(), models.back().end());
    }
    virtual ~AspOutPrinter() { }

    Models models;
    std::initializer_list<std::string> filter;
};

// }}}
// {{{ definition of solve

inline Models solve(std::string &&str, std::initializer_list<std::string> filter = {""}, std::initializer_list<Clasp::wsum_t> minimize = {}) {
    // grounder: setup
    std::stringstream ss;
    PlainLparseOutputter plo(ss);
    OutputBase out(plo);
    Input::Program prg;
    Defines defs;
    Input::NongroundProgramBuilder pb(prg, out, defs);
    Input::NonGroundParser parser(pb);
    parser.pushStream("-", make_unique<std::stringstream>(std::move(str)));
    // grounder: parse
    parser.parse();
    // grounder: preprocess
    prg.rewrite(defs);
    prg.check();
    if (!message_printer()->hasError()) {
        Ground::Program gPrg(prg.toGround(out.domains));
        // grounder: ground
        gPrg.ground(out);
        plo.finish(out.domains, out.outPreds);
    }
    // solver: setup
    Clasp::SharedContext ctx;
	Clasp::ProgramBuilder api;
	Clasp::SolveParams params;
    AspOutPrinter printer(filter);
    api.startProgram(ctx, Clasp::ProgramBuilder::EqOptions().ext(Clasp::ProgramBuilder::mode_transform_weight));
    // solver: parse
    if (Clasp::parseLparse(ss, api) && api.endProgram()) {
        // solver: create minimize constraint
        Clasp::SharedMinimizeData *nm = nullptr;
		if (minimize.size()) {
            Clasp::MinimizeBuilder mb;
            api.addMinimize(mb);
            unsigned i(0);
            for (auto &x : minimize) { mb.setOptimum(i++, x); }
	    	nm = mb.build(ctx);
            nm->setMode(Clasp::MinimizeMode_t::enumerate, 0);
        }
        // solver: enumerate
        ctx.addEnumerator(new Clasp::BacktrackEnumerator(0, &printer));
		ctx.enumerator()->setMinimize(nm);
        ctx.enumerator()->enumerate(0);
        ctx.endInit();
        Clasp::solve(ctx, params);
    }
    std::sort(printer.models.begin(), printer.models.end());
    return std::move(printer.models);
}

// }}}

} } } // namespace Test Output Gringo

#endif // _GRINGO_OUTPUT_TEST_SOLVER_HELPER_HH
