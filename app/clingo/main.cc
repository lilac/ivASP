/*
 * main.cc
 *
 *  Created on: Jul 12, 2013
 *      Author: james
 */

#include <gringo/input/nongroundparser.hh>
#include <gringo/input/programbuilder.hh>
#include <gringo/input/program.hh>
#include <gringo/ground/program.hh>
#include <gringo/logger.hh>
#include <iostream>
#include <stdexcept>
#include <clasp/reader.h>
#include "clasp_app.h"
#include "program_builder_outputter.hh"

bool g_verbose = false;
#define LOG if (g_verbose) std::cerr

typedef std::vector<std::string> StringSeq;
class GringoInput : public Clasp::Input {
public:
	explicit GringoInput(StringSeq& in);
	Format format() const { return Clasp::Input::SMODELS; }
	virtual bool   read(ApiPtr api, uint32 properties);
	void   addMinimize(Clasp::MinimizeBuilder& m, ApiPtr api);
	void   getAssumptions(Clasp::LitVec&) {}

private:
	Gringo::Ground::Program prg;
	Gringo::Output::OutputBase out;
};

Gringo::Ground::Program prepare(Gringo::Output::OutputBase &out, StringSeq &files) {
    using namespace Gringo;
    Input::Program prg;
    Defines defs;
    Input::NongroundProgramBuilder pb(prg, out, defs);
    Input::NonGroundParser parser(pb);
    for (auto &x : files) {
        LOG << "file: " << x << std::endl;
        parser.pushFile(x.c_str());
    }
    if (files.empty()) {
        LOG << "reading from stdin" << std::endl;
        parser.pushFile("-");
    }
    parser.parse();
    LOG << "************** parsed program **************" << std::endl << prg;
    LOG << "*********** extensional database ***********" << std::endl;
    prg.rewrite(defs);
    LOG << "************* rewritten program ************" << std::endl << prg;
    prg.check();
    if (message_printer()->hasError()) {
        throw std::runtime_error("grounding stopped because of errors");
    }
    return prg.toGround(out.domains);
}

void printVersion() {
    std::cout << "gringo version 4.0-rc2\n";
}

int main(int argc, char **argv) {
	Clasp::Application& app = Clasp::Application::instance();
	auto ig = [](Clasp::Application &app) {
		return std::unique_ptr<Clasp::Input>(new GringoInput(app.getInputs()));
	};
	return app.run(argc, argv, ig);
}

GringoInput::GringoInput(StringSeq &files) {

	using namespace Gringo;
	Output::OutputPredicates outputPredicates;

	std::move(outputPredicates.begin(), outputPredicates.end(), std::back_inserter(out.outPreds));
	prg = std::move(prepare(out, files));
}

bool GringoInput::read(ApiPtr api, uint32 properties) {
	ClaspProgramBuilderOutputter pbo(*api.api);
	out.setOutputter(pbo);
	if (!prg.linearized) { prg.linearize(out); }
	prg.ground(out);
	pbo.finish(out.domains, out.outPreds); // TODO: examine if this is necessary.
	return true;
}

inline void GringoInput::addMinimize(Clasp::MinimizeBuilder& m, ApiPtr api) {
	api.api->addMinimize(m);
}
