/*
 * program_builder_outputter.cc
 *
 *  Created on: Jul 12, 2013
 *      Author: james
 */
#include "program_builder_outputter.hh"
#include <cassert>
// {{{ definition of ClaspProgramBuilderOutputter
ClaspProgramBuilderOutputter::ClaspProgramBuilderOutputter(Clasp::ProgramBuilder &pb): pb(pb) { }

void ClaspProgramBuilderOutputter::printBasicRule(unsigned head,
		const LitVec& body) {
	pb.startRule();
    pb.addHead(head);
    for (auto &x : body) {
        if (x < 0) { pb.addToBody(-x, false); }
        else if (x > 0) { pb.addToBody(x, true); }
        else { assert(!"Var == 0!"); }
    }
    pb.endRule();
}

void ClaspProgramBuilderOutputter::printChoiceRule(const AtomVec& head,
		const LitVec& body) {
    pb.startRule(Clasp::CHOICERULE);
    for (auto &x : head) {
        pb.addHead(x);
    }
    for (auto &x : body) {
        if (x < 0) { pb.addToBody(-x, false); }
        else if (x > 0) { pb.addToBody(x, true); }
        else { assert(false); }
    }
    pb.endRule();
}

void ClaspProgramBuilderOutputter::printCardinalityRule(unsigned head,
		unsigned lower, const LitVec& body) {
    pb.startRule(Clasp::CONSTRAINTRULE, lower);
    pb.addHead(head);
    for (auto &x : body) {
	if (x < 0) { pb.addToBody(-x, false); }
	else if (x > 0) { pb.addToBody(x, true); }
	else { assert(false); }
    }
    pb.endRule();
}

void ClaspProgramBuilderOutputter::printWeightRule(unsigned head,
		unsigned lower, const LitWeightVec& body) {
	pb.startRule(Clasp::WEIGHTRULE, lower);
	pb.addHead(head);
	for (auto &x : body) {
		int lit = std::get<0>(x);
		unsigned w = std::get<1>(x);
		if (lit < 0) { pb.addToBody(-lit, false, w); }
		else if (lit > 0) { pb.addToBody(lit, true, w); }
		else { assert(false); }
	}
	pb.endRule();
}

void ClaspProgramBuilderOutputter::printMinimize(const LitWeightVec& body) {
	pb.startRule(Clasp::OPTIMIZERULE);
	for (auto &x : body) {
		int lit = std::get<0>(x);
		unsigned w = std::get<1>(x);
		if (lit < 0) { pb.addToBody(-lit, false, w); }
		else if (lit > 0) { pb.addToBody(lit, true, w); }
		else { assert(false); }
	}
	pb.endRule();
}

void ClaspProgramBuilderOutputter::printDisjunctiveRule(const AtomVec& head,
		const LitVec& body) {
}

unsigned ClaspProgramBuilderOutputter::falseUid() {
	return 1;
}

unsigned ClaspProgramBuilderOutputter::newUid() {
	return uids++;
}

void ClaspProgramBuilderOutputter::finishRules() {
}

void ClaspProgramBuilderOutputter::printSymbol(unsigned atomUid, Gringo::Value v) {
	std::stringstream ss;
	ss << v;
	pb.setAtomName(atomUid, ss.str().c_str());
}

void ClaspProgramBuilderOutputter::finishSymbols() {
}

// }}}
