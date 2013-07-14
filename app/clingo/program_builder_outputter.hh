/*
 * program_builder_outputter.hh
 *
 *  Created on: Jul 12, 2013
 *      Author: james
 */

#ifndef PROGRAM_BUILDER_OUTPUTTER_HH_
#define PROGRAM_BUILDER_OUTPUTTER_HH_

#include <gringo/output/statement.hh>
#include <clasp/program_builder.h>
using namespace Gringo::Output;

struct ClaspProgramBuilderOutputter : LparseOutputter {
	ClaspProgramBuilderOutputter(Clasp::ProgramBuilder &api);
	virtual void printBasicRule(unsigned head, LitVec const &body);
	virtual void printChoiceRule(AtomVec const &head, LitVec const &body);
	virtual void printCardinalityRule(unsigned head, unsigned lower, LitVec const &body);
	virtual void printWeightRule(unsigned head, unsigned lower, LitWeightVec const &body);
	virtual void printMinimize(LitWeightVec const &body);
	virtual void printDisjunctiveRule(AtomVec const &head, LitVec const &body);
	virtual unsigned falseUid();
	virtual unsigned newUid();
	virtual void finishRules();
	virtual void printSymbol(unsigned atomUid, Gringo::Value v);
	virtual void finishSymbols();
	virtual ~ClaspProgramBuilderOutputter() {}

	Clasp::ProgramBuilder &pb;
	unsigned      uids = 2;
};



#endif /* PROGRAM_BUILDER_OUTPUTTER_HH_ */
