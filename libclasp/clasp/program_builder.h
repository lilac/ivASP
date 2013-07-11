// 
// Copyright (c) 2006, 2007, 2012 Benjamin Kaufmann
// 
// This file is part of Clasp. See http://www.cs.uni-potsdam.de/clasp/ 
// 
// Clasp is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// Clasp is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Clasp; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//

#ifndef CLASP_PROGRAM_BUILDER_H_INCLUDED
#define CLASP_PROGRAM_BUILDER_H_INCLUDED

#ifdef _MSC_VER
#pragma once
#endif

#include <clasp/program_rule.h>
#include <clasp/util/misc_types.h>
#include <string>
#include <map>
#include <algorithm>
#include <iosfwd>
#include <stdexcept>

namespace Clasp {

class SharedContext;
class MinimizeBuilder;
class Preprocessor;
class Configuration;

/**
 * \defgroup problem Problem specification
 * Classes and functions for defining/processing logic programs
 */
//@{

//! Program statistics for *one* incremental step.
struct PreproStats {
	typedef std::pair<uint32, uint32> RPair;
	PreproStats() { reset(); }
	void   reset();
	uint32 sumEqs()          const { return numEqs(Var_t::atom_var) + numEqs(Var_t::body_var) + numEqs(Var_t::atom_body_var); }
	uint32 numEqs(VarType t) const { return eqs_[t-1]; }
	RPair  rules(RuleType t) const { return rules_[ruleIndex(t)]; }
	RPair& rules(RuleType t)       { return rules_[ruleIndex(t)]; }
	bool   tr()              const { return rules_[0].first != rules_[0].second; }
	uint32 sumRules()        const;
	void   incEqs(VarType t)       { ++eqs_[t-1]; }
	void   upRule(RuleType t,  int32 i) { RPair& x = rules_[ruleIndex(t)]; x.first += i; x.second += i; }
	void   trRule(RuleType t, uint32 i) { --rules_[ruleIndex(t)].second; rules_[0].first -= i; }
	void   accu(const PreproStats& o);
	uint32 bodies;          /**< How many body-objects were created? */
	uint32 atoms;           /**< Number of program atoms */
	uint32 auxAtoms;        /**< Number of aux atoms created */
	uint32 sccs;            /**< How many strongly connected components? */
	uint32 nonHcfs;         /**< How many non head-cycle free components?*/
	uint32 gammas;          /**< How many non-hcf gamma rules */
	uint32 ufsNodes;        /**< How many nodes in the positive BADG? */
private:
	uint32 eqs_[3];         /**< How many equivalences?: eqs[0]: Atom-Atom, eqs[1]: Body-Body, eqs[2]: Other */
	RPair  rules_[NUM_RULE_TYPES]; /**< Number of rules before/after translation. */
};

//! Exception type for signaling an invalid incremental program update.
class RedefinitionError : public std::logic_error {
public:
	explicit RedefinitionError(const std::string& m) : std::logic_error(m) {}
};

//! Interface for defining a logic program.
/*!
 * Use this class to specify a logic program. Once the program is completly defined,
 * call endProgram() to load the logic program into a SharedContext object.
 */
class ProgramBuilder {
public:
	ProgramBuilder();
	~ProgramBuilder();

	//! Defines the possible modes for handling extended rules, i.e. choice, cardinality, and weight rules.
	enum ExtendedRuleMode {
		mode_native           = 0, /**< Handle extended rules natively                          */
		mode_transform        = 1, /**< Transform extended rules to normal rules                */
		mode_transform_choice = 2, /**< Transform only choice rules to normal rules             */
		mode_transform_card   = 3, /**< Transform only cardinality rules to normal rules        */
		mode_transform_weight = 4, /**< Transform cardinality- and weight rules to normal rules */
		mode_transform_integ  = 5, /**< Transform cardinality-based integrity constraints       */
		mode_transform_dynamic= 6  /**< Heuristically decide whether or not to transform a particular extended rule */
	};

	//! Options for the (Eq)-Preprocessor
	struct EqOptions {
		EqOptions() : iters(5), erMode(mode_native), noSCC(false), dfOrder(false), backprop(false), normalize(false), noGamma(false) {}
		EqOptions& iterations(uint32 it)   { iters   = it;   return *this;}
		EqOptions& depthFirst()            { dfOrder = true; return *this;}
		EqOptions& backpropagate()         { backprop= true; return *this;}
		EqOptions& noScc()                 { noSCC   = true; return *this;}
		EqOptions& noEq()                  { iters   = 0;    return *this;}
		EqOptions& disableGamma()          { noGamma = true; return *this;}
		EqOptions& ext(ExtendedRuleMode m) { erMode = m;     return *this;}
		uint32           iters;    /**< Number of iterations - 0 = disabled */
		ExtendedRuleMode erMode;   /**< ExtendedRuleMode */
		bool             noSCC;    /**< Disable scc checking, i.e. only compute supported models? */  
		bool             dfOrder;  /**< Classify in depth-first order? */
		bool             backprop; /**< Enable backpropagation? */
		bool             normalize;/**< Canonically order program */
		bool             noGamma;  /**< Disable creation of gamma rules for non-hcf disjunctions? */
	};

	/*!
	 * \name Setup and step control functions
	 */
	//@{

	//! Starts the definition of a logic program. 
	/*!
	 * This function shall be called exactly once before a new program is defined.
	 * It discards any previously added program.
	 *
	 * \param ctx    The context object in which the program builder should store the preprocessed problem.
	 * \param eqOpts Options for the eq-preprocessor.
	 */
	ProgramBuilder& startProgram(SharedContext& ctx, const EqOptions& eqOpts = EqOptions());

	//! Sets the mode for handling extended rules.
	/*!
	 * The default mode is to handle all extended rules natively.
	 */
	void setExtendedRuleMode(ExtendedRuleMode m) { eqOpts_.ext(m); }

	//! Sets the configuration to be used for checker solvers in disjunctive LP solving.
	void setNonHcfConfiguration(Configuration* c){ nonHcfCfg_ = c; }

	//! Unfreezes a currently frozen program and starts an incremental step.
	/*!
	 * \pre The program is either frozen or at step 0.
	 *
	 * If a program is to be defined incrementally, this function must be called
	 * exactly once for each step before any new rules or atoms are added.
	 * \note Program update only works correctly under the following assumptions:
	 *  - Atoms introduced in step i are either:
	 *    - solely defined in step i OR,
	 *    - marked as frozen in step i and solely defined in step i+k OR,
	 *    - forced to false by a acompute statement in step 0
	 *
	 * \post The program is no longer frozen and calling program mutating functions is valid again. 
	 * \throws std::logic_error precondition is violated.
	 */
	ProgramBuilder& updateProgram();

	//! Finishes the definition of the logic program (or its current increment).
	/*!
	 * Applies program mutating operations issued in the current step and transforms 
	 * the new program into the solver's internal representation. 
	 *
	 * \return false if the program is initially conflicting, true otherwise.
	 *
	 * \post
	 *  - If true is returned, the program is considered to be "frozen" and calling 
	 *    program mutating functions is invalid until the next call to updateProgram().
	 *  - If false is returned, the state of the object is undefined and startProgram() 
	 *    and disposeProgram() are the only remaining valid operations. 
	 *  . 
	 */
	bool endProgram();

	//! Adds all minimize statements contained in the program to m.
	/*!
	 * \pre The program is frozen.
	 */
	void addMinimize(MinimizeBuilder& m);

	//! Writes the (possibly simplified) program in lparse-format to the given stream.
	void writeProgram(std::ostream& os);
	
	//! Disposes (parts of) the internal representation of the logic program.
	/*!
	 * \param forceFullDispose If set to true, the whole program is disposed. Otherwise,
	 *  only the rules (of the current step) are disposed but atoms and any incremental
	 *  control data remain.
	 */
	void disposeProgram(bool forceFullDispose);

	//! Clones the program and adds it to the given ctx.
	/*
	 * \pre The program is currently frozen.
	 */
	bool cloneProgram(SharedContext& ctx);

	//@}

	/*!
	 * \name Program mutating functions
	 * 
	 * Functions in this group shall only be called if the program is currently not 
	 * frozen. That is, only between the call to startProgram() (resp. updateProgram() if in 
	 * incremental setting) and endProgram(). A std::logic_error is raised if this precondition is violated. 
	 *
	 */
	//@{

	//! Adds a new atom to the program.
	/*!
	 * \return The new atom's id. 
	 */
	Var newAtom();

	//! Sets the name of the given atom and adds it to the program's symbol table.
	/*!
	 * \pre 
	 *   - The atom is either not yet known or was added in the current step (atomId >= startAtom()).
	 *   - The atom was not yet added to the symbol table, i.e. 
	 *     setAtomName() is called at most once for an atom.
	 *   . 
	 * \param atomId The id of the atom for which a name should be set
	 * \param name The new name of the atom with the given id.
	 * \note If atomId is not yet known, an atom with the given id is implicitly created. 
	 *
	 * \throws RedefinitionError precondition is violated. 
	 * \throws std::logic_error  program is frozen.
	 */
	ProgramBuilder& setAtomName(Var atomId, const char* name);

	//! Forces the atom's truth-value to value. 
	/*!
	 * \pre The atom is either not yet known, false, or an atom from the current step.
	 * \param atomId Id of the Atom for which a truth-value should be set.
	 * \param pos If true, atom is set to true (forced to be in every answer set). Otherwise
	 *            atom is set to false (not part of any answer set).
	 * \note If atomId is not yet known, an atom with the given id is implicitly created. 
	 */
	ProgramBuilder& setCompute(Var atomId, bool value);

	//! Protects an otherwise undefined atom from preprocessing. 
	/*!
	 * If the atom is defined in this or a previous step, the operation has no effect. 
	 * \note If atomId is not yet known, an atom with the given id is implicitly created. 
	 */
	ProgramBuilder& freeze(Var atomId);

	//! Removes any protection from the given atom. 
	/*!
	 * If the atom is defined in this or a previous step, the operation has no effect. 
	 * \note
	 *   - The effect is logically equivalent to adding a rule atomId :- false. 
	 *   - A call to unfreeze() always overwrites a call to freeze() even if the 
	 *     latter comes after the former
	 *   . 
	 */
	ProgramBuilder& unfreeze(Var atomId);

	//! Adds the given rule to the program.
	/*!
	 * \pre The head of the rule does not contain an atom defined in a 
	 *      previous incremental step. 
	 *
	 * Simplifies the given rule and adds it to the program if it
	 * is neither tautological (e.g. a :- a) nor contradictory (e.g. a :- b, not b). 
	 * Atoms in the simplified rule that are not yet known are implicitly created. 
	 *
	 * \throws RedefinitionError if the precondition is violated. 
	 * \note If the head of the simplified rule mentions an atom from a previous step,
	 *       that atom shall either be frozen or false. In the former case, 
	 *       unfreeze() is implicitly called. In the latter case, the rule is interpreted 
	 *       as an integrity constraint. 
	 */
	ProgramBuilder& addRule(const PrgRule& r);
	
	//@}

	/*!
	 * \name Rule creation functions
	 * 
	 * Functions in this group may be used to construct logic program rules.
	 * The construction of a rule must start with a call to startRule() and
	 * ends with a call to endRule(). Functions for adding elements to a
	 * rule shall only be called between calls to startRule()/endRule() and
	 * only one rule can be under construction at any one time.
	 */
	//@{

	//! Starts the construction of a rule.
	/*! 
	 * \param t The type of the new rule.
	 * \param bound The lower bound (resp. min weight) of the rule to be created.
	 *
	 * \note the bound-parameter is only interpreted if the rule to be created is
	 * either a constraint- or a weight-rule.
	 */
	ProgramBuilder& startRule(RuleType t = BASICRULE, weight_t bound = -1) {
		rule_.clear();
		rule_.setType(t);
		if ((t == CONSTRAINTRULE || t == WEIGHTRULE) && bound > 0) {
			rule_.setBound(bound);
		}
		return *this;
	}

	//! Sets the bound (resp. min weight) of the currently active rule.
	/*!
	 * \param bound The lower bound (resp. min weight) of the rule to be created.
	 * \pre The rule under construction is either a constraint or weight rule.
	 */
	ProgramBuilder& setBound(weight_t bound) { // only valid for CONSTRAINT and WEIGHTRULES
		rule_.setBound(bound);
		return *this;
	}

	//! Adds the atom with the given id as a head to the currently active rule.
	ProgramBuilder& addHead(Var atomId) {
		assert(atomId > 0);
		rule_.addHead(atomId);
		return *this;
	}
	
	//! Adds a subgoal to the currently active rule.
	/*!
	 * \pre atomId > 0 && weight >= 0
	 * \param atomId The id of the atom to be added to the rule.
	 * \param pos true if the atom is positive. Fals otherwise
	 * \param weight The weight the new predecessor should have in the rule.
	 * \note The weight parameter is only used if the active rule is a weight or optimize rule.
	 */
	ProgramBuilder& addToBody(Var atomId, bool pos, weight_t weight = 1) {
		rule_.addToBody(atomId, pos, weight);
		return *this;
	}
	
	//! Finishes the construction of the active rule and adds it to the program.
	/*!
	 * \see ProgramBuilder::addRule(PrgRule&);
	 */
	ProgramBuilder& endRule() {
		return addRule(rule_);
	}
	
	//@}

	/*!
	 * \name Query functions
	 * 
	 * Functions in this group are useful to query important information
	 * once the program is frozen, i.e. after endProgram() was called.
	 */
	 //@{
	
	//! Returns true if the program contains any minimize statements.
	bool   hasMinimize()     const { return minimize_ != 0; }
	//! Returns the number of atoms in the logic program.
	uint32 numAtoms()        const { return (uint32)atoms_.size()-1; }
	//! Returns the number of bodies in the current (slice of the) logic program.
	uint32 numBodies()       const { return (uint32)bodies_.size(); }
	//! Returns the number of disjunctive heads.
	uint32 numDisjunctions() const { return (uint32)disjunctions_.size(); }
	//! Returns the id of the first atom of the current step.
	Var    startAtom()       const { return incData_?incData_->startAtom_:1; }
	//! Returns the stored context object
	SharedContext* context() const { return ctx_; }
	//! Returns the internal literal that is associated with the given atom.
	/*!
	 * \pre atomId is a known atom
	 * \return A literal that is valid in the current solving context. 
	 * \note Untill endProgram() is called, atoms from the current step are
	 *       associated with the always-false literal negLit(0).
	 * \throws std::logic_error if precondition is violated.
	 */
	Literal getLiteral(Var atomId) const;
	//! Returns a vector of internal literals that, when assumed true, makes all frozen atoms false. 
	/*!
	 * \pre The program is currently frozen. 
	 */
	void    getAssumptions(LitVec& out) const;

	PreproStats stats;
	//@}

	/*!
	 * \name Implementation functions
	 * Low-level implementation functions. Use with care and only if you
	 * know what you are doing!
	 */
	//@{
	typedef VarVec::const_iterator                VarIter;
	typedef PrgHead*const*                        HeadIter;
	typedef std::pair<EdgeIterator, EdgeIterator> EdgeRange;
	typedef std::pair<HeadIter, HeadIter>         HeadRange;
	const EqOptions& options()     const { return eqOpts_; }
	bool       hasConflict()       const { return !ok_; }
	PrgAtom*   getAtom(Var atomId) const { return atoms_[atomId]; }
	PrgHead*   getHead(PrgEdge it) const { return it.isAtom() ? (PrgHead*)getAtom(it.node()) : (PrgHead*)getDisj(it.node()); }
	PrgNode*   getSupp(PrgEdge it) const { return it.isBody() ? (PrgNode*)getBody(it.node()) : (PrgNode*)getDisj(it.node()); }
	Var        getEqAtom(Var a)    const { return getEqNode(atoms_, a); }
	PrgBody*   getBody(Var bodyId) const { return bodies_[bodyId]; }
	Var        getEqBody(Var b)    const { return getEqNode(bodies_, b); }
	PrgDisj*   getDisj(Var disjId) const { return disjunctions_[disjId]; }
	bool       isFact(PrgAtom* a)  const { return a->value() == value_true && eqOpts_.noSCC == false; }
	HeadIter   disj_begin()        const { return disjunctions_.empty() ? 0 : reinterpret_cast<HeadIter>(&disjunctions_[0]); }
	HeadIter   disj_end()          const { return disj_begin() + numDisjunctions(); }
	HeadIter   atom_begin()        const { return reinterpret_cast<HeadIter>(&atoms_[0]); }
	HeadIter   atom_end()          const { return atom_begin() + (numAtoms()+1); }
	VarIter    unfreeze_begin()    const { return incData_?incData_->unfreeze_.begin() : activeHead_.end(); }
	VarIter    unfreeze_end()      const { return incData_?incData_->unfreeze_.end()   : activeHead_.end(); }
	RuleType   simplifyRule(const PrgRule& r, VarVec& head, BodyInfo& info);
	VarVec&    getSupportedBodies(bool sorted);
	uint32     update(PrgBody* b, uint32 oldHash, uint32 newHash);
	bool       assignValue(PrgAtom* a, ValueRep v);
	bool       assignValue(PrgHead* h, ValueRep v);
	bool       propagate(bool backprop);
	PrgAtom*   mergeEqAtoms(PrgAtom* a, Var rootAtom);
	PrgBody*   mergeEqBodies(PrgBody* b, Var rootBody, bool hashEq, bool atomsAssigned);
	bool       propagate()               { return propagate(options().backprop); }
	void       setConflict()             { ok_ = false; }
	RuleState& ruleState()               { return ruleState_; }
	// ------------------------------------------------------------------------
	// Statistics
	void      upRules(RuleType r, int i) { stats.upRule(r, i);  }
	void      incTr(RuleType r, uint32 n){ stats.trRule(r, n);  }
	void      incTrAux(uint32 n)         { stats.auxAtoms += n; }
	void      incEqs(VarType t)          { stats.incEqs(t);     }
	// ------------------------------------------------------------------------
	//@}
private:
	ProgramBuilder(const ProgramBuilder&);
	ProgramBuilder& operator=(const ProgramBuilder&);	
	typedef PodVector<PrgRule*>::type RuleList;
	typedef std::multimap<uint32, uint32>   BodyIndex; // hash -> bodies[offset]
	typedef std::multimap<uint32, uint32>   DisjIndex; // hash -> bodies[offset]
	typedef BodyIndex::iterator             IndexIter;
	typedef std::pair<IndexIter, IndexIter> IndexRange;
	typedef PodVector<uint8>::type          SccMap;
	struct MinimizeRule;
	// ------------------------------------------------------------------------
	// Program definition
	PrgAtom*      resize(Var atomId);
	void          addRuleImpl(RuleType t,    const VarVec& head, BodyInfo& body);
	bool          handleNatively(RuleType t, const BodyInfo& i) const;
	bool          transformNoAux(RuleType t, const BodyInfo& i) const;
	void          transformExtended();
	void          transformIntegrity(uint32 maxAux);
	PrgBody*      getBodyFor(BodyInfo& body, bool addDeps = true);
	PrgDisj*      getDisjFor(const VarVec& heads, uint32 headHash);
	PrgBody*      assignBodyFor(BodyInfo& body, EdgeType x, bool strongSimp);
	uint32        findEqBody(PrgBody* b, uint32 hash);
	uint32        equalBody(const IndexRange& range, BodyInfo& info) const;
	RuleType      simplifyBody(const PrgRule& r, BodyInfo& info);
	uint32        removeBody(PrgBody* b, uint32 oldHash);
	Literal       getEqAtomLit(Literal lit, const BodyList& supports, Preprocessor& p, const SccMap& x);
	bool          positiveLoopSafe(PrgBody* b, PrgBody* root) const;
	template <class C>
	Var getEqNode(C& vec, Var id)  const {
		if (!vec[id]->eq()) return id;
		typedef typename C::value_type NodeType;
		NodeType n = vec[id];
		NodeType r;
		Var root   = n->id();
		for (r = vec[root]; r->eq(); r = vec[root]) {
			// if n == r and r == r' -> n == r'
			n->setEq(root = r->id());
		}
		return root;
	}
	void          updateFrozenAtoms();
	void          normalize();
	// ------------------------------------------------------------------------
	// Nogood creation
	bool prepareProgram(bool checkSccs);
	void finalizeDisjunctions(Preprocessor& p, uint32 numSccs);
	void prepareNonHcfComponents();
	void simplifyMinimize();
	bool addConstraints();
	// ------------------------------------------------------------------------
	void writeBody(const BodyInfo& body, std::ostream&) const;
	void transform(const PrgBody& body, BodyInfo& out)  const;
	void transform(const MinimizeRule&, BodyInfo& out)  const;
	Var  getFalseAtom() const;
	PrgRule       rule_;        // active rule
	BodyInfo      activeBody_;  // simplified body of active rule
	VarVec        activeHead_;  // simplified head of active rule
	RuleState     ruleState_;   // which atoms appear in the active rule?
	RuleList      extended_;    // extended rules to be translated
	BodyIndex     bodyIndex_;   // hash -> body
	DisjIndex     disjIndex_;   // hash -> disjunction
	BodyList      bodies_;      // all bodies
	AtomList      atoms_;       // all atoms
	AtomList      sccAtoms_;    // atoms that are strongly connected
	DisjList      disjunctions_;// all (head) disjunctions
	VarVec        initialSupp_; // bodies that are (initially) supported
	VarVec        propQ_;       // assigned atoms
	NonHcfSet     nonHcfs_;     // set of non-hcf sccs
	struct MinimizeRule {
		WeightLitVec lits_;
		MinimizeRule* next_;
	} *minimize_;               // list of minimize-rules
	struct Incremental  {
		Incremental();
		uint32  startAtom_;       // first atom of current iteration
		uint32  startAux_;        // first aux atom of current iteration
		uint32  startScc_;        // first valid scc number in this iteration
		VarVec  freeze_;          // list of frozen atoms
		VarVec  unfreeze_;        // list of atom that are unfreezed in this iteration
	}* incData_;                // additional state to handle incrementally defined programs 
	SharedContext*   ctx_;      // context object containing symbol table and var info
	Configuration*   nonHcfCfg_;// optional configuration second level solvers
	EqOptions        eqOpts_;   // eq-preprocessing 
	bool             frozen_;   // is the program currently frozen?
	bool             ok_;       // conflict?
};
}
#endif
