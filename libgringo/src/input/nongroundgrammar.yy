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

%define namespace "Gringo::Input::NonGroundGrammar"
%name-prefix="GringoNonGroundGrammar_"
%locations
%defines
%error-verbose
%parse-param { Gringo::Input::NonGroundParser *lexer }
%lex-param { Gringo::Input::NonGroundParser *lexer }
%skeleton "lalr1.cc"
%pure_parser
//%debug

// {{{ auxiliary code

%code requires
{
    #include "gringo/input/programbuilder.hh"

    namespace Gringo { namespace Input { class NonGroundParser; } }
}

%{

#include "gringo/input/nongroundparser.hh"
#include "input/nongroundgrammar/location.hh"
#include "gringo/input/programbuilder.hh"
#include <climits> 

#define BUILDER (lexer->builder())

using namespace Gringo;
using namespace Gringo::Input;

int GringoNonGroundGrammar_lex(void *value, NonGroundGrammar::location* loc, NonGroundParser *lexer) {
    return lexer->lex(value, *loc);
}

namespace {
    Location loc(NonGroundGrammar::location const &loc) {
        // Note: constructing the FWStrings over and over again costs (a little)
        return Location(*loc.begin.filename, loc.begin.line, loc.begin.column, *loc.end.filename, loc.end.line, loc.end.column);
    }
} // namespace

%}

%code {

void NonGroundGrammar::parser::error(NonGroundGrammar::location const &l, std::string const &msg) {
    lexer->parseError(loc(l), msg);
}

}

// }}}
// {{{ nonterminals
// {{{ union type for stack elements
%union
{
    TermUid term;
    TermVecUid termvec;
    TermVecVecUid termvecvec;
    LitVecUid litvec;
    LitUid lit;
    BdAggrElemVecUid bodyaggrelemvec;
    CondLitVecUid condlitlist;
    HdAggrElemVecUid headaggrelemvec;
    BoundVecUid bounds;
    BdLitVecUid body;
    HdLitUid head;
    Relation rel;
    AggregateFunction fun;
    struct {
        unsigned first;
        unsigned second;
    } pair;
    struct {
        TermVecUid first;
        LitVecUid second;
    } bodyaggrelem;
    struct {
        LitUid first;
        LitVecUid second;
    } lbodyaggrelem;
    struct {
        AggregateFunction fun;
        unsigned choice : 1;
        unsigned elems : 31;
    } aggr;
    struct {
        Relation rel;
        TermUid term;
    } bound;
	struct {
		TermUid first;
		TermUid second;
	} termpair;
    unsigned uid;
    int num;
}

// }}}

// TODO: improve naming scheme
%type <term> constterm term
%type <termvec> termvec ntermvec consttermvec unaryargvec optimizetuple
%type <termvecvec> argvec constargvec
%type <lit> literal
%type <litvec> litvec nlitvec optcondition noptcondition
%type <bodyaggrelem> bodyaggrelem
%type <lbodyaggrelem> altbodyaggrelem conjunction
%type <bodyaggrelemvec> bodyaggrelemvec
%type <condlitlist> altbodyaggrelemvec altheadaggrelemvec disjunctionsep disjunction
%type <headaggrelemvec> headaggrelemvec
%type <bound> upper
%type <body> bodycomma bodydot optimizelitvec optimizecond
%type <head> head

%type <uid> 
    identifier 
    lubodyaggregate
    luheadaggregate
%type <pair> 
    atom
%type <fun> 
    aggregatefunction
%type <aggr>
    bodyaggregate headaggregate
%type <rel>
    cmp
%type <termpair>
	optimizeweight

// }}}
// {{{ terminals

%token
    ADD         "+"
    AND         "&"
    ASSIGN      "="
    AT          "@"
    BASE        "#base"
    BNOT        "~"
    COLON       ":"
    COMMA       ","
    CONST       "#const"
    COUNT       "#count"
    CUMULATIVE  "#cumulative"
    DOT         "."
    DOTS        ".."
    EQ          "=="
    END         0 "<EOF>"
    EXTERNAL    "#external"
    FALSE       "#false"
    FORGET      "#forget"
    GEQ         ">="
    GT          ">"
    IF          ":-"
    INCLUDE     "#include"
    INFIMUM     "#inf"
    LBRACE      "{"
    LBRACK      "["
    LEQ         "<="
    LPAREN      "("
    LT          "<"
    MAX         "#max"
    MAXIMIZE    "#maximize"
    MIN         "#min"
    MINIMIZE    "#minimize"
    MOD         "\\"
    MUL         "*"
    NEQ         "!="
    POW         "**"
    QUESTION    "?"
    RBRACE      "}"
    RBRACK      "]"
    RPAREN      ")"
    SEM         ";"
    SHOW        "#show"
    SLASH       "/"
    SUB         "-"
    SUM         "#sum"
    SUMP        "#sum+"
    SUPREMUM    "#sup"
    TRUE        "#true"
    UBNOT
    UMINUS
    VBAR        "|"
    VOLATILE    "#volatile"
    WIF         ":~"
    XOR         "^"

%token <num>
    NUMBER     "<NUMBER>"

%token <uid>
    ANONYMOUS  "<ANONYMOUS>"
    IDENTIFIER "<IDENTIFIER>"
    LUA        "<LUA>"
    STRING     "<STRING>"
    VARIABLE   "<VARIABLE>"
    NOT        "not"
    SHOWSIG   "<SHOWSIG>"

// {{{ operator precedence and associativity

%left DOTS
%left XOR
%left QUESTION
%left AND
%left ADD SUB
%left MUL SLASH MOD
%right POW
%left UMINUS UBNOT

// }}}
// }}}

%%

// {{{ logic program and global definitions

start
    : program
    ;

program
    : program statement
    | 
    ;

// Note: skip until the next "." in case of an error

statement
    : error DOT
    ;

identifier
    : IDENTIFIER[a] { $$ = $a; }
    ;

// }}}
// {{{ terms
// {{{ constterms are terms without variables and pooling operators

constterm
    : constterm[a] XOR constterm[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::XOR, $a, $b); }
    | constterm[a] QUESTION constterm[b]                  { $$ = BUILDER.term(loc(@a + @b), BinOp::OR, $a, $b); }
    | constterm[a] AND constterm[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::AND, $a, $b); }
    | constterm[a] ADD constterm[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::ADD, $a, $b); }
    | constterm[a] SUB constterm[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::SUB, $a, $b); }
    | constterm[a] MUL constterm[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::MUL, $a, $b); }
    | constterm[a] SLASH constterm[b]                     { $$ = BUILDER.term(loc(@a + @b), BinOp::DIV, $a, $b); }
    | constterm[a] MOD constterm[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::MOD, $a, $b); }
    | constterm[a] POW constterm[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::POW, $a, $b); }
    | SUB[l] constterm[a] %prec UMINUS                    { $$ = BUILDER.term(loc(@l + @a), UnOp::NEG, $a); }
    | BNOT[l] constterm[a] %prec UBNOT                    { $$ = BUILDER.term(loc(@l + @a), UnOp::NOT, $a); }
    | LPAREN[l] constargvec[a] RPAREN[r]                  { $$ = BUILDER.term(loc(@l + @r), FWString(""), $a, false); }
    | identifier[a] LPAREN constargvec[b] RPAREN[r]       { $$ = BUILDER.term(loc(@a + @r), $a, $b, false); }
    | AT[l] identifier[a] LPAREN constargvec[b] RPAREN[r] { $$ = BUILDER.term(loc(@l + @r), $a, $b, true); }
    | VBAR[l] constterm[a] VBAR[r]                        { $$ = BUILDER.term(loc(@l + @r), UnOp::ABS, $a); }
    | identifier[a]                                       { $$ = BUILDER.term(loc(@a), Value(FWString($a))); }
    | NUMBER[a]                                           { $$ = BUILDER.term(loc(@a), Value($a)); }
    | STRING[a]                                           { $$ = BUILDER.term(loc(@a), Value(FWString($a), false)); }
    | INFIMUM[a]                                          { $$ = BUILDER.term(loc(@a), Value(true)); }
    | SUPREMUM[a]                                         { $$ = BUILDER.term(loc(@a), Value(false)); }
    ;

// {{{ arguments lists for functions in constant terms

consttermvec
    : constterm[a]                       { $$ = BUILDER.termvec(BUILDER.termvec(), $a);  }
    | consttermvec[a] COMMA constterm[b] { $$ = BUILDER.termvec($a, $b);  }
    ;

constargvec
    : consttermvec[a] { $$ = BUILDER.termvecvec(BUILDER.termvecvec(), $a);  }
    |                 { $$ = BUILDER.termvecvec();  }
    ;

// }}}
// }}}
// {{{ terms including variables

term
    : term[a] DOTS term[b]                           { $$ = BUILDER.term(loc(@a + @b), $a, $b); }
    | term[a] XOR term[b]                            { $$ = BUILDER.term(loc(@a + @b), BinOp::XOR, $a, $b); }
    | term[a] QUESTION term[b]                       { $$ = BUILDER.term(loc(@a + @b), BinOp::OR, $a, $b); }
    | term[a] AND term[b]                            { $$ = BUILDER.term(loc(@a + @b), BinOp::AND, $a, $b); }
    | term[a] ADD term[b]                            { $$ = BUILDER.term(loc(@a + @b), BinOp::ADD, $a, $b); }
    | term[a] SUB term[b]                            { $$ = BUILDER.term(loc(@a + @b), BinOp::SUB, $a, $b); }
    | term[a] MUL term[b]                            { $$ = BUILDER.term(loc(@a + @b), BinOp::MUL, $a, $b); }
    | term[a] SLASH term[b]                          { $$ = BUILDER.term(loc(@a + @b), BinOp::DIV, $a, $b); }
    | term[a] MOD term[b]                            { $$ = BUILDER.term(loc(@a + @b), BinOp::MOD, $a, $b); }
    | term[a] POW term[b]                            { $$ = BUILDER.term(loc(@a + @b), BinOp::POW, $a, $b); }
    | SUB[l] term[a] %prec UMINUS                    { $$ = BUILDER.term(loc(@l + @a), UnOp::NEG, $a); }
    | BNOT[l] term[a] %prec UBNOT                    { $$ = BUILDER.term(loc(@l + @a), UnOp::NOT, $a); }
    | LPAREN[l] argvec[a] RPAREN[r]                  { $$ = BUILDER.term(loc(@l + @r), FWString(""), $a, false); }
    | identifier[a] LPAREN argvec[b] RPAREN[r]       { $$ = BUILDER.term(loc(@a + @r), $a, $b, false); }
    | AT[l] identifier[a] LPAREN argvec[b] RPAREN[r] { $$ = BUILDER.term(loc(@l + @r), $a, $b, true); }
    | VBAR[l] unaryargvec[a] VBAR[r]                 { $$ = BUILDER.term(loc(@l + @r), UnOp::ABS, $a); }
    | identifier[a]                                  { $$ = BUILDER.term(loc(@a), Value(FWString($a))); }
    | NUMBER[a]                                      { $$ = BUILDER.term(loc(@a), Value($a)); }
    | STRING[a]                                      { $$ = BUILDER.term(loc(@a), Value(FWString($a), false)); }
    | INFIMUM[a]                                     { $$ = BUILDER.term(loc(@a), Value(true)); }
    | SUPREMUM[a]                                    { $$ = BUILDER.term(loc(@a), Value(false)); }
    | VARIABLE[a]                                    { $$ = BUILDER.term(loc(@a), FWString($a)); }
    | ANONYMOUS[a]                                   { $$ = BUILDER.term(loc(@a), FWString("_")); }
    ;

// {{{ argument lists for unary operations

unaryargvec
    : term[a]                    { $$ = BUILDER.termvec(BUILDER.termvec(), $a); }
    | unaryargvec[a] SEM term[b] { $$ = BUILDER.termvec($a, $b); }
    ;

// }}}
// {{{ argument lists for functions

ntermvec
    : term[a]                   { $$ = BUILDER.termvec(BUILDER.termvec(), $a); }
    | ntermvec[a] COMMA term[b] { $$ = BUILDER.termvec($a, $b); }
    ;

termvec
    : ntermvec[a] { $$ = $a; }
    |             { $$ = BUILDER.termvec(); }
    ;

argvec
    : termvec[a]               { $$ = BUILDER.termvecvec(BUILDER.termvecvec(), $a); }
    | argvec[a] SEM termvec[b] { $$ = BUILDER.termvecvec($a, $b); }
    ;

// }}}
// }}}
// }}}
// {{{ literals

cmp
    : GT     { $$ = Relation::GT; }
    | LT     { $$ = Relation::LT; }
    | GEQ    { $$ = Relation::GEQ; }
    | LEQ    { $$ = Relation::LEQ; }
    | EQ     { $$ = Relation::EQ; }
    | NEQ    { $$ = Relation::NEQ; }
    | ASSIGN { $$ = Relation::ASSIGN; }
    ;

atom
    : identifier[id]                                  { $$ = { $id, BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec()) << 1u }; }
    | identifier[id] LPAREN argvec[tvv] RPAREN[r]     { $$ = { $id, $tvv << 1u }; }
    | SUB identifier[id]                              { $$ = { $id, BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec()) << 1u | 1u }; }
    | SUB identifier[id] LPAREN argvec[tvv] RPAREN[r] { $$ = { $id, $tvv << 1u | 1u }; }
    ;

literal
    : TRUE[a]                  { $$ = BUILDER.boollit(loc(@a), true); }
    | FALSE[a]                 { $$ = BUILDER.boollit(loc(@a), false); }
    | atom[a]                  { $$ = BUILDER.predlit(loc(@a), NAF::POS, $a.second & 1, FWString($a.first), TermVecVecUid($a.second >> 1u)); }
    | NOT[l] atom[a]           { $$ = BUILDER.predlit(loc(@a), NAF::NOT, $a.second & 1, FWString($a.first), TermVecVecUid($a.second >> 1u)); }
    | NOT[l] NOT atom[a]       { $$ = BUILDER.predlit(loc(@a), NAF::NOTNOT, $a.second & 1, FWString($a.first), TermVecVecUid($a.second >> 1u)); }
    | term[l] cmp[rel] term[r] { $$ = BUILDER.rellit(loc(@l + @r), $rel, $l, $r); }
    ;

// }}}
// {{{ aggregates
// {{{ auxiliary rules

nlitvec
    : literal[lit]                    { $$ = BUILDER.litvec(BUILDER.litvec(), $lit); }
    | nlitvec[vec] COMMA literal[lit] { $$ = BUILDER.litvec($vec, $lit); }
    ;

litvec
    : nlitvec[vec] { $$ = $vec; }
    |              { $$ = BUILDER.litvec(); }
    ;

optcondition
    : COLON litvec[vec] { $$ = $vec; }
    |                   { $$ = BUILDER.litvec(); }
    ;

noptcondition
    : COLON nlitvec[vec] { $$ = $vec; }
    |                    { $$ = BUILDER.litvec(); }
    ;

aggregatefunction
    : SUM   { $$ = AggregateFunction::SUM; }
    | SUMP  { $$ = AggregateFunction::SUMP; }
    | MIN   { $$ = AggregateFunction::MIN; }
    | MAX   { $$ = AggregateFunction::MAX; }
    | COUNT { $$ = AggregateFunction::COUNT; }
    ;

// }}}
// {{{ body aggregates
// {{{ body aggregate elements

bodyaggrelem
    : COLON litvec[cond]                { $$ = { BUILDER.termvec(), $cond }; }
    | ntermvec[args] optcondition[cond] { $$ = { $args, $cond }; }
    ;

bodyaggrelemvec
    : bodyaggrelem[elem]                          { $$ = BUILDER.bodyaggrelemvec(BUILDER.bodyaggrelemvec(), $elem.first, $elem.second); }
    | bodyaggrelemvec[vec] SEM bodyaggrelem[elem] { $$ = BUILDER.bodyaggrelemvec($vec, $elem.first, $elem.second); }
    ;

// Note: alternative syntax (without weight)

altbodyaggrelem
    : literal[lit] optcondition[cond] { $$ = { $lit, $cond }; }
    ;

altbodyaggrelemvec
    : altbodyaggrelem[elem]                             { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $elem.first, $elem.second); }
    | altbodyaggrelemvec[vec] SEM altbodyaggrelem[elem] { $$ = BUILDER.condlitvec($vec, $elem.first, $elem.second); }
    ;

// }}}

bodyaggregate
    : LBRACE RBRACE                                               { $$ = { AggregateFunction::COUNT, true, BUILDER.condlitvec() }; }
    | LBRACE altbodyaggrelemvec[elems] RBRACE                     { $$ = { AggregateFunction::COUNT, true, $elems }; }
    | aggregatefunction[fun] LBRACE RBRACE                        { $$ = { $fun, false, BUILDER.bodyaggrelemvec() }; }
    | aggregatefunction[fun] LBRACE bodyaggrelemvec[elems] RBRACE { $$ = { $fun, false, $elems }; }
    ;

upper
    : term[t]          { $$ = { Relation::LEQ, $t }; }
    | cmp[rel] term[t] { $$ = { $rel, $t }; }
    |                  { $$ = { Relation::LEQ, TermUid(-1) }; }
    ;

lubodyaggregate
    : term[l]          bodyaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, $l, $u.rel, $u.term)); }
    | term[l] cmp[rel] bodyaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec($rel, $l, $u.rel, $u.term)); }
    |                  bodyaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, TermUid(-1), $u.rel, $u.term)); }
    ;

// }}}
// {{{ head aggregates
// {{{ head aggregate elements

headaggrelemvec
    : headaggrelemvec[vec] SEM termvec[tuple] COLON literal[head] optcondition[cond] { $$ = BUILDER.headaggrelemvec($vec, $tuple, $head, $cond); }
    | termvec[tuple] COLON literal[head] optcondition[cond]                          { $$ = BUILDER.headaggrelemvec(BUILDER.headaggrelemvec(), $tuple, $head, $cond); }
    ;

altheadaggrelemvec
    : literal[lit] optcondition[cond]                             { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $lit, $cond); }
    | altheadaggrelemvec[vec] SEM literal[lit] optcondition[cond] { $$ = BUILDER.condlitvec($vec, $lit, $cond); }
    ;

/// }}}

headaggregate
    : aggregatefunction[fun] LBRACE RBRACE                        { $$ = { $fun, false, BUILDER.headaggrelemvec() }; }
    | aggregatefunction[fun] LBRACE headaggrelemvec[elems] RBRACE { $$ = { $fun, false, $elems }; }
    | LBRACE RBRACE                                               { $$ = { AggregateFunction::COUNT, true, BUILDER.condlitvec()}; }
    | LBRACE altheadaggrelemvec[elems] RBRACE                     { $$ = { AggregateFunction::COUNT, true, $elems}; }
    ;

luheadaggregate
    : term[l]          headaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, $l, $u.rel, $u.term)); }
    | term[l] cmp[rel] headaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec($rel, $l, $u.rel, $u.term)); }
    |                  headaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, TermUid(-1), $u.rel, $u.term)); }
    ;

// }}}
// {{{ conjunctions

conjunction
    : literal[lit] COLON litvec[cond] { $$ = { $lit, $cond }; }
    ;

// }}}
// {{{ disjunctions

dsym
    : SEM
    | VBAR
    ;

disjunctionsep
    : disjunctionsep[vec] literal[lit] COMMA                    { $$ = BUILDER.condlitvec($vec, $lit, BUILDER.litvec()); }
    | disjunctionsep[vec] literal[lit] noptcondition[cond] dsym { $$ = BUILDER.condlitvec($vec, $lit, $cond); }
    |                                                           { $$ = BUILDER.condlitvec(); }
    ;

// Note: for simplicity appending first condlit here
disjunction
    : literal[lit] COMMA  disjunctionsep[vec] literal[clit] noptcondition[ccond]                    { $$ = BUILDER.condlitvec(BUILDER.condlitvec($vec, $clit, $ccond), $lit, BUILDER.litvec()); }
    | literal[lit] dsym    disjunctionsep[vec] literal[clit] noptcondition[ccond]                   { $$ = BUILDER.condlitvec(BUILDER.condlitvec($vec, $clit, $ccond), $lit, BUILDER.litvec()); }
    | literal[lit]  COLON nlitvec[cond] dsym disjunctionsep[vec] literal[clit] noptcondition[ccond] { $$ = BUILDER.condlitvec(BUILDER.condlitvec($vec, $clit, $ccond), $lit, $cond); }
    | literal[clit] COLON nlitvec[ccond]                                                            { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $clit, $ccond); }
    ;

// }}}
// }}}
// {{{ statements
// {{{ rules

bodycomma
    : bodycomma[body] literal[lit] COMMA                      { $$ = BUILDER.bodylit($body, $lit); }
    | bodycomma[body] literal[lit] SEM                        { $$ = BUILDER.bodylit($body, $lit); }
    | bodycomma[body] lubodyaggregate[aggr] COMMA             { $$ = lexer->bodyaggregate($body, loc(@aggr), NAF::POS, $aggr); }
    | bodycomma[body] lubodyaggregate[aggr] SEM               { $$ = lexer->bodyaggregate($body, loc(@aggr), NAF::POS, $aggr); }
    | bodycomma[body] NOT[l] lubodyaggregate[aggr] COMMA      { $$ = lexer->bodyaggregate($body, loc(@aggr + @l), NAF::NOT, $aggr); }
    | bodycomma[body] NOT[l] lubodyaggregate[aggr] SEM        { $$ = lexer->bodyaggregate($body, loc(@aggr + @l), NAF::NOT, $aggr); }
    | bodycomma[body] NOT[l] NOT lubodyaggregate[aggr] COMMA  { $$ = lexer->bodyaggregate($body, loc(@aggr + @l), NAF::NOTNOT, $aggr); }
    | bodycomma[body] NOT[l] NOT lubodyaggregate[aggr] SEM    { $$ = lexer->bodyaggregate($body, loc(@aggr + @l), NAF::NOTNOT, $aggr); }
    | bodycomma[body] conjunction[conj] SEM                   { $$ = BUILDER.conjunction($body, loc(@conj), $conj.first, $conj.second); }
    |                                                         { $$ = BUILDER.body(); }
    ;

bodydot
    : bodycomma[body] literal[lit] DOT                      { $$ = BUILDER.bodylit($body, $lit); }
    | bodycomma[body] lubodyaggregate[aggr] DOT             { $$ = lexer->bodyaggregate($body, loc(@aggr), NAF::POS, $aggr); }
    | bodycomma[body] NOT[l] lubodyaggregate[aggr] DOT      { $$ = lexer->bodyaggregate($body, loc(@aggr + @l), NAF::NOT, $aggr); }
    | bodycomma[body] NOT[l] NOT lubodyaggregate[aggr] DOT  { $$ = lexer->bodyaggregate($body, loc(@aggr + @l), NAF::NOTNOT, $aggr); }
    | bodycomma[body] conjunction[conj] DOT                 { $$ = BUILDER.conjunction($body, loc(@conj), $conj.first, $conj.second); }
    ;

head
    : literal[lit]            { $$ = BUILDER.headlit($lit); }
    | disjunction[elems]      { $$ = BUILDER.disjunction(loc(@$), $elems); }
    | luheadaggregate[aggr]   { $$ = lexer->headaggregate(loc(@$), $aggr); }
    ;

statement
    : head[hd] DOT            { BUILDER.rule(loc(@$), $hd); }
    | head[hd] IF bodydot[bd] { BUILDER.rule(loc(@$), $hd, $bd); }
    | IF bodydot[bd]          { BUILDER.rule(loc(@$), BUILDER.headlit(BUILDER.boollit(loc(@$), false)), $bd); }
    | IF DOT                  { BUILDER.rule(loc(@$), BUILDER.headlit(BUILDER.boollit(loc(@$), false)), BUILDER.body()); }
    ;

// }}}
// {{{ optimization

optimizetuple
	: COMMA ntermvec[vec] { $$ = $vec; }
	|                     { $$ = BUILDER.termvec(); }
	;

optimizeweight
	: term[w] AT term[p] { $$ = {$w, $p}; }
	| term[w]            { $$ = {$w, BUILDER.term(loc(@$), Value(0))}; }
	;

optimizelitvec
    : literal[lit]                          { $$ = BUILDER.bodylit(BUILDER.body(), $lit); }
    | optimizelitvec[bd] COMMA literal[lit] { $$ = BUILDER.bodylit($bd, $lit); }
    ;

optimizecond
    : COLON optimizelitvec[bd] { $$ = $bd; }
    | COLON                    { $$ = BUILDER.body(); }
    |                          { $$ = BUILDER.body(); }
    ;

statement
    : WIF bodydot[bd] LBRACK optimizeweight[w] optimizetuple[t] RBRACK { BUILDER.optimize(loc(@$), $w.first, $w.second, $t, $bd); }
    | WIF         DOT LBRACK optimizeweight[w] optimizetuple[t] RBRACK { BUILDER.optimize(loc(@$), $w.first, $w.second, $t, BUILDER.body()); }
    ;

maxelemlist
    :                 optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(loc(@$), BUILDER.term(loc(@w), UnOp::NEG, $w.first), $w.second, $t, $bd); }
    | maxelemlist SEM optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(loc(@$), BUILDER.term(loc(@w), UnOp::NEG, $w.first), $w.second, $t, $bd); }
    ;

minelemlist
    :                 optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(loc(@$), $w.first, $w.second, $t, $bd); }
    | minelemlist SEM optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(loc(@$), $w.first, $w.second, $t, $bd); }
    ;

statement
    : MINIMIZE LBRACE RBRACE DOT
    | MAXIMIZE LBRACE RBRACE DOT
    | MINIMIZE LBRACE minelemlist RBRACE DOT
    | MAXIMIZE LBRACE maxelemlist RBRACE DOT
    ;

// }}}
// {{{ visibility

statement
    : SHOWSIG[sig]                   { BUILDER.showsig(loc(@$), FWSignature($sig)); }
    | SHOW DOT                       { BUILDER.showsig(loc(@$), FWSignature("", 0)); }
    | SHOW term[t] COLON bodydot[bd] { BUILDER.show(loc(@$), $t, $bd); }
    | SHOW term[t] DOT               { BUILDER.show(loc(@$), $t, BUILDER.body()); }
    ;

// }}}
// {{{ constants

statement
    : CONST identifier[uid] ASSIGN constterm[rhs] DOT {  BUILDER.define(loc(@$), $uid, $rhs); }
    ;

// }}}
// {{{ lua

statement
    : LUA DOT { throw std::logic_error("TODO: lua support not implemented yet"); }
    ;

// }}}
// {{{ modules

// TODO: work in progress
/*
parameter
    : constterm
    | constterm COLON constterm
    ;

duration
    : AT constterm
    | AT constterm COLON constterm
    | 
    ;

lifetime
    : LBRACK constterm RBRACK
    | 
    ;

statement
    : VOLATILE parameter duration lifetime DOT
    | CUMULATIVE parameter duration DOT
    | BASE parameter duration DOT
    | BASE DOT
    ;
*/

// }}} 
// {{{ include

statement
    : INCLUDE[start] STRING[file] DOT[end] { lexer->include($file, @start + @end); }
    ;

// }}}
// {{{ external

// TODO: work in progress
/*
statement
    : EXTERNAL identifier SLASH NUMBER DOT
    | EXTERNAL atom optcondition DOT
    ;
*/

// }}}
// {{{ forget

// TODO: how to do this one ...

// }}}
// }}}

