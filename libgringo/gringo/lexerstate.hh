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

#ifndef _GRINGO_LEXERSTATE_HH
#define _GRINGO_LEXERSTATE_HH

#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <cassert>
#include <memory>

namespace Gringo {

// {{{ declaration of LexerState

class LexerState {
private:
    struct State;

public:
    LexerState();
    void start();
    bool eof() const;
    bool push(std::string const &filename);
    bool push(std::string const &file, std::unique_ptr<std::istream> in);
    void pop();
    bool empty() const;
    std::string string(int start = 0, int end = 0);
    void step(char s);
    void step();
    int integer() const;
    int line() const;
    int column() const;
    std::string const &filename() const;
    char *&cursor();
    char *&marker();
    char const *limit() const;
    void fill(size_t n);
    void seek(int offset);
private:
    State const &state() const;
    State &state();
private:
    std::vector<State> states_;
};

// }}}
// {{{ declaration of LexerState::State

struct LexerState::State {
    State(std::string const &filename);
    State(State &&) = default;
    void fill(size_t n);
    void step();
    void start();
    ~State();

    std::unique_ptr<std::istream> in_;
    std::string const &filename_;
    size_t bufmin_;
    size_t bufsize_;
    char *buffer_;
    char *start_;
    char *offset_;
    char *cursor_;
    char *limit_;
    char *marker_;
    char *eof_;
    int line_;
};

// }}}

// {{{ defintion of LexerState::State

inline LexerState::State::State(std::string const &filename) 
    : filename_(filename)
    , bufmin_(4096), bufsize_(0)
    , buffer_(0), start_(0), offset_(0)
    , cursor_(0), limit_(0), marker_(0)
    , eof_(0), line_(1) { }

inline void LexerState::State::fill(size_t n) {
    if(eof_) return;
    if(start_ > buffer_) {
        size_t shift = start_ - buffer_;
        memmove(buffer_, start_, limit_ - start_);
        start_  = buffer_;
        offset_-= shift;
        marker_-= shift;
        limit_ -= shift;
        cursor_-= shift;
    }
    size_t inc = n < bufmin_ ? bufmin_ : n;
    if(bufsize_ < inc + (limit_ - buffer_)) {
        bufsize_  = inc + (limit_ - buffer_);
        char *buf = (char*)realloc(buffer_, bufsize_ * sizeof(char));
        start_    = buf + (start_ - buffer_);
        cursor_   = buf + (cursor_ - buffer_);
        limit_    = buf + (limit_ - buffer_);
        marker_   = buf + (marker_ - buffer_);
        offset_   = buf + (offset_ - buffer_);
        buffer_   = buf;
    }
    in_->read(limit_, inc);
    limit_+= in_->gcount();
    if(size_t(in_->gcount()) < inc) {
        eof_ = limit_;
        *eof_++ = '\n';
    }
}

inline void LexerState::State::step() {
    offset_ = cursor_;
    line_++;
}

inline void LexerState::State::start() { 
    start_ = cursor_;
}

inline LexerState::State::~State() {
    if(buffer_) free(buffer_); 
}

// }}}
// {{{ defintion of LexerState

inline LexerState::LexerState() = default;

inline void LexerState::start() {
    state().start(); 
}

inline bool LexerState::eof() const {
    return state().cursor_ == state().eof_;
}

inline bool LexerState::push(std::string const &file, std::unique_ptr<std::istream> in) {
    states_.emplace_back(file);
    state().in_ = std::move(in);
    return true;
}

inline bool LexerState::push(std::string const &file) {
    if (file == "-") {
        states_.emplace_back(file);
        state().in_.reset(new std::istream(std::cin.rdbuf(0)));
        return true;
    }
    else {
        std::unique_ptr<std::ifstream> ifs(new std::ifstream(file));
        if(ifs->is_open()) {
            states_.emplace_back(file);
            state().in_.reset(ifs.release());
            return true;
        }
        else { return false; }
    }
}

inline void LexerState::pop() {
    states_.pop_back();
}

inline bool LexerState::empty() const {
    return states_.empty();
}

inline std::string LexerState::string(int start, int end) {
    return std::string(state().start_ + start, state().cursor_ - end);
}

inline void LexerState::step(char s) {
    for (char *c = state().start_; c != state().cursor_; c++)
    {
        if (*c == s) { step(); }
    }
}

inline void LexerState::step() {
    state().step();
}

inline int LexerState::integer() const {
    int r = 0;
    int s = 0;
    if(*state().start_ == '-') s = 1;
    for(char *i = state().start_ + s; i != state().cursor_; i++) {
        r *= 10;
        r += *i - '0';
    }
    return s ? -r : r;
}

inline int LexerState::line() const {
    return state().line_;
}

inline int LexerState::column() const {
    return state().cursor_ - state().offset_ + 1;
}

inline std::string const &LexerState::filename() const {
    return state().filename_;
} 

inline char *&LexerState::cursor() {
    return state().cursor_;
}

inline char *&LexerState::marker() {
    return state().marker_;
}

inline char const *LexerState::limit() const {
    return state().limit_;
}

inline void LexerState::fill(size_t n) {
    state().fill(n);
}

inline LexerState::State const &LexerState::state() const {
    assert(!empty());
    return states_.back();
}

inline LexerState::State &LexerState::state() {
    assert(!empty());
    return states_.back();
}

inline void LexerState::seek(int offset) {
    state().cursor_ = state().start_ + offset;
    state().marker_ = state().cursor_;
}

// }}}

} // namespace Gringo

#endif // _GRINGO_LEXERSTATE_HH
