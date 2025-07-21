/*
 * TLineReader.h
 *
 *  Created on: Sep 26, 2022
 *      Author: Andreas
 */

#ifndef CORE_FILES_TLINEREADER_H
#define CORE_FILES_TLINEREADER_H

#include <iterator>
#include <memory>
#include <string_view>

#include "coretools/Containers/TView.h"
#include "coretools/Files/TReader.h"

namespace coretools {

class TLineReader {
	static constexpr size_t _minSize = 1024;

	mutable std::unique_ptr<char[]> _buffer;
	mutable std::unique_ptr<TReader> _reader{new TNoReader};
	mutable size_t _size  = 0;
	mutable size_t _pos   = 0;
	mutable size_t _len   = 0;
	mutable bool _hasLine = false;

	void _readBuffer() const {
		if (_reader->eof()) return;
		if (!_buffer) {
			_buffer.reset(new char[_minSize]);
			_size = _minSize;
		}

		TView<char> view(_buffer.get(), _size);
		view.remove_prefix(_len); // we already added _len chars to buffer from overlap

		auto count = _len + _reader->fill(view);
		auto eolIt = std::find(view.begin(), view.end(), '\n');

		// buffer to small to hold at least one line
		while (count == _size && eolIt == view.end()) {
			// resize buffer
			const size_t newSize = 2 * _size;
			char *newBuffer      = new char[newSize];
			std::copy(_buffer.get(), _buffer.get() + _size, newBuffer);
			_buffer.reset(newBuffer);

			view = TView<char>(_buffer.get(), newSize);
			view.remove_prefix(_size);
			_size = newSize;

			// append from file
			count += _reader->fill(view);
			eolIt  = std::find(view.begin(), view.end(), '\n');
		}

		if (count < _size) { // EOF
			_size = count;
		}
		_pos = 0; // reset
		_len = std::min<size_t>(_size, std::distance(_buffer.get(), eolIt));
	}

	void _readLine() const {
		if (_hasLine || _pos > _size) return; // already has line or eof

		TView<char> view(_buffer.get(), _size);
		view.remove_prefix(_pos);

		const auto eolIt = std::find(view.begin(), view.end(), '\n');
		if (eolIt == view.end()) {
			std::copy(view.begin(), view.end(), _buffer.get()); // copy overlap to begin
			_len = _size - _pos;                                // length of line this far
			_readBuffer();
		} else {
			_len = std::distance(view.begin(), eolIt);
		}
		_hasLine = true;
	}

public:
	TLineReader() = default;
	TLineReader(TReader *reader) : _reader(reader) {}
	TLineReader(std::string_view Filename) : _reader(makeReader(Filename)) {}

	void open(TReader *reader) {
		user_assert(!isOpen(), "File '", reader->name(), "' is already open!");

		_reader.reset(reader);
		_pos     = 0;
		_hasLine = false;
	}

	void open(std::string_view Filename) {
		open(makeReader(Filename));
	}

	void close() {
		_reader.reset(new TNoReader);
		_buffer.reset(nullptr);
		_hasLine = false;
		_pos     = 0;
	}

	bool isOpen() const noexcept {
		return _reader->isOpen();
	}

	bool empty() const noexcept {
		_readLine();
		return _pos >= _size;
	}

	std::string_view front() const {
		_readLine();
		return std::string_view(_buffer.get() + _pos, _len);
	}

	void popFront() {
		_readLine(); // finish current line

		_pos += _len + 1; //Don't count \n
		_hasLine = false;
	}

	const std::string &name() const noexcept { return _reader->name(); }

	void setPosition(size_t pos) {
		_hasLine            = false;
		_len                = 0;
		const size_t maxPos = _reader->tell();
		const size_t minPos = maxPos - _size;
		_pos = pos - minPos; // underflow -> big number
		if (_pos >= _size) {
			_reader->seek(pos);
			// reset buffer
			_readBuffer();
		}
	}

	size_t getPosition() const noexcept {
		_readLine();
		return _reader->tell() - _size + _pos;
	}
};

} // namespace coretools

#endif
