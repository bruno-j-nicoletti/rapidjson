// Tencent is pleased to support the open source community by making RapidJSON available.
//
// Copyright (C) 2015 THL A29 Limited, a Tencent company, and Milo Yip. All rights reserved.
//
// Licensed under the MIT License (the "License"); you may not use this file except
// in compliance with the License. You may obtain a copy of the License at
//
// http://opensource.org/licenses/MIT
//
// Unless required by applicable law or agreed to in writing, software distributed
// under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

#ifndef RAPIDJSON_TOKENISER_H_
#define RAPIDJSON_TOKENISER_H_

/*! \file reader.h */

#include "rapidjson.h"
#include "encodings.h"
#include "reader.h"
#include "internal/meta.h"
#include "internal/stack.h"
#include "internal/strtod.h"

#if defined(RAPIDJSON_SIMD) && defined(_MSC_VER)
#include <intrin.h>
#pragma intrinsic(_BitScanForward)
#endif
#ifdef RAPIDJSON_SSE42
#include <nmmintrin.h>
#elif defined(RAPIDJSON_SSE2)
#include <emmintrin.h>
#endif

#ifdef _MSC_VER
RAPIDJSON_DIAG_PUSH
RAPIDJSON_DIAG_OFF(4127)  // conditional expression is constant
RAPIDJSON_DIAG_OFF(4702)  // unreachable code
#endif

#ifdef __GNUC__
RAPIDJSON_DIAG_PUSH
RAPIDJSON_DIAG_OFF(effc++)
#endif

#include "error/error.h" // ParseErrorCode, ParseResult

RAPIDJSON_NAMESPACE_BEGIN

////////////////////////////////////////////////////////////////////////////////
/// struct that represents a type of JSON token encountered and its value
struct Token {
public :
  /// the kind of tokens we can encounter
  enum Type {
    eNull,
    eBool,
    eInt32,
    eUInt32,
    eInt64,
    eUInt64,
    eDouble,
    eString,
    eKey,
    eStartObject,
    eEndObject,
    eStartArray,
    eEndArray,
    eFinished,
    eFailed
  };

  Token(Type t)
    : type_(t)
  {
  }

  Token(bool value)
    : type_(eBool)
  {
    value_.vBool = value;
  }

  Token(int32_t value)
    : type_(eInt32)
  {
    value_.vInt32 = value;
  }

  Token(uint32_t value)
    : type_(eUInt32)
  {
    value_.vUInt32 = value;
  }

  Token(int64_t value)
    : type_(eInt64)
  {
    value_.vInt64 = value;
  }

  Token(uint64_t value)
    : type_(eUInt64)
  {
    value_.vUInt64 = value;
  }

  Token(double value)
    : type_(eDouble)
  {
    value_.vDouble = value;
  }

  Token(Type t, const char *s, SizeType l)
    : type_(t)
  {
    value_.vStr.str = s;
    value_.vStr.length = l;
  }

  Type type() const { return type_;}

  operator bool() { return value_.vBool; }

  operator int32_t() { return value_.vInt32; }
  operator uint32_t() { return value_.vUInt32; }
  operator int64_t() { return value_.vInt64; }
  operator uint64_t() { return value_.vUInt64; }
  operator double() { return value_.vDouble; }



protected :
  Type type_;

  union value_t {
    bool            vBool;
    int32_t         vInt32;
    uint32_t        vUInt32;
    int64_t         vInt64;
    uint64_t        vUInt64;
    double          vDouble;
    struct str_t {
      const char *str;
      SizeType length;
    } vStr;
  } value_;
};


///////////////////////////////////////////////////////////////////////////////
// Tokeniser

//! SAX-style JSON parser. Use \ref Reader for UTF8 encoding and default allocator.
/*! Tokeniser parses JSON text from a stream, and send events synchronously to an
    object implementing Handler concept.

    It needs to allocate a stack for storing a single decoded string during
    non-destructive parsing.

    For in-situ parsing, the decoded string is directly written to the source
    text string, no temporary buffer is required.

    A Tokeniser object can be reused for parsing multiple JSON text.

    \tparam SourceEncoding Encoding of the input stream.
    \tparam TargetEncoding Encoding of the parse output.
    \tparam StackAllocator Allocator type for stack.
*/
template <typename InputStream, typename SourceEncoding = UTF8<>, typename TargetEncoding = UTF8<>, int ParseFlags = RAPIDJSON_PARSE_DEFAULT_FLAGS, typename StackAllocator = CrtAllocator>
class Tokeniser {
public:
    typedef typename SourceEncoding::Ch Ch; //!< SourceEncoding character type

    //! Constructor.
    /*! \param stackAllocator Optional allocator for allocating stack memory. (Only use for non-destructive parsing)
        \param stackCapacity stack capacity in bytes for storing a single decoded string.  (Only use for non-destructive parsing)
    */
  Tokeniser(InputStream& is,
                   StackAllocator* stackAllocator = 0,
                   size_t stackCapacity = kDefaultStackCapacity)
    : stack_(stackAllocator, stackCapacity)
    , parseResult_()
    , stream_(is)
    {
      SkipWhitespace(stream_);
      if (stream_.Peek() == '\0') {
        RAPIDJSON_PARSE_ERROR(kParseErrorDocumentEmpty, stream_.Tell());
      }
    }

    //! Fetch the next JSON token from the stream.
    Token Get()
    {
      if(parseResult_.IsError()) return Token(Token::eFailed);

      SkipWhitespace(stream_);
      switch (stream_.Peek()) {
      case '\0' : stream_.Take(); return Token(Token::eFinished);
      case ',' : stream_.Take(); return Get();
      case '{'  : stream_.Take(); return Token(Token::eStartObject);
      case '}'  : stream_.Take(); return Token(Token::eEndObject);
      case '['  : stream_.Take(); return Token(Token::eStartArray);
      case ']'  : stream_.Take(); return Token(Token::eEndArray);
      case 'n'  : return ParseNull();
      case 't'  : return ParseTrue();
      case 'f'  : return ParseFalse();
      case '"'  : return ParseString();
      default   : return ParseNumber();
      }
    }

    //! Unget the given token and push it back on the stream
    //void UnGet(const Token &tok);

    //! Whether a parse error has occured in the last parsing.
    bool HasParseError() const { return parseResult_.IsError(); }

    //! Get the \ref ParseErrorCode of last parsing.
    ParseErrorCode GetParseErrorCode() const { return parseResult_.Code(); }

    //! Get the position of last parsing error in input, 0 otherwise.
    size_t GetErrorOffset() const { return parseResult_.Offset(); }

protected:
    void SetParseError(ParseErrorCode code, size_t offset) { parseResult_.Set(code, offset); }

private:
    // Prohibit copy constructor & assignment operator.
    Tokeniser(const Tokeniser&);
    Tokeniser& operator=(const Tokeniser&);

    void ClearStack() { stack_.Clear(); }

    // clear stack on any exit from ParseStream, e.g. due to exception
    struct ClearStackOnExit {
        explicit ClearStackOnExit(Tokeniser& r) : r_(r) {}
        ~ClearStackOnExit() { r_.ClearStack(); }
    private:
        Tokeniser& r_;
        ClearStackOnExit(const ClearStackOnExit&);
        ClearStackOnExit& operator=(const ClearStackOnExit&);
    };

    Token ParseNull() {
        RAPIDJSON_ASSERT(stream_.Peek() == 'n');
        stream_.Take();

        if (stream_.Take() == 'u' && stream_.Take() == 'l' && stream_.Take() == 'l') {
          return Token(Token::eNull);
        }
        else {
            RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorValueInvalid, stream_.Tell() - 1);
            return Token(Token::eFailed);
        }
    }

    Token ParseTrue() {
        RAPIDJSON_ASSERT(stream_.Peek() == 't');
        stream_.Take();

        if (stream_.Take() == 'r' && stream_.Take() == 'u' && stream_.Take() == 'e') {
            return Token(true);
        }
        else {
            RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorValueInvalid, stream_.Tell() - 1);
            return Token(Token::eFailed);
        }
    }

    Token ParseFalse() {
        RAPIDJSON_ASSERT(stream_.Peek() == 'f');
        stream_.Take();

        if (stream_.Take() == 'a' && stream_.Take() == 'l' && stream_.Take() == 's' && stream_.Take() == 'e') {
            return Token(false);
        }
        else {
            RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorValueInvalid, stream_.Tell() - 1);
            return Token(Token::eFailed);
        }
    }

    unsigned ParseHex4() {
        unsigned codepoint = 0;
        for (int i = 0; i < 4; i++) {
            Ch c = stream_.Take();
            codepoint <<= 4;
            codepoint += static_cast<unsigned>(c);
            if (c >= '0' && c <= '9')
                codepoint -= '0';
            else if (c >= 'A' && c <= 'F')
                codepoint -= 'A' - 10;
            else if (c >= 'a' && c <= 'f')
                codepoint -= 'a' - 10;
            else {
                RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorStringUnicodeEscapeInvalidHex, stream_.Tell() - 1);
                RAPIDJSON_PARSE_ERROR_EARLY_RETURN(0);
            }
        }
        return codepoint;
    }

    template <typename CharType>
    class StackStream {
    public:
        typedef CharType Ch;

        StackStream(internal::Stack<StackAllocator>& stack) : stack_(stack), length_(0) {}
        RAPIDJSON_FORCEINLINE void Put(Ch c) {
            *stack_.template Push<Ch>() = c;
            ++length_;
        }
        size_t Length() const { return length_; }
        Ch* Pop() {
            return stack_.template Pop<Ch>(length_);
        }

    private:
        StackStream(const StackStream&);
        StackStream& operator=(const StackStream&);

        internal::Stack<StackAllocator>& stack_;
        SizeType length_;
    };

    // Parse string and generate String event. Different code paths for kParseInsituFlag.
    Token ParseString(bool isKey = false) {
        internal::StreamLocalCopy<InputStream> copy(stream_);
        InputStream& s(copy.s);

        if (ParseFlags & kParseInsituFlag) {
            typename InputStream::Ch *head = s.PutBegin();
            ParseStringToStream<SourceEncoding, SourceEncoding>(s, s);
            RAPIDJSON_PARSE_ERROR_EARLY_RETURN(Token(Token::eFailed));
            size_t length = s.PutEnd(head) - 1;
            RAPIDJSON_ASSERT(length <= 0xFFFFFFFF);
            const typename TargetEncoding::Ch* const str = (typename TargetEncoding::Ch*)head;
            if(isKey) {
              return Token(Token::eKey, str, SizeType(length));
            }
            else {
              return Token(Token::eString, str, SizeType(length));
            }
        }
        else {
            StackStream<typename TargetEncoding::Ch> stackStream(stack_);
            ParseStringToStream<SourceEncoding, TargetEncoding>(s, stackStream);
            RAPIDJSON_PARSE_ERROR_EARLY_RETURN(Token(Token::eFailed));
            SizeType length = static_cast<SizeType>(stackStream.Length()) - 1;
            const typename TargetEncoding::Ch* const str = stackStream.Pop();
            if(isKey) {
              return Token(Token::eKey, str, SizeType(length));
            }
            else {
              return Token(Token::eString, str, SizeType(length));
            }
        }
    }

    // Parse string to an output is
    // This function handles the prefix/suffix double quotes, escaping, and optional encoding validation.
    template<typename SEncoding, typename TEncoding, typename OutputStream>
    RAPIDJSON_FORCEINLINE void ParseStringToStream(InputStream& is, OutputStream& os) {
//!@cond RAPIDJSON_HIDDEN_FROM_DOXYGEN
#define Z16 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        static const char escape[256] = {
            Z16, Z16, 0, 0,'\"', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,'/',
            Z16, Z16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,'\\', 0, 0, 0,
            0, 0,'\b', 0, 0, 0,'\f', 0, 0, 0, 0, 0, 0, 0,'\n', 0,
            0, 0,'\r', 0,'\t', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            Z16, Z16, Z16, Z16, Z16, Z16, Z16, Z16
        };
#undef Z16
//!@endcond

        RAPIDJSON_ASSERT(stream_.Peek() == '\"');
        stream_.Take();  // Skip '\"'

        for (;;) {
            Ch c = stream_.Peek();
            if (c == '\\') {    // Escape
                stream_.Take();
                Ch e = stream_.Take();
                if ((sizeof(Ch) == 1 || unsigned(e) < 256) && escape[(unsigned char)e]) {
                    os.Put(escape[(unsigned char)e]);
                }
                else if (e == 'u') {    // Unicode
                    unsigned codepoint = ParseHex4();
                    RAPIDJSON_PARSE_ERROR_EARLY_RETURN_VOID;
                    if (codepoint >= 0xD800 && codepoint <= 0xDBFF) {
                        // Handle UTF-16 surrogate pair
                        if (stream_.Take() != '\\' || stream_.Take() != 'u')
                            RAPIDJSON_PARSE_ERROR(kParseErrorStringUnicodeSurrogateInvalid, stream_.Tell() - 2);
                        unsigned codepoint2 = ParseHex4();
                        RAPIDJSON_PARSE_ERROR_EARLY_RETURN_VOID;
                        if (codepoint2 < 0xDC00 || codepoint2 > 0xDFFF)
                            RAPIDJSON_PARSE_ERROR(kParseErrorStringUnicodeSurrogateInvalid, stream_.Tell() - 2);
                        codepoint = (((codepoint - 0xD800) << 10) | (codepoint2 - 0xDC00)) + 0x10000;
                    }
                    TEncoding::Encode(os, codepoint);
                }
                else
                    RAPIDJSON_PARSE_ERROR(kParseErrorStringEscapeInvalid, stream_.Tell() - 1);
            }
            else if (c == '"') {    // Closing double quote
                stream_.Take();
                os.Put('\0');   // null-terminate the string
                return;
            }
            else if (c == '\0')
                RAPIDJSON_PARSE_ERROR(kParseErrorStringMissQuotationMark, stream_.Tell() - 1);
            else if ((unsigned)c < 0x20) // RFC 4627: unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
                RAPIDJSON_PARSE_ERROR(kParseErrorStringEscapeInvalid, stream_.Tell() - 1);
            else {
                if (ParseFlags & kParseValidateEncodingFlag ?
                    !Transcoder<SEncoding, TEncoding>::Validate(is, os) :
                    !Transcoder<SEncoding, TEncoding>::Transcode(is, os))
                    RAPIDJSON_PARSE_ERROR(kParseErrorStringInvalidEncoding, stream_.Tell());
            }
        }
    }

    template<typename INPUTSTREAM, bool backup>
    class NumberStream;

    template<typename INPUTSTREAM>
    class NumberStream<INPUTSTREAM, false> {
    public:
        NumberStream(Tokeniser& reader, INPUTSTREAM& s) : is(s) { (void)reader;  }
        ~NumberStream() {}

        RAPIDJSON_FORCEINLINE Ch Peek() const { return is.Peek(); }
        RAPIDJSON_FORCEINLINE Ch TakePush() { return is.Take(); }
        RAPIDJSON_FORCEINLINE Ch Take() { return is.Take(); }
        size_t Tell() { return is.Tell(); }
        size_t Length() { return 0; }
        const char* Pop() { return 0; }

    protected:
        NumberStream& operator=(const NumberStream&);

        INPUTSTREAM& is;
    };

    template<typename INPUTSTREAM>
    class NumberStream<INPUTSTREAM, true> : public NumberStream<INPUTSTREAM, false> {
        typedef NumberStream<INPUTSTREAM, false> Base;
    public:
        NumberStream(Tokeniser& reader, INPUTSTREAM& is) : NumberStream<INPUTSTREAM, false>(reader, is), stackStream(reader.stack_) {}
        ~NumberStream() {}

        RAPIDJSON_FORCEINLINE Ch TakePush() {
            stackStream.Put((char)Base::stream_.Peek());
            return Base::stream_.Take();
        }

        size_t Length() { return stackStream.Length(); }

        const char* Pop() {
            stackStream.Put('\0');
            return stackStream.Pop();
        }

    private:
        StackStream<char> stackStream;
    };

    Token ParseNumber() {
        internal::StreamLocalCopy<InputStream> copy(stream_);
        NumberStream<InputStream, (ParseFlags & kParseFullPrecisionFlag) != 0> s(*this, copy.s);

        // Parse minus
        bool minus = false;
        if (s.Peek() == '-') {
            minus = true;
            s.Take();
        }

        // Parse int: zero / ( digit1-9 *DIGIT )
        unsigned i = 0;
        uint64_t i64 = 0;
        bool use64bit = false;
        int significandDigit = 0;
        if (s.Peek() == '0') {
            i = 0;
            s.TakePush();
        }
        else if (s.Peek() >= '1' && s.Peek() <= '9') {
            i = static_cast<unsigned>(s.TakePush() - '0');

            if (minus)
                while (s.Peek() >= '0' && s.Peek() <= '9') {
                    if (i >= 214748364) { // 2^31 = 2147483648
                        if (i != 214748364 || s.Peek() > '8') {
                            i64 = i;
                            use64bit = true;
                            break;
                        }
                    }
                    i = i * 10 + static_cast<unsigned>(s.TakePush() - '0');
                    significandDigit++;
                }
            else
                while (s.Peek() >= '0' && s.Peek() <= '9') {
                    if (i >= 429496729) { // 2^32 - 1 = 4294967295
                        if (i != 429496729 || s.Peek() > '5') {
                            i64 = i;
                            use64bit = true;
                            break;
                        }
                    }
                    i = i * 10 + static_cast<unsigned>(s.TakePush() - '0');
                    significandDigit++;
                }
        }
        else {
            RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorValueInvalid, s.Tell());
            return Token(Token::eFailed);
        }

        // Parse 64bit int
        bool useDouble = false;
        double d = 0.0;
        if (use64bit) {
            if (minus)
                while (s.Peek() >= '0' && s.Peek() <= '9') {
                     if (i64 >= RAPIDJSON_UINT64_C2(0x0CCCCCCC, 0xCCCCCCCC)) // 2^63 = 9223372036854775808
                        if (i64 != RAPIDJSON_UINT64_C2(0x0CCCCCCC, 0xCCCCCCCC) || s.Peek() > '8') {
                            d = i64;
                            useDouble = true;
                            break;
                        }
                    i64 = i64 * 10 + static_cast<unsigned>(s.TakePush() - '0');
                    significandDigit++;
                }
            else
                while (s.Peek() >= '0' && s.Peek() <= '9') {
                    if (i64 >= RAPIDJSON_UINT64_C2(0x19999999, 0x99999999)) // 2^64 - 1 = 18446744073709551615
                        if (i64 != RAPIDJSON_UINT64_C2(0x19999999, 0x99999999) || s.Peek() > '5') {
                            d = i64;
                            useDouble = true;
                            break;
                        }
                    i64 = i64 * 10 + static_cast<unsigned>(s.TakePush() - '0');
                    significandDigit++;
                }
        }

        // Force double for big integer
        if (useDouble) {
            while (s.Peek() >= '0' && s.Peek() <= '9') {
                if (d >= 1.7976931348623157e307) {// DBL_MAX / 10.0
                    RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorNumberTooBig, s.Tell());
                    return Token(Token::eFailed);
                }
                d = d * 10 + (s.TakePush() - '0');
            }
        }

        // Parse frac = decimal-point 1*DIGIT
        int expFrac = 0;
        size_t decimalPosition;
        if (s.Peek() == '.') {
            s.Take();
            decimalPosition = s.Length();

            if (!(s.Peek() >= '0' && s.Peek() <= '9')) {
                RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorNumberMissFraction, s.Tell());
                return Token(Token::eFailed);
            }

            if (!useDouble) {
#if RAPIDJSON_64BIT
                // Use i64 to store significand in 64-bit architecture
                if (!use64bit)
                    i64 = i;

                while (s.Peek() >= '0' && s.Peek() <= '9') {
                    if (i64 > RAPIDJSON_UINT64_C2(0x1FFFFF, 0xFFFFFFFF)) // 2^53 - 1 for fast path
                        break;
                    else {
                        i64 = i64 * 10 + static_cast<unsigned>(s.TakePush() - '0');
                        --expFrac;
                        if (i64 != 0)
                            significandDigit++;
                    }
                }

                d = (double)i64;
#else
                // Use double to store significand in 32-bit architecture
                d = use64bit ? (double)i64 : (double)i;
#endif
                useDouble = true;
            }

            while (s.Peek() >= '0' && s.Peek() <= '9') {
                if (significandDigit < 17) {
                    d = d * 10.0 + (s.TakePush() - '0');
                    --expFrac;
                    if (d > 0.0)
                        significandDigit++;
                }
                else
                    s.TakePush();
            }
        }
        else
            decimalPosition = s.Length(); // decimal position at the end of integer.

        // Parse exp = e [ minus / plus ] 1*DIGIT
        int exp = 0;
        if (s.Peek() == 'e' || s.Peek() == 'E') {
            if (!useDouble) {
                d = use64bit ? i64 : i;
                useDouble = true;
            }
            s.Take();

            bool expMinus = false;
            if (s.Peek() == '+')
                s.Take();
            else if (s.Peek() == '-') {
                s.Take();
                expMinus = true;
            }

            if (s.Peek() >= '0' && s.Peek() <= '9') {
                exp = s.Take() - '0';
                if (expMinus) {
                    while (s.Peek() >= '0' && s.Peek() <= '9') {
                        exp = exp * 10 + (s.Take() - '0');
                        if (exp >= 214748364) {                         // Issue #313: prevent overflow exponent
                            while (s.Peek() >= '0' && s.Peek() <= '9')  // Consume the rest of exponent
                                s.Take();
                        }
                    }
                }
                else {  // positive exp
                    int maxExp = 308 - expFrac;
                    while (s.Peek() >= '0' && s.Peek() <= '9') {
                        exp = exp * 10 + (s.Take() - '0');
                        if (exp > maxExp) {
                            RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorNumberTooBig, s.Tell());
                            return Token(Token::eFailed);
                        }
                    }
                }
            }
            else {
                RAPIDJSON_PARSE_ERROR_NORETURN(kParseErrorNumberMissExponent, s.Tell());
                return Token(Token::eFailed);
            }

            if (expMinus)
                exp = -exp;
        }

        // Finish parsing, call event according to the type of number.
        size_t length = s.Length();
        const char* decimal = s.Pop();  // Pop stack no matter if it will be used or not.

        if (useDouble) {
            int p = exp + expFrac;
            if (ParseFlags & kParseFullPrecisionFlag)
                d = internal::StrtodFullPrecision(d, p, decimal, length, decimalPosition, exp);
            else
                d = internal::StrtodNormalPrecision(d, p);
            return Token(d);
        }
        else {
            if (use64bit) {
                if (minus)
                    return Token(static_cast<int64_t>(~i64 + 1));
                else
                    return Token(static_cast<uint64_t>(i64));
            }
            else {
                if (minus)
                    return Token(static_cast<int32_t>(~i + 1));
                else
                    return Token(static_cast<uint32_t>(i));
            }
        }
    }


    static const size_t kDefaultStackCapacity = 256;    //!< Default stack capacity in bytes for storing a single decoded string.
    internal::Stack<StackAllocator> stack_;  //!< A stack for storing decoded string temporarily during non-destructive parsing.
    ParseResult parseResult_;
    InputStream stream_;
}; // class Tokeniser

RAPIDJSON_NAMESPACE_END

#ifdef __GNUC__
RAPIDJSON_DIAG_POP
#endif

#ifdef _MSC_VER
RAPIDJSON_DIAG_POP
#endif

#endif // RAPIDJSON_READER_H_
