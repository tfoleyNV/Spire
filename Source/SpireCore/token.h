// token.h
#ifndef SPIRE_TOKEN_H_INCLUDED
#define SPIRE_TOKEN_H_INCLUDED

#include "../CoreLib/Basic.h"

#include "source-loc.h"

namespace Spire {
namespace Compiler {

using namespace CoreLib::Basic;

enum class TokenType
{
#define TOKEN(NAME, DESC) NAME,
#include "token-defs.h"
};

char const* TokenTypeToString(TokenType type);

enum TokenFlag : unsigned int
{
    AtStartOfLine   = 1 << 0,
    AfterWhitespace = 1 << 1,
};
typedef unsigned int TokenFlags;

class Token
{
public:
	TokenType Type = TokenType::Unknown;
	String Content;
	CodePosition Position;
    TokenFlags flags = 0;
	Token() = default;
	Token(TokenType type, const String & content, int line, int col, int pos, String fileName, TokenFlags flags = 0)
        : flags(flags)
	{
		Type = type;
		Content = content;
		Position = CodePosition(line, col, pos, fileName);
	}
};



}}

#endif
