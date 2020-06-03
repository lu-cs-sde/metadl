/* 3.4 Line Terminators */
LineTerminator = \n|\r|\r\n
InputCharacter = [^\r\n]

/* 3.6 White Space */
WhiteSpace = [ ] | \t | \f | {LineTerminator}

/* 3.7 Comments */
Comment = {TraditionalComment} | {EndOfLineComment}

DocumentationComment = "/**" ([^*/] | "*"+ [^*/]) ([^*] | "*"+ [^*/])* "*"+ "/"
TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment = "//" {InputCharacter}* {LineTerminator}?

/* Modified numeric literal parsing for Java 7.
 * Accept most things that start with a digit 0-9
 * and contains any combination of the legal numeric
 * literal component characters
 *
 * component characters (uppercase also accepted):
 * 0-9  decimal and hex digits
 * a-f  hex digits
 * x    hex specifier
 * e    decimal exponent
 * p    hex exponent
 * .    floating point
 * -+   floating point exponent sign (only after exponent character)
 *
 * We must only scan digits after a plus or minus, the rest is
 * not as sensitive.
 *
 * See JLSv3 $3.101 http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.1
 */
AnyDigit = [_0-9a-fA-FxXlL]
NonHexDigit = [_0-9]
NumericLiteral = [0-9] {NonHexDigit}* {FloatExponentPart}? {FloatSuffix}?
               | [0-9] {AnyDigit}* {HexExponentPart}? {FloatSuffix}?
               | [0-9] {AnyDigit}* \. {AnyDigit}* {ExponentPart}? {FloatSuffix}?
               | \. [0-9] {AnyDigit}* {ExponentPart}? {FloatSuffix}?
ExponentPart = {HexExponentPart} | {FloatExponentPart}
HexExponentPart = [pP] [_0-9]+
                | [pP] [+-] [0-9] [_0-9]*
FloatExponentPart = [eE] [_0-9fF]*
             | [eE] [+-] [0-9] [_0-9]*
FloatSuffix = [fFdD]

/* 3.10.4 Character Literals */
SingleCharacter = [^\r\n\'\\]

/* 3.10.5 String Literals */
StringCharacter = [^\r\n\"\\]

/* 3.10.6 Escape Sequences for Character and String Literals */
OctalEscape = \\ {OctalDigit}
            | \\ {OctalDigit} {OctalDigit}
            | \\  {ZeroToThree} {OctalDigit} {OctalDigit}
OctalDigit = [0-7]
ZeroToThree = [0-3]

