import {
  andThen,
  between,
  choice,
  createParserForwardedToRef,
  many,
  many1,
  mapParser,
  opt,
  orElse,
  parseCharacter,
  parseDigit,
  parseString,
  runParser,
  satisfy,
  sepBy,
  sequence,
  setParserLabel
} from "./main.js";

interface JObject {
  [key: string]: JValue;
}

type JValue =
  | null
  | boolean
  | string
  | number
  | JObject
  | JValue[]

const [jValue, jValueRef] = createParserForwardedToRef<JValue>();

const jNull = mapParser(
  parseString('null'),
  () => null,
);

const jBool = orElse(
  mapParser(parseString('true'), () => true),
  mapParser(parseString('false'), () => false),
);

const jUnescapedChar = satisfy(x => x !== '\\' && x !== '"', 'char');
const escapedCharMap: Record<string, string> = {
  "\\\"": '\"',      // quote
  "\\\\": '\\',      // reverse solidus
  "\\/": '/',        // solidus
  "\\b": '\b',       // backspace
  "\\f": '\f',       // formfeed
  "\\n": '\n',       // newline
  "\\r": '\r',       // cr
  "\\t": '\t',       // tab
};

const jEscapedChar = choice(
  Object.entries(escapedCharMap)
    .map((([key, val]) => mapParser(parseString(key), () => val))),
);

const digits = Array(10).fill(0).map((_, i) => String.fromCharCode('0'.charCodeAt(0) + i))
const lowerCaseLetters = Array(26).fill(0).map((_, i) => String.fromCharCode('a'.charCodeAt(0) + i))
const upperCaseLetters = Array(26).fill(0).map((_, i) => String.fromCharCode('A'.charCodeAt(0) + i))

const quotedString = between(
  parseCharacter('"'),
  mapParser(
    many(
      choice([jUnescapedChar, jEscapedChar])
    ),
    x => x.join('')
  ),
  parseCharacter('"'),
);

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('quotedString', () => {
    const result = runParser(quotedString, '"abc"');
    expect(result).toEqual({
      type: 'Success',
      val: 'abc',
      rest: {
        lines: ['"abc"'],
        position: {
          line: 0,
          column: 5,
        },
      },
    });
  });
}

const jString = quotedString;

export const parseInteger = setParserLabel(
  mapParser(
    sequence<unknown>([
      opt(parseCharacter('-')),
      many1(parseDigit),
    ]), ([sign, digits]) => Number([sign, ...digits as string[]].join(''))
  ), 'integer');

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('parseInteger-negative', () => {
    const result = runParser(parseInteger, '-33abc');
    expect(result).toEqual({
      type: 'Success',
      val: -33,
      rest: {
        lines: ['-33abc'],
        position: {
          line: 0,
          column: 3,
        },
      },
    });
  });

  test('parseInteger-positive', () => {
    const result = runParser(parseInteger, '12abc');
    expect(result).toEqual({
      type: 'Success',
      val: 12,
      rest: {
        lines: ['12abc'],
        position: {
          line: 0,
          column: 2,
        },
      },
    });
  });
}

export const parseFloat = setParserLabel(
  mapParser<any, number>(
    sequence<unknown>([
      opt(parseCharacter('-')),
      many1(parseDigit),
      sequence<unknown>([
        parseCharacter('.'),
        many1(parseDigit),
      ]),
    ]), ([sign, digitsBeforeDot, [dot, digitsAfterDot]]) => Number([sign, ...digitsBeforeDot as string[], ...[dot, ...digitsAfterDot]].join(''))
  ), 'float');

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('parseFloat-negative', () => {
    const result = runParser(parseFloat, '-33.22');
    expect(result).toEqual({
      type: 'Success',
      val: -33.22,
      rest: {
        lines: ['-33.22'],
        position: {
          line: 0,
          column: 6,
        },
      },
    });
  });

  test('parseFloat-positive', () => {
    const result = runParser(parseFloat, '123.45');
    expect(result).toEqual({
      type: 'Success',
      val: 123.45,
      rest: {
        lines: ['123.45'],
        position: {
          line: 0,
          column: 6,
        },
      },
    });
  });
}

const jNumber = orElse(parseInteger, parseFloat);

const jArray = between(
  parseCharacter('['),
  mapParser(opt(sepBy(jValue, parseCharacter(','))), x => x ?? []),
  parseCharacter(']'),
);

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('jArray-empty', () => {
    const result = runParser(jArray, '[]');
    expect(result).toEqual({
      type: 'Success',
      val: [],
      rest: {
        lines: ['[]'],
        position: {
          line: 0,
          column: 2,
        },
      },
    });
  });

  test('jArray-single-element', () => {
    const result = runParser(jArray, '[1]');
    expect(result).toEqual({
      type: 'Success',
      val: [1],
      rest: {
        lines: ['[1]'],
        position: {
          line: 0,
          column: 3,
        },
      },
    });
  });

  test('jArray-two-element', () => {
    const result = runParser(jArray, '[1,2]');
    expect(result).toEqual({
      type: 'Success',
      val: [1, 2],
      rest: {
        lines: ['[1,2]'],
        position: {
          line: 0,
          column: 5,
        },
      },
    });
  });
}

const keyValue = mapParser(
  andThen(quotedString, andThen(parseCharacter(':'), jValue)),
  (x) => ({[x[0]]: x[1][1]})
);

const jObject = setParserLabel(
  between(
    parseCharacter('{'),
    mapParser(opt(sepBy(keyValue, parseCharacter(','))), x => Object.assign({}, ...(x ?? []))),
    parseCharacter('}'),
  ),
  'jObject',
);

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('jObject-empty', () => {
    const result = runParser(jObject, '{}');
    expect(result).toEqual({
      type: 'Success',
      val: {},
      rest: {
        lines: ['{}'],
        position: {
          line: 0,
          column: 2,
        },
      },
    });
  });

  test('jObject-single-element', () => {
    const result = runParser(jObject, '{"a":1}');
    expect(result).toEqual({
      type: 'Success',
      val: {
        a: 1,
      },
      rest: {
        lines: ['{"a":1}'],
        position: {
          line: 0,
          column: 7,
        },
      },
    });
  });

  test('jObject-two-element', () => {
    const result = runParser(jObject, '{"a":1,"b":2}');
    expect(result).toEqual({
      type: 'Success',
      val: {
        a: 1,
        b: 2,
      },
      rest: {
        lines: ['{"a":1,"b":2}'],
        position: {
          line: 0,
          column: 13,
        },
      },
    });
  });
}

jValueRef.current = choice([
  jNull,
  jBool,
  jString,
  jNumber,
  jArray,
  jObject,
]);

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('JSON parser', () => {
    const result = runParser(jValue, '[1,{"A":2},null,"3"]');
    expect(result).toEqual({
      type: 'Success',
      val: [1, {"A": 2}, null, "3"],
      rest: {
        lines: ['[1,{"A":2},null,"3"]'],
        position: {
          line: 0,
          column: 20,
        },
      },
    });
  });
}
