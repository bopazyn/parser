interface Position {
  line: number;
  column: number;
}

const initialPosition: Position = {line: 0, column: 0};
const incrementLine = (position: Position): Position =>
  ({line: position.line + 1, column: 0});
const incrementColumn = (position: Position): Position =>
  ({line: position.line, column: position.column + 1});

interface InputState {
  lines: string[];
  position: Position;
}

const fromStr = (str: string): InputState => {
  const lines = !str ? [] : str.split(/\n/);
  return ({lines, position: initialPosition});
};

const END_OF_FILE = Symbol('End of file');
const getCurrentLine = (inputState: InputState): string | typeof END_OF_FILE =>
  inputState.lines[inputState.position.line] ?? END_OF_FILE;

const nextCharacter = (inputState: InputState): [InputState, string | null] => {
  if (inputState.position.line >= inputState.lines.length) {
    return [inputState, null];
  }

  const currentLine = getCurrentLine(inputState);
  if (inputState.position.column < (currentLine as string).length) {
    const c = (currentLine as string)[inputState.position.column];
    const newState: InputState = ({position: incrementColumn(inputState.position), lines: inputState.lines});
    return [newState, c];
  }

  const c = '\n';
  const newState: InputState = ({position: incrementLine(inputState.position), lines: inputState.lines});
  return [newState, c];
}

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const readAllChars = (input: InputState): string[] => {
    const [nextInput, c] = nextCharacter(input);
    if (c === null) {
      return [];
    }
    return [c, ...readAllChars(nextInput)];
  }

  test('nextCharacter-empty', () => {
    const inputState = fromStr('');
    const result = readAllChars(inputState);
    expect(result).toEqual([]);
  })

  test('nextCharacter-a', () => {
    const inputState = fromStr('a');
    const result = readAllChars(inputState);
    expect(result).toEqual(['a', '\n']);
  });

  test('nextCharacter-a-b', () => {
    const inputState = fromStr('a\nb');
    const result = readAllChars(inputState);
    expect(result).toEqual(['a', '\n', 'b', '\n']);
  });
}

interface ParserPosition extends Position {
  currentLine: ReturnType<typeof getCurrentLine>;
}

const parserPositionFromInputState = (inputState: InputState): ParserPosition => ({
  currentLine: getCurrentLine(inputState),
  line: inputState.position.line,
  column: inputState.position.column,
})

type ParseResult<T> =
  | { type: 'Success', val: T, rest: InputState }
  | { type: 'Failure', position: ParserPosition, parser: ParserLabel, error: string }

type ParserLabel = string;

interface Parser<T> {
  parserFn: (input: InputState) => ParseResult<T>;
  label: ParserLabel;
}

const setParserLabel = <T, >(parser: Parser<T>, label: ParserLabel): Parser<T> => ({
  ...parser,
  label,
  parserFn: (input) => {
    const res = runParserOnInput(parser, input);
    return res.type === 'Success' ? res : ({...res, parser: label});
  }
});

const satisfy = (predicate: (s: string) => boolean, label: ParserLabel): Parser<string> => ({
  label,
  parserFn: (input) => {
    const [nextInput, c] = nextCharacter(input);
    const position = parserPositionFromInputState(input);
    if (c === null) {
      return {type: 'Failure', position, parser: label, error: 'No more input'};
    }

    if (!predicate(c)) {
      return {type: 'Failure', position, parser: label, error: `Unexpected '${c}'`};
    }
    return {
      type: 'Success',
      val: c, rest: nextInput
    };
  },
})

const parseCharacter = (c: string): Parser<string> =>
  satisfy(x => x === c, `parse ${c}`);

const runParserOnInput = <T, >(parser: Parser<T>, input: InputState) =>
  parser.parserFn(input);

const runParser = <T, >(parser: Parser<T>, str: string) =>
  parser.parserFn(fromStr(str));


if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');

  test('parseCharacter-empty', () => {
    const result = runParser(parseA, '');
    expect(result).toEqual({
      type: 'Failure',
      parser: 'parse a',
      error: 'No more input',
      position: {
        line: 0,
        column: 0,
        currentLine: END_OF_FILE,
      },
    });
  });

  test('parseCharacter-b', () => {
    const result = runParser(parseA, 'b');
    expect(result).toEqual({
      type: 'Failure',
      parser: 'parse a',
      error: `Unexpected 'b'`,
      position: {
        line: 0,
        column: 0,
        currentLine: 'b',
      },
    });
  });

  test('parseCharacter-a', () => {
    const result = runParser(parseA, 'a');
    expect(result).toEqual({
      type: 'Success',
      val: 'a',
      rest: {
        lines: ['a'],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });
}

const bindParser = <T1, T2>(parser: Parser<T1>, func: (arg: T1) => Parser<T2>): Parser<T2> => ({
  label: `unknown`,
  parserFn: (input) => {
    const res = runParserOnInput(parser, input);
    if (res.type === 'Failure') {
      return res;
    }

    return runParserOnInput(func(res.val), res.rest);
  },
});

const andThen = <T1, T2>(parser1: Parser<T1>, parser2: Parser<T2>): Parser<[T1, T2]> => {
  const label = `${parser1.label} && ${parser2.label}`;
  const newParser = bindParser(parser1, r1 =>
    bindParser(parser2, r2 =>
      returnParser([r1, r2] as [T1, T2]),
    ),
  );
  return setParserLabel(newParser, label);
};

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');
  const parseB = parseCharacter('b');
  const parser = andThen(parseA, parseB);

  test('andThen-success', () => {
    const result = runParser(parser, 'ab');
    expect(result).toEqual({
      type: 'Success',
      val: ['a', 'b'],
      rest: {
        lines: ['ab'],
        position: {
          line: 0,
          column: 2,
        },
      },
    });
  });

  test('andThen-first-fails', () => {
    const result = runParser(parser, 'bb');
    expect(result).toEqual({
      type: 'Failure',
      parser: 'parse a && parse b',
      error: `Unexpected 'b'`,
      position: {
        line: 0,
        column: 0,
        currentLine: 'bb',
      },
    });
  });

  test('andThen-second-fails', () => {
    const result = runParser(parser, 'aa');
    expect(result).toEqual({
      type: 'Failure',
      parser: 'parse a && parse b',
      error: `Unexpected 'a'`,
      position: {
        line: 0,
        column: 1,
        currentLine: 'aa',
      },
    });
  });
}

const orElse = <T1, T2>(parser1: Parser<T1>, parser2: Parser<T2>): Parser<T1 | T2> => ({
  label: `${parser1.label} || ${parser2.label}`,
  parserFn: (input) => {
    const res = runParserOnInput(parser1, input);
    return res.type === 'Success' ? res : runParserOnInput(parser2, input);
  },
})

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');
  const parseB = parseCharacter('b');
  const parser = orElse(parseA, parseB);

  test('orElse-first-succeed', () => {
    const result = runParser(parser, 'ab');
    expect(result).toEqual({
      type: 'Success',
      val: 'a',
      rest: {
        lines: ['ab'],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });

  test('orElse-second-succeed', () => {
    const result = runParser(parser, 'bb');
    expect(result).toEqual({
      type: 'Success',
      val: 'b',
      rest: {
        lines: ['bb'],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });
}

const choice = <T, >(parsers: Parser<T>[]): Parser<T> => parsers.reduce(orElse);
const anyOf = (characters: string[]) => {
  const newParser = choice(characters.map(parseCharacter));
  return setParserLabel(newParser, `anyOf (${characters.join(',')})`)
};

const range = (start: number, end: number): number[] =>
  Array.from({length: end - start + 1}, (_, k) => k + start);

const parseLowercase = satisfy(x => /[a-z]/.test(x), 'lowercase letter');
const parseUppercase = satisfy(x => /[A-Z]/.test(x), 'uppercase letter');
const parseDigit = satisfy(x => /\d/.test(x), 'digit');
const parseWhiteCharacter = satisfy(x => /\s/.test(x), 'digit');

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('parseLowercase', () => {
    const result = runParser(parseLowercase, 'a');
    expect(result).toEqual({
      type: 'Success',
      val: 'a',
      rest: {
        lines: ['a'],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });

  test('parseUppercase', () => {
    const result = runParser(parseUppercase, 'A');
    expect(result).toEqual({
      type: 'Success',
      val: 'A',
      rest: {
        lines: ['A'],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });

  test('parseDigit', () => {
    const result = runParser(parseDigit, '0');
    expect(result).toEqual({
      type: 'Success',
      val: '0',
      rest: {
        lines: ['0'],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });

  test('parseWhiteCharacter', () => {
    const result = runParser(parseWhiteCharacter, ' ');
    expect(result).toEqual({
      type: 'Success',
      val: ' ',
      rest: {
        lines: [' '],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });
}

const mapParser = <T, TRes>(parser: Parser<T>, func: (arg: T) => TRes): Parser<TRes> => {
  const fParser = (arg: T) => returnParser(func(arg));
  return bindParser(parser, fParser);
};

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('mapParser', () => {
    const parser = mapParser(parseDigit, Number);
    const result = runParser(parser, '0');
    expect(result).toEqual({
      type: 'Success',
      val: 0,
      rest: {
        lines: ['0'],
        position: {
          line: 0,
          column: 1,
        },
      },
    });
  });
}

const returnParser = <T, >(val: T): Parser<T> => ({
  label: 'unknown',
  parserFn: (input) => ({type: 'Success', val, rest: input})
});

const applyParser = <T, TRes>(parser: Parser<((arg: T) => TRes)>, val: Parser<T>) =>
  bindParser(parser,
    f => bindParser(val,
      x => returnParser(f(x))
    )
  );

const lift2Parser = <T1, T2, TRes>(func: (arg1: T1) => (arg2: T2) => TRes) =>
  (p1: Parser<T1>) => (p2: Parser<T2>) => {
    const parser = returnParser(func);
    return applyParser(applyParser(parser, p1), p2);
  };

const addParser = lift2Parser((a: number) => (b: number) => a + b);

const sequence = <T, >(parsers: Parser<T>[]): Parser<T[]> => {
  const con = (head: T) => (tail: T[]) => [head, ...tail];
  const conP = lift2Parser(con);
  if (parsers.length === 0) {
    return returnParser([]);
  }
  const [h, ...t] = parsers;
  return conP(h)(sequence(t));
};

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('sequence', () => {
    const parser = sequence([
      parseCharacter('a'),
      parseCharacter('b'),
      parseCharacter('c'),
    ]);
    const result = runParser(parser, 'abc');
    expect(result).toEqual({
      type: 'Success',
      val: ['a', 'b', 'c'],
      rest: {
        lines: ['abc'],
        position: {
          line: 0,
          column: 3,
        },
      },
    });
  });
}

const many = <T, >(parser: Parser<T>): Parser<T[]> => ({
  label: `many(${parser.label})`,
  parserFn: (input) => {
    const firstResult = runParserOnInput(parser, input);
    if (firstResult.type === 'Failure') {
      return {type: 'Success', val: [], rest: input};
    }
    const secondResult = runParserOnInput(many(parser), firstResult.rest);
    if (secondResult.type === 'Failure') {
      throw new Error('Fatal error');
    }
    return {type: 'Success', val: [firstResult.val, ...secondResult.val], rest: secondResult.rest}
  },
});

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('many-empty', () => {
    const parser = many(parseCharacter('a'));
    const result = runParser(parser, '');
    expect(result).toEqual({
      type: 'Success',
      val: [],
      rest: {
        lines: [],
        position: {
          line: 0,
          column: 0,
        },
      },
    });
  });

  test('many', () => {
    const parser = many(parseCharacter('a'));
    const result = runParser(parser, 'aaa');
    expect(result).toEqual({
      type: 'Success',
      val: ['a', 'a', 'a'],
      rest: {
        lines: ['aaa'],
        position: {
          line: 0,
          column: 3,
        },
      },
    });
  });
}

const many1 = <T, >(parser: Parser<T>): Parser<T[]> => {
  const newParser = bindParser(parser, h =>
    bindParser(many(parser), t =>
      returnParser([h, ...t])
    )
  );
  return setParserLabel(newParser, `many1(${parser.label})`)
};

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('many1-empty', () => {
    const parser = many1(parseCharacter('a'));
    const result = runParser(parser, 'b');
    expect(result).toEqual({
      type: 'Failure',
      parser: 'many1(parse a)',
      error: `Unexpected 'b'`,
      position: {
        line: 0,
        column: 0,
        currentLine: 'b',
      },
    });
  });
}

const opt = <T, >(parser: Parser<T>): Parser<T | null> =>
  orElse(parser, returnParser(null));

const parseString = (str: string): Parser<string> => {
  const characterParsers = str.split('').map(parseCharacter);
  const parser = sequence(characterParsers);
  return setParserLabel(mapParser(parser, x => x.join('')), `parse ${str}`);
};

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('parseString', () => {
    const parser = parseString('ABC');
    const result = runParser(parser, 'ABCd');
    expect(result).toEqual({
      type: 'Success',
      val: 'ABC',
      rest: {
        lines: ['ABCd'],
        position: {
          line: 0,
          column: 3,
        },
      },
    });
  });
}

const parseInteger = setParserLabel(
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

const parseFloat = setParserLabel(
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
