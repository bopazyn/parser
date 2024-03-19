type ParseResult<T> =
  | { type: 'Success', val: T, rest: string }
  | { type: 'Failure', error: string }

type Parser<T> = (str: string) => ParseResult<T>;

const parseCharacter = (c: string): Parser<string> =>
  (str) => {
    if (str.length === 0) {
      return {type: 'Failure', error: 'No more input'};
    }
    const tmp = str.substring(0, c.length);
    if (tmp !== c) {
      return {type: 'Failure', error: `Expecting '${c}'. Got '${tmp}'`};
    }
    return {type: 'Success', val: c, rest: str.substring(c.length)};
  };

const runParser = <T, >(parser: Parser<T>, str: string) => parser(str);


if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');

  test('parseCharacter-empty', () => {
    const result = runParser(parseA, '');
    expect(result).toEqual({type: 'Failure', error: 'No more input'});
  });

  test('parseCharacter-b', () => {
    const result = runParser(parseA, 'b');
    expect(result).toEqual({type: 'Failure', error: `Expecting 'a'. Got 'b'`});
  });

  test('parseCharacter-a', () => {
    const result = runParser(parseA, 'a');
    expect(result).toEqual({type: 'Success', val: 'a', rest: ''});
  });
}

const bindParser = <T1, T2>(parser: Parser<T1>, func: (arg: T1) => Parser<T2>): Parser<T2> =>
  (str) => {
    const res = runParser(parser, str);
    if (res.type === 'Failure') {
      return res;
    }

    return runParser(func(res.val), res.rest);
  };

const andThen = <T1, T2>(parser1: Parser<T1>, parser2: Parser<T2>): Parser<[T1, T2]> =>
  bindParser(parser1, r1 =>
    bindParser(parser2, r2 =>
      returnParser([r1, r2] as [T1, T2]),
    ),
  );

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');
  const parseB = parseCharacter('b');
  const parser = andThen(parseA, parseB);

  test('andThen-success', () => {
    const result = runParser(parser, 'ab');
    expect(result).toEqual({type: 'Success', val: ['a', 'b'], rest: ''});
  });

  test('andThen-first-fails', () => {
    const result = runParser(parser, 'bb');
    expect(result).toEqual({type: 'Failure', error: `Expecting 'a'. Got 'b'`});
  });

  test('andThen-second-fails', () => {
    const result = runParser(parser, 'aa');
    expect(result).toEqual({type: 'Failure', error: `Expecting 'b'. Got 'a'`});
  });
}

const orElse = <T1, T2>(parser1: Parser<T1>, parser2: Parser<T2>): Parser<T1 | T2> =>
  (str) => {
    const res = runParser(parser1, str);
    return res.type === 'Success' ? res : runParser(parser2, str);
  }

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');
  const parseB = parseCharacter('b');
  const parser = orElse(parseA, parseB);

  test('orElse-first-succeed', () => {
    const result = runParser(parser, 'ab');
    expect(result).toEqual({type: 'Success', val: 'a', rest: 'b'});
  });

  test('orElse-second-succeed', () => {
    const result = runParser(parser, 'bb');
    expect(result).toEqual({type: 'Success', val: 'b', rest: 'b'});
  });
}

const choice = <T, >(parsers: Parser<T>[]): Parser<T> => parsers.reduce(orElse);
const anyOf = (characters: string[]) => choice(characters.map(parseCharacter));

const range = (start: number, end: number): number[] =>
  Array.from({length: end - start + 1}, (_, k) => k + start);

const parseLowercase = anyOf(range(97, 122).map(x => String.fromCharCode(x)));
const parseUppercase = anyOf(range(65, 90).map(x => String.fromCharCode(x)));
const parseDigit = anyOf(range(0, 9).map(x => x.toString()));

const mapParser = <T, TRes>(parser: Parser<T>, func: (arg: T) => TRes): Parser<TRes> => {
  const fParser = (arg: T) => returnParser(func(arg));
  return bindParser(parser, fParser);
};

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('mapParser', () => {
    const parser = mapParser(parseDigit, Number);
    const result = runParser(parser, '0');
    expect(result).toEqual({type: 'Success', val: 0, rest: ''});
  });
}

const returnParser = <T, >(val: T): Parser<T> =>
  (str) => ({type: 'Success', val, rest: str});

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
    expect(result).toEqual({type: 'Success', val: ['a', 'b', 'c'], rest: ''});
  });
}

const many = <T, >(parser: Parser<T>): Parser<T[]> =>
  (str) => {
    const firstResult = runParser(parser, str);
    if (firstResult.type === 'Failure') {
      return {type: 'Success', val: [], rest: str};
    }
    const secondResult = runParser(many(parser), firstResult.rest);
    if (secondResult.type === 'Failure') {
      throw new Error('Fatal error');
    }
    return {type: 'Success', val: [firstResult.val, ...secondResult.val], rest: secondResult.rest}
  }

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('many-empty', () => {
    const parser = many(parseCharacter('a'));
    const result = runParser(parser, '');
    expect(result).toEqual({type: 'Success', val: [], rest: ''});
  });

  test('many', () => {
    const parser = many(parseCharacter('a'));
    const result = runParser(parser, 'aaa');
    expect(result).toEqual({type: 'Success', val: ['a', 'a', 'a'], rest: ''});
  });
}

const many1 = <T, >(parser: Parser<T>): Parser<T[]> =>
  bindParser(parser, h =>
    bindParser(many(parser), t =>
      returnParser([h, ...t])
    )
  );


if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  test('many1-empty', () => {
    const parser = many1(parseCharacter('a'));
    const result = runParser(parser, 'b');
    expect(result).toEqual({type: 'Failure', error: `Expecting 'a'. Got 'b'`});
  });
}

const opt = <T, >(parser: Parser<T>): Parser<T | null> =>
  orElse(parser, returnParser(null));

