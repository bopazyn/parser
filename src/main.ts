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

const run = <T, >(parser: Parser<T>, str: string) => parser(str);


if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');

  test('parseCharacter-empty', () => {
    const result = run(parseA, '');
    expect(result).toEqual({type: 'Failure', error: 'No more input'});
  });

  test('parseCharacter-b', () => {
    const result = run(parseA, 'b');
    expect(result).toEqual({type: 'Failure', error: `Expecting 'a'. Got 'b'`});
  });

  test('parseCharacter-a', () => {
    const result = run(parseA, 'a');
    expect(result).toEqual({type: 'Success', val: 'a', rest: ''});
  });
}

const andThen = <T1, T2>(parser1: Parser<T1>, parser2: Parser<T2>): Parser<[T1, T2]> =>
  (str) => {
    const res1 = run(parser1, str);
    if (res1.type === 'Failure') {
      return res1;
    }
    const res2 = run(parser2, res1.rest);
    if (res2.type === 'Failure') {
      return res2;
    }
    return ({
      type: 'Success',
      val: [res1.val, res2.val],
      rest: res2.rest,
    });
  }

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');
  const parseB = parseCharacter('b');
  const parser = andThen(parseA, parseB);

  test('andThen-success', () => {
    const result = run(parser, 'ab');
    expect(result).toEqual({type: 'Success', val: ['a', 'b'], rest: ''});
  });

  test('andThen-first-fails', () => {
    const result = run(parser, 'bb');
    expect(result).toEqual({type: 'Failure', error: `Expecting 'a'. Got 'b'`});
  });

  test('andThen-second-fails', () => {
    const result = run(parser, 'aa');
    expect(result).toEqual({type: 'Failure', error: `Expecting 'b'. Got 'a'`});
  });
}

const orElse = <T1, T2>(parser1: Parser<T1>, parser2: Parser<T2>): Parser<T1 | T2> =>
  (str) => {
    const res1 = run(parser1, str);
    if (res1.type === 'Success') {
      return res1;
    }
    const res2 = run(parser2, str);
    return res2;
  }

if (import.meta.vitest) {
  const {test, expect} = import.meta.vitest;

  const parseA = parseCharacter('a');
  const parseB = parseCharacter('b');
  const parser = orElse(parseA, parseB);

  test('orElse-first-succeed', () => {
    const result = run(parser, 'ab');
    expect(result).toEqual({type: 'Success', val: 'a', rest: 'b'});
  });

  test('orElse-second-succeed', () => {
    const result = run(parser, 'bb');
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
