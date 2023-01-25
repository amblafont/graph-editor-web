(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $author$project$ArrowStyle$DefaultHead = {$: 'DefaultHead'};
var $author$project$ArrowStyle$DefaultTail = {$: 'DefaultTail'};
var $author$project$ArrowStyle$Left = {$: 'Left'};
var $author$project$ArrowStyle$empty = {bend: 0, dashed: false, _double: false, head: $author$project$ArrowStyle$DefaultHead, labelAlignment: $author$project$ArrowStyle$Left, labelPosition: 0.5, tail: $author$project$ArrowStyle$DefaultTail};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$Polygraph$EdgeObj = F3(
	function (a, b, c) {
		return {$: 'EdgeObj', a: a, b: b, c: c};
	});
var $author$project$Polygraph$Graph = function (a) {
	return {$: 'Graph', a: a};
};
var $author$project$Polygraph$NodeObj = function (a) {
	return {$: 'NodeObj', a: a};
};
var $elm_community$intdict$IntDict$Empty = {$: 'Empty'};
var $elm_community$intdict$IntDict$empty = $elm_community$intdict$IntDict$Empty;
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm_community$intdict$IntDict$Inner = function (a) {
	return {$: 'Inner', a: a};
};
var $elm_community$intdict$IntDict$size = function (dict) {
	switch (dict.$) {
		case 'Empty':
			return 0;
		case 'Leaf':
			return 1;
		default:
			var i = dict.a;
			return i.size;
	}
};
var $elm_community$intdict$IntDict$inner = F3(
	function (p, l, r) {
		var _v0 = _Utils_Tuple2(l, r);
		if (_v0.a.$ === 'Empty') {
			var _v1 = _v0.a;
			return r;
		} else {
			if (_v0.b.$ === 'Empty') {
				var _v2 = _v0.b;
				return l;
			} else {
				return $elm_community$intdict$IntDict$Inner(
					{
						left: l,
						prefix: p,
						right: r,
						size: $elm_community$intdict$IntDict$size(l) + $elm_community$intdict$IntDict$size(r)
					});
			}
		}
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Bitwise$complement = _Bitwise_complement;
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm_community$intdict$IntDict$highestBitSet = function (n) {
	var shiftOr = F2(
		function (i, shift) {
			return i | (i >>> shift);
		});
	var n1 = A2(shiftOr, n, 1);
	var n2 = A2(shiftOr, n1, 2);
	var n3 = A2(shiftOr, n2, 4);
	var n4 = A2(shiftOr, n3, 8);
	var n5 = A2(shiftOr, n4, 16);
	return n5 & (~(n5 >>> 1));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm_community$intdict$IntDict$signBit = $elm_community$intdict$IntDict$highestBitSet(-1);
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm_community$intdict$IntDict$isBranchingBitSet = function (p) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$Bitwise$xor($elm_community$intdict$IntDict$signBit),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Bitwise$and(p.branchingBit),
			$elm$core$Basics$neq(0)));
};
var $elm_community$intdict$IntDict$higherBitMask = function (branchingBit) {
	return branchingBit ^ (~(branchingBit - 1));
};
var $elm_community$intdict$IntDict$lcp = F2(
	function (x, y) {
		var branchingBit = $elm_community$intdict$IntDict$highestBitSet(x ^ y);
		var mask = $elm_community$intdict$IntDict$higherBitMask(branchingBit);
		var prefixBits = x & mask;
		return {branchingBit: branchingBit, prefixBits: prefixBits};
	});
var $elm_community$intdict$IntDict$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm_community$intdict$IntDict$leaf = F2(
	function (k, v) {
		return $elm_community$intdict$IntDict$Leaf(
			{key: k, value: v});
	});
var $elm_community$intdict$IntDict$prefixMatches = F2(
	function (p, n) {
		return _Utils_eq(
			n & $elm_community$intdict$IntDict$higherBitMask(p.branchingBit),
			p.prefixBits);
	});
var $elm_community$intdict$IntDict$update = F3(
	function (key, alter, dict) {
		var join = F2(
			function (_v2, _v3) {
				var k1 = _v2.a;
				var l = _v2.b;
				var k2 = _v3.a;
				var r = _v3.b;
				var prefix = A2($elm_community$intdict$IntDict$lcp, k1, k2);
				return A2($elm_community$intdict$IntDict$isBranchingBitSet, prefix, k2) ? A3($elm_community$intdict$IntDict$inner, prefix, l, r) : A3($elm_community$intdict$IntDict$inner, prefix, r, l);
			});
		var alteredNode = function (mv) {
			var _v1 = alter(mv);
			if (_v1.$ === 'Just') {
				var v = _v1.a;
				return A2($elm_community$intdict$IntDict$leaf, key, v);
			} else {
				return $elm_community$intdict$IntDict$empty;
			}
		};
		switch (dict.$) {
			case 'Empty':
				return alteredNode($elm$core$Maybe$Nothing);
			case 'Leaf':
				var l = dict.a;
				return _Utils_eq(l.key, key) ? alteredNode(
					$elm$core$Maybe$Just(l.value)) : A2(
					join,
					_Utils_Tuple2(
						key,
						alteredNode($elm$core$Maybe$Nothing)),
					_Utils_Tuple2(l.key, dict));
			default:
				var i = dict.a;
				return A2($elm_community$intdict$IntDict$prefixMatches, i.prefix, key) ? (A2($elm_community$intdict$IntDict$isBranchingBitSet, i.prefix, key) ? A3(
					$elm_community$intdict$IntDict$inner,
					i.prefix,
					i.left,
					A3($elm_community$intdict$IntDict$update, key, alter, i.right)) : A3(
					$elm_community$intdict$IntDict$inner,
					i.prefix,
					A3($elm_community$intdict$IntDict$update, key, alter, i.left),
					i.right)) : A2(
					join,
					_Utils_Tuple2(
						key,
						alteredNode($elm$core$Maybe$Nothing)),
					_Utils_Tuple2(i.prefix.prefixBits, dict));
		}
	});
var $elm_community$intdict$IntDict$insert = F3(
	function (key, value, dict) {
		return A3(
			$elm_community$intdict$IntDict$update,
			key,
			$elm$core$Basics$always(
				$elm$core$Maybe$Just(value)),
			dict);
	});
var $elm_community$intdict$IntDict$fromList = function (pairs) {
	return A3(
		$elm$core$List$foldl,
		function (_v0) {
			var a = _v0.a;
			var b = _v0.b;
			return A2($elm_community$intdict$IntDict$insert, a, b);
		},
		$elm_community$intdict$IntDict$empty,
		pairs);
};
var $elm_community$intdict$IntDict$Disjunct = F2(
	function (a, b) {
		return {$: 'Disjunct', a: a, b: b};
	});
var $elm_community$intdict$IntDict$Left = {$: 'Left'};
var $elm_community$intdict$IntDict$Parent = F2(
	function (a, b) {
		return {$: 'Parent', a: a, b: b};
	});
var $elm_community$intdict$IntDict$Right = {$: 'Right'};
var $elm_community$intdict$IntDict$SamePrefix = {$: 'SamePrefix'};
var $elm_community$intdict$IntDict$combineBits = F3(
	function (a, b, mask) {
		return (a & (~mask)) | (b & mask);
	});
var $elm_community$intdict$IntDict$mostSignificantBranchingBit = F2(
	function (a, b) {
		return (_Utils_eq(a, $elm_community$intdict$IntDict$signBit) || _Utils_eq(b, $elm_community$intdict$IntDict$signBit)) ? $elm_community$intdict$IntDict$signBit : A2($elm$core$Basics$max, a, b);
	});
var $elm_community$intdict$IntDict$determineBranchRelation = F2(
	function (l, r) {
		var rp = r.prefix;
		var lp = l.prefix;
		var mask = $elm_community$intdict$IntDict$highestBitSet(
			A2($elm_community$intdict$IntDict$mostSignificantBranchingBit, lp.branchingBit, rp.branchingBit));
		var modifiedRightPrefix = A3($elm_community$intdict$IntDict$combineBits, rp.prefixBits, ~lp.prefixBits, mask);
		var prefix = A2($elm_community$intdict$IntDict$lcp, lp.prefixBits, modifiedRightPrefix);
		var childEdge = F2(
			function (branchPrefix, c) {
				return A2($elm_community$intdict$IntDict$isBranchingBitSet, branchPrefix, c.prefix.prefixBits) ? $elm_community$intdict$IntDict$Right : $elm_community$intdict$IntDict$Left;
			});
		return _Utils_eq(lp, rp) ? $elm_community$intdict$IntDict$SamePrefix : (_Utils_eq(prefix, lp) ? A2(
			$elm_community$intdict$IntDict$Parent,
			$elm_community$intdict$IntDict$Left,
			A2(childEdge, l.prefix, r)) : (_Utils_eq(prefix, rp) ? A2(
			$elm_community$intdict$IntDict$Parent,
			$elm_community$intdict$IntDict$Right,
			A2(childEdge, r.prefix, l)) : A2(
			$elm_community$intdict$IntDict$Disjunct,
			prefix,
			A2(childEdge, prefix, l))));
	});
var $elm_community$intdict$IntDict$uniteWith = F3(
	function (merger, l, r) {
		var mergeWith = F3(
			function (key, left, right) {
				var _v14 = _Utils_Tuple2(left, right);
				if (_v14.a.$ === 'Just') {
					if (_v14.b.$ === 'Just') {
						var l2 = _v14.a.a;
						var r2 = _v14.b.a;
						return $elm$core$Maybe$Just(
							A3(merger, key, l2, r2));
					} else {
						return left;
					}
				} else {
					if (_v14.b.$ === 'Just') {
						return right;
					} else {
						var _v15 = _v14.a;
						var _v16 = _v14.b;
						return $elm$core$Maybe$Nothing;
					}
				}
			});
		var _v0 = _Utils_Tuple2(l, r);
		_v0$1:
		while (true) {
			_v0$2:
			while (true) {
				switch (_v0.a.$) {
					case 'Empty':
						var _v1 = _v0.a;
						return r;
					case 'Leaf':
						switch (_v0.b.$) {
							case 'Empty':
								break _v0$1;
							case 'Leaf':
								break _v0$2;
							default:
								break _v0$2;
						}
					default:
						switch (_v0.b.$) {
							case 'Empty':
								break _v0$1;
							case 'Leaf':
								var r2 = _v0.b.a;
								return A3(
									$elm_community$intdict$IntDict$update,
									r2.key,
									function (l_) {
										return A3(
											mergeWith,
											r2.key,
											l_,
											$elm$core$Maybe$Just(r2.value));
									},
									l);
							default:
								var il = _v0.a.a;
								var ir = _v0.b.a;
								var _v3 = A2($elm_community$intdict$IntDict$determineBranchRelation, il, ir);
								switch (_v3.$) {
									case 'SamePrefix':
										return A3(
											$elm_community$intdict$IntDict$inner,
											il.prefix,
											A3($elm_community$intdict$IntDict$uniteWith, merger, il.left, ir.left),
											A3($elm_community$intdict$IntDict$uniteWith, merger, il.right, ir.right));
									case 'Parent':
										if (_v3.a.$ === 'Left') {
											if (_v3.b.$ === 'Right') {
												var _v4 = _v3.a;
												var _v5 = _v3.b;
												return A3(
													$elm_community$intdict$IntDict$inner,
													il.prefix,
													il.left,
													A3($elm_community$intdict$IntDict$uniteWith, merger, il.right, r));
											} else {
												var _v8 = _v3.a;
												var _v9 = _v3.b;
												return A3(
													$elm_community$intdict$IntDict$inner,
													il.prefix,
													A3($elm_community$intdict$IntDict$uniteWith, merger, il.left, r),
													il.right);
											}
										} else {
											if (_v3.b.$ === 'Right') {
												var _v6 = _v3.a;
												var _v7 = _v3.b;
												return A3(
													$elm_community$intdict$IntDict$inner,
													ir.prefix,
													ir.left,
													A3($elm_community$intdict$IntDict$uniteWith, merger, l, ir.right));
											} else {
												var _v10 = _v3.a;
												var _v11 = _v3.b;
												return A3(
													$elm_community$intdict$IntDict$inner,
													ir.prefix,
													A3($elm_community$intdict$IntDict$uniteWith, merger, l, ir.left),
													ir.right);
											}
										}
									default:
										if (_v3.b.$ === 'Left') {
											var parentPrefix = _v3.a;
											var _v12 = _v3.b;
											return A3($elm_community$intdict$IntDict$inner, parentPrefix, l, r);
										} else {
											var parentPrefix = _v3.a;
											var _v13 = _v3.b;
											return A3($elm_community$intdict$IntDict$inner, parentPrefix, r, l);
										}
								}
						}
				}
			}
			var l2 = _v0.a.a;
			return A3(
				$elm_community$intdict$IntDict$update,
				l2.key,
				function (r_) {
					return A3(
						mergeWith,
						l2.key,
						$elm$core$Maybe$Just(l2.value),
						r_);
				},
				r);
		}
		var _v2 = _v0.b;
		return l;
	});
var $elm_community$intdict$IntDict$union = $elm_community$intdict$IntDict$uniteWith(
	F3(
		function (key, old, _new) {
			return old;
		}));
var $author$project$Polygraph$fromNodesAndEdges = F2(
	function (ln, le) {
		var dn = $elm_community$intdict$IntDict$fromList(
			A2(
				$elm$core$List$map,
				function (_v1) {
					var id = _v1.id;
					var label = _v1.label;
					return _Utils_Tuple2(
						id,
						$author$project$Polygraph$NodeObj(label));
				},
				ln));
		var de = $elm_community$intdict$IntDict$fromList(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var id = _v0.id;
					var from = _v0.from;
					var to = _v0.to;
					var label = _v0.label;
					return _Utils_Tuple2(
						id,
						A3($author$project$Polygraph$EdgeObj, from, to, label));
				},
				le));
		return $author$project$Polygraph$Graph(
			A2($elm_community$intdict$IntDict$union, dn, de));
	});
var $elm$core$Debug$log = _Debug_log;
var $author$project$Polygraph$mapRep = F2(
	function (f, _v0) {
		var g = _v0.a;
		return $author$project$Polygraph$Graph(
			f(g));
	});
var $elm_community$intdict$IntDict$findMax = function (dict) {
	findMax:
	while (true) {
		switch (dict.$) {
			case 'Empty':
				return $elm$core$Maybe$Nothing;
			case 'Leaf':
				var l = dict.a;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(l.key, l.value));
			default:
				var i = dict.a;
				var $temp$dict = i.right;
				dict = $temp$dict;
				continue findMax;
		}
	}
};
var $author$project$Polygraph$supId = function (g) {
	var _v0 = $elm_community$intdict$IntDict$findMax(g);
	if (_v0.$ === 'Just') {
		var _v1 = _v0.a;
		var id = _v1.a;
		return id + 1;
	} else {
		return 0;
	}
};
var $author$project$Polygraph$nextId = function (_v0) {
	var g = _v0.a;
	return $author$project$Polygraph$supId(g);
};
var $author$project$Polygraph$newObject = F2(
	function (g, o) {
		var id = $author$project$Polygraph$nextId(g);
		return _Utils_Tuple2(
			A2(
				$author$project$Polygraph$mapRep,
				A2($elm_community$intdict$IntDict$insert, id, o),
				g),
			id);
	});
var $author$project$Polygraph$newEdge = F4(
	function (g, i1, i2, e) {
		return A2(
			$author$project$Polygraph$newObject,
			g,
			A3($author$project$Polygraph$EdgeObj, i1, i2, e));
	});
var $author$project$GraphDefs$NormalEdge = function (a) {
	return {$: 'NormalEdge', a: a};
};
var $author$project$GraphDefs$newGenericLabel = function (d) {
	return {details: d, selected: false, weaklySelected: false, zindex: 0};
};
var $author$project$GraphDefs$newEdgeLabel = F2(
	function (s, style) {
		return $author$project$GraphDefs$newGenericLabel(
			$author$project$GraphDefs$NormalEdge(
				{dims: $elm$core$Maybe$Nothing, label: s, style: style}));
	});
var $author$project$GraphDefs$newNodeLabel = F3(
	function (p, s, isMath) {
		return {dims: $elm$core$Maybe$Nothing, isMath: isMath, label: s, pos: p, selected: false, weaklySelected: false};
	});
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $author$project$ParseLatex$buildGraph = F2(
	function (offset, _v0) {
		var nodes = _v0.a;
		var edges = _v0.b;
		var maxX = $elm$core$List$length(nodes) + 1;
		var mkPos = F2(
			function (x, y) {
				return _Utils_Tuple2((y + 0.5) * offset, (x + 0.5) * offset);
			});
		var mkId = function (_v4) {
			var x = _v4.a;
			var y = _v4.b;
			return (y * maxX) + x;
		};
		var nodesPos = A2(
			$elm$core$Debug$log,
			'nodes',
			A2(
				$elm$core$List$filter,
				function (_v3) {
					var label = _v3.label;
					return label.label !== '';
				},
				$elm$core$List$concat(
					A2(
						$elm$core$List$indexedMap,
						function (x) {
							return $elm$core$List$indexedMap(
								F2(
									function (y, s) {
										return {
											id: mkId(
												_Utils_Tuple2(x + 1, y + 1)),
											label: A3(
												$author$project$GraphDefs$newNodeLabel,
												A2(mkPos, x, y),
												s,
												true)
										};
									}));
						},
						nodes))));
		var _v1 = A2(
			$elm$core$Debug$log,
			'ids',
			$elm$core$List$sort(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.id;
					},
					nodesPos)));
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v2, g) {
					var from = _v2.from;
					var to = _v2.to;
					var label = _v2.label;
					return A4(
						$author$project$Polygraph$newEdge,
						g,
						A2(
							$elm$core$Debug$log,
							'idfrom',
							mkId(from)),
						A2(
							$elm$core$Debug$log,
							'idto',
							mkId(to)),
						A2($author$project$GraphDefs$newEdgeLabel, label, $author$project$ArrowStyle$empty)).a;
				}),
			A2($author$project$Polygraph$fromNodesAndEdges, nodesPos, _List_Nil),
			edges);
	});
var $author$project$Modes$DefaultMode = {$: 'DefaultMode'};
var $author$project$Msg$Standard = {$: 'Standard'};
var $author$project$Model$createModel = F2(
	function (sizeGrid, g) {
		return {
			autoSave: true,
			bottomText: '',
			fileName: 'graph.json',
			graph: g,
			hideGrid: true,
			history: _List_Nil,
			latexPreamble: '',
			mode: $author$project$Modes$DefaultMode,
			mouseOnCanvas: false,
			mousePos: _Utils_Tuple2(0, 0),
			quickInput: '',
			scenario: $author$project$Msg$Standard,
			sizeGrid: sizeGrid,
			specialKeys: {alt: false, ctrl: false, shift: false},
			statusMsg: ''
		};
	});
var $author$project$Format$GraphInfo$defaultGridSize = 200;
var $author$project$Polygraph$empty = $author$project$Polygraph$Graph($elm_community$intdict$IntDict$empty);
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Model$iniModel = function () {
	var sizeGrid = $author$project$Format$GraphInfo$defaultGridSize;
	var graph = A2(
		$elm$core$Maybe$withDefault,
		$author$project$Polygraph$empty,
		A2(
			$elm$core$Maybe$map,
			$author$project$ParseLatex$buildGraph(sizeGrid),
			$elm$core$Maybe$Nothing));
	return A2($author$project$Model$createModel, sizeGrid, graph);
}();
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Msg$CopyGraph = {$: 'CopyGraph'};
var $author$project$Msg$Do = function (a) {
	return {$: 'Do', a: a};
};
var $author$project$Msg$FileName = function (a) {
	return {$: 'FileName', a: a};
};
var $author$project$Msg$FindReplace = function (a) {
	return {$: 'FindReplace', a: a};
};
var $author$project$Msg$KeyChanged = F3(
	function (a, b, c) {
		return {$: 'KeyChanged', a: a, b: b, c: c};
	});
var $author$project$Msg$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $author$project$Msg$MinuteTick = {$: 'MinuteTick'};
var $author$project$Msg$MouseClick = {$: 'MouseClick'};
var $author$project$Msg$MouseMove = function (a) {
	return {$: 'MouseMove', a: a};
};
var $author$project$Msg$PasteGraph = function (a) {
	return {$: 'PasteGraph', a: a};
};
var $author$project$Msg$QuickInput = F2(
	function (a, b) {
		return {$: 'QuickInput', a: a, b: b};
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$clipboardGraph = _Platform_incomingPort(
	'clipboardGraph',
	A2(
		$elm$json$Json$Decode$andThen,
		function (sizeGrid) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (nodes) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (latexPreamble) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (edges) {
									return $elm$json$Json$Decode$succeed(
										{edges: edges, latexPreamble: latexPreamble, nodes: nodes, sizeGrid: sizeGrid});
								},
								A2(
									$elm$json$Json$Decode$field,
									'edges',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (to) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (label) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (id) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (from) {
																		return $elm$json$Json$Decode$succeed(
																			{from: from, id: id, label: label, to: to});
																	},
																	A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
															},
															A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
													},
													A2(
														$elm$json$Json$Decode$field,
														'label',
														A2(
															$elm$json$Json$Decode$andThen,
															function (zindex) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (style) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (label) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (isPullshout) {
																						return $elm$json$Json$Decode$succeed(
																							{isPullshout: isPullshout, label: label, style: style, zindex: zindex});
																					},
																					A2($elm$json$Json$Decode$field, 'isPullshout', $elm$json$Json$Decode$bool));
																			},
																			A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'style',
																		A2(
																			$elm$json$Json$Decode$andThen,
																			function (tail) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (position) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (head) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (_double) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (dashed) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (bend) {
																														return A2(
																															$elm$json$Json$Decode$andThen,
																															function (alignment) {
																																return $elm$json$Json$Decode$succeed(
																																	{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																															},
																															A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																													},
																													A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																											},
																											A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																									},
																									A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																							},
																							A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																					},
																					A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																			},
																			A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))));
															},
															A2($elm$json$Json$Decode$field, 'zindex', $elm$json$Json$Decode$int))));
											},
											A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
						},
						A2($elm$json$Json$Decode$field, 'latexPreamble', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'nodes',
					$elm$json$Json$Decode$list(
						A2(
							$elm$json$Json$Decode$andThen,
							function (label) {
								return A2(
									$elm$json$Json$Decode$andThen,
									function (id) {
										return $elm$json$Json$Decode$succeed(
											{id: id, label: label});
									},
									A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
							},
							A2(
								$elm$json$Json$Decode$field,
								'label',
								A2(
									$elm$json$Json$Decode$andThen,
									function (pos) {
										return A2(
											$elm$json$Json$Decode$andThen,
											function (label) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (isMath) {
														return $elm$json$Json$Decode$succeed(
															{isMath: isMath, label: label, pos: pos});
													},
													A2($elm$json$Json$Decode$field, 'isMath', $elm$json$Json$Decode$bool));
											},
											A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
									},
									A2(
										$elm$json$Json$Decode$field,
										'pos',
										A2(
											$elm$json$Json$Decode$andThen,
											function (_v0) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (_v1) {
														return $elm$json$Json$Decode$succeed(
															_Utils_Tuple2(_v0, _v1));
													},
													A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
											},
											A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))));
		},
		A2($elm$json$Json$Decode$field, 'sizeGrid', $elm$json$Json$Decode$int)));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $author$project$Main$findReplace = _Platform_incomingPort(
	'findReplace',
	A2(
		$elm$json$Json$Decode$andThen,
		function (search) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (replace) {
					return $elm$json$Json$Decode$succeed(
						{replace: replace, search: search});
				},
				A2($elm$json$Json$Decode$field, 'replace', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'search', $elm$json$Json$Decode$string)));
var $elm_community$intdict$IntDict$map = F2(
	function (f, dict) {
		switch (dict.$) {
			case 'Empty':
				return $elm_community$intdict$IntDict$empty;
			case 'Leaf':
				var l = dict.a;
				return A2(
					$elm_community$intdict$IntDict$leaf,
					l.key,
					A2(f, l.key, l.value));
			default:
				var i = dict.a;
				return A3(
					$elm_community$intdict$IntDict$inner,
					i.prefix,
					A2($elm_community$intdict$IntDict$map, f, i.left),
					A2($elm_community$intdict$IntDict$map, f, i.right));
		}
	});
var $author$project$Polygraph$mapObj = F3(
	function (fn, fe, o) {
		if (o.$ === 'NodeObj') {
			var n = o.a;
			return $author$project$Polygraph$NodeObj(
				fn(n));
		} else {
			var i1 = o.a;
			var i2 = o.b;
			var e = o.c;
			return A3(
				$author$project$Polygraph$EdgeObj,
				i1,
				i2,
				fe(e));
		}
	});
var $author$project$Polygraph$map = F2(
	function (fn, fe) {
		return $author$project$Polygraph$mapRep(
			$elm_community$intdict$IntDict$map(
				function (i) {
					return A2(
						$author$project$Polygraph$mapObj,
						fn(i),
						fe(i));
				}));
	});
var $author$project$GraphDefs$PullshoutEdge = {$: 'PullshoutEdge'};
var $author$project$ArrowStyle$Centre = {$: 'Centre'};
var $author$project$ArrowStyle$Over = {$: 'Over'};
var $author$project$ArrowStyle$Right = {$: 'Right'};
var $author$project$ArrowStyle$alignmentFromString = function (tail) {
	switch (tail) {
		case 'centre':
			return $author$project$ArrowStyle$Centre;
		case 'right':
			return $author$project$ArrowStyle$Right;
		case 'over':
			return $author$project$ArrowStyle$Over;
		default:
			return $author$project$ArrowStyle$Left;
	}
};
var $author$project$ArrowStyle$NoHead = {$: 'NoHead'};
var $author$project$ArrowStyle$TwoHeads = {$: 'TwoHeads'};
var $author$project$ArrowStyle$headFromString = function (head) {
	switch (head) {
		case 'twoheads':
			return $author$project$ArrowStyle$TwoHeads;
		case 'none':
			return $author$project$ArrowStyle$NoHead;
		default:
			return $author$project$ArrowStyle$DefaultHead;
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$ArrowStyle$Hook = {$: 'Hook'};
var $author$project$ArrowStyle$HookAlt = {$: 'HookAlt'};
var $author$project$ArrowStyle$tailFromString = function (tail) {
	switch (tail) {
		case 'hook':
			return $author$project$ArrowStyle$Hook;
		case 'hookalt':
			return $author$project$ArrowStyle$HookAlt;
		default:
			return $author$project$ArrowStyle$DefaultTail;
	}
};
var $author$project$Format$Version8$toEdgeLabel = function (_v0) {
	var label = _v0.label;
	var style = _v0.style;
	var isPullshout = _v0.isPullshout;
	var zindex = _v0.zindex;
	return {
		details: isPullshout ? $author$project$GraphDefs$PullshoutEdge : $author$project$GraphDefs$NormalEdge(
			{
				dims: $elm$core$Maybe$Nothing,
				label: label,
				style: {
					bend: style.bend,
					dashed: style.dashed,
					_double: style._double,
					head: $author$project$ArrowStyle$headFromString(style.head),
					labelAlignment: $author$project$ArrowStyle$alignmentFromString(style.alignment),
					labelPosition: A2(
						$elm$core$Basics$max,
						0.1,
						A2($elm$core$Basics$min, 0.9, style.position)),
					tail: $author$project$ArrowStyle$tailFromString(style.tail)
				}
			}),
		selected: false,
		weaklySelected: false,
		zindex: zindex
	};
};
var $author$project$Format$Version8$toNodeLabel = function (_v0) {
	var pos = _v0.pos;
	var label = _v0.label;
	var isMath = _v0.isMath;
	return {dims: $elm$core$Maybe$Nothing, isMath: isMath, label: label, pos: pos, selected: false, weaklySelected: false};
};
var $author$project$Format$Version8$fromJSGraph = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	var sizeGrid = _v0.sizeGrid;
	var latexPreamble = _v0.latexPreamble;
	return {
		graph: A3(
			$author$project$Polygraph$map,
			function (_v1) {
				return $author$project$Format$Version8$toNodeLabel;
			},
			function (_v2) {
				return $author$project$Format$Version8$toEdgeLabel;
			},
			A2($author$project$Polygraph$fromNodesAndEdges, nodes, edges)),
		latexPreamble: latexPreamble,
		sizeGrid: sizeGrid
	};
};
var $author$project$Format$LastVersion$fromJSGraph = $author$project$Format$Version8$fromJSGraph;
var $author$project$Polygraph$edgeMap = F2(
	function (f, _v0) {
		var id = _v0.id;
		var from = _v0.from;
		var to = _v0.to;
		var label = _v0.label;
		return {
			from: from,
			id: id,
			label: f(label),
			to: to
		};
	});
var $author$project$Format$Version7$toNextEdge = function (_v0) {
	var label = _v0.label;
	var style = _v0.style;
	var isPullback = _v0.isPullback;
	return {isPullshout: isPullback, label: label, style: style, zindex: 0};
};
var $author$project$Format$Version7$toNextVersion = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	var sizeGrid = _v0.sizeGrid;
	var latexPreamble = _v0.latexPreamble;
	return {
		edges: A2(
			$elm$core$List$map,
			$author$project$Polygraph$edgeMap($author$project$Format$Version7$toNextEdge),
			edges),
		latexPreamble: latexPreamble,
		nodes: nodes,
		sizeGrid: sizeGrid
	};
};
var $author$project$Format$Version7$fromJSGraph = function (g) {
	return $author$project$Format$Version8$fromJSGraph(
		$author$project$Format$Version7$toNextVersion(g));
};
var $author$project$Format$Version6$toNextEdge = function (_v0) {
	var label = _v0.label;
	var style = _v0.style;
	var isPullback = _v0.isPullback;
	return {isPullback: isPullback, label: label, style: style, zindex: 0};
};
var $author$project$Format$Version6$toNextVersion = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	var sizeGrid = _v0.sizeGrid;
	var latexPreamble = _v0.latexPreamble;
	return {
		edges: A2(
			$elm$core$List$map,
			$author$project$Polygraph$edgeMap($author$project$Format$Version6$toNextEdge),
			edges),
		latexPreamble: latexPreamble,
		nodes: nodes,
		sizeGrid: sizeGrid
	};
};
var $author$project$Format$Version6$fromJSGraph = function (g) {
	return $author$project$Format$Version7$fromJSGraph(
		$author$project$Format$Version6$toNextVersion(g));
};
var $author$project$Format$Version5$toNextEdge = function (_v0) {
	var label = _v0.label;
	var style = _v0.style;
	return {isPullback: false, label: label, style: style};
};
var $author$project$Format$Version5$toNextVersion = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	var sizeGrid = _v0.sizeGrid;
	var latexPreamble = _v0.latexPreamble;
	return {
		edges: A2(
			$elm$core$List$map,
			$author$project$Polygraph$edgeMap($author$project$Format$Version5$toNextEdge),
			edges),
		latexPreamble: latexPreamble,
		nodes: nodes,
		sizeGrid: sizeGrid
	};
};
var $author$project$Format$Version5$fromJSGraph = function (g) {
	return $author$project$Format$Version6$fromJSGraph(
		$author$project$Format$Version5$toNextVersion(g));
};
var $author$project$Format$Version4$toNextVersion = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	var sizeGrid = _v0.sizeGrid;
	return {edges: edges, latexPreamble: '', nodes: nodes, sizeGrid: sizeGrid};
};
var $author$project$Format$Version4$fromJSGraph = function (g) {
	return $author$project$Format$Version5$fromJSGraph(
		$author$project$Format$Version4$toNextVersion(g));
};
var $author$project$Polygraph$nodeMap = F2(
	function (f, _v0) {
		var id = _v0.id;
		var label = _v0.label;
		return {
			id: id,
			label: f(label)
		};
	});
var $author$project$Format$Version3$toNextNode = function (_v0) {
	var label = _v0.label;
	var pos = _v0.pos;
	return {isMath: true, label: label, pos: pos};
};
var $author$project$Format$Version3$toNextVersion = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	var sizeGrid = _v0.sizeGrid;
	return {
		edges: edges,
		nodes: A2(
			$elm$core$List$map,
			$author$project$Polygraph$nodeMap($author$project$Format$Version3$toNextNode),
			nodes),
		sizeGrid: sizeGrid
	};
};
var $author$project$Format$Version3$fromJSGraph = function (g) {
	return $author$project$Format$Version4$fromJSGraph(
		$author$project$Format$Version3$toNextVersion(g));
};
var $author$project$Format$Version2$toNextVersion = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	return {edges: edges, nodes: nodes, sizeGrid: $author$project$Format$GraphInfo$defaultGridSize};
};
var $author$project$Format$Version2$fromJSGraph = function (g) {
	return $author$project$Format$Version3$fromJSGraph(
		$author$project$Format$Version2$toNextVersion(g));
};
var $author$project$Format$Version1$toNextEdge = function (_v0) {
	var label = _v0.label;
	var style = _v0.style;
	return {
		label: label,
		style: {alignment: 'left', bend: style.bend, dashed: style.dashed, _double: style._double, head: style.head, position: 0.5, tail: style.tail}
	};
};
var $author$project$Format$Version1$toNextVersion = function (_v0) {
	var nodes = _v0.nodes;
	var edges = _v0.edges;
	return {
		edges: A2(
			$elm$core$List$map,
			$author$project$Polygraph$edgeMap($author$project$Format$Version1$toNextEdge),
			edges),
		nodes: nodes
	};
};
var $author$project$Format$Version1$fromJSGraph = function (g) {
	return $author$project$Format$Version2$fromJSGraph(
		$author$project$Format$Version1$toNextVersion(g));
};
var $author$project$Format$Version0$toNextEdge = function (_v0) {
	var label = _v0.label;
	var style = _v0.style;
	var bend = _v0.bend;
	return {
		label: label,
		style: {bend: bend, dashed: style.dashed, _double: style._double, head: style.head, tail: style.tail}
	};
};
var $author$project$Format$Version0$toNextVersion = function (_v0) {
	var nodes = _v0.a;
	var edges = _v0.b;
	return {
		edges: A2(
			$elm$core$List$map,
			$author$project$Polygraph$edgeMap($author$project$Format$Version0$toNextEdge),
			edges),
		nodes: nodes
	};
};
var $author$project$Format$Version0$fromJSGraph = function (g) {
	return $author$project$Format$Version1$fromJSGraph(
		$author$project$Format$Version0$toNextVersion(g));
};
var $author$project$HtmlDefs$Character = function (a) {
	return {$: 'Character', a: a};
};
var $author$project$HtmlDefs$Control = function (a) {
	return {$: 'Control', a: a};
};
var $author$project$HtmlDefs$toKey = function (string) {
	var _v0 = $elm$core$String$uncons(string);
	if ((_v0.$ === 'Just') && (_v0.a.b === '')) {
		var _v1 = _v0.a;
		var _char = _v1.a;
		return $author$project$HtmlDefs$Character(_char);
	} else {
		return $author$project$HtmlDefs$Control(string);
	}
};
var $author$project$HtmlDefs$keyDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$HtmlDefs$toKey,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$HtmlDefs$keysDecoder = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (alt, ctrl, shift) {
			return {alt: alt, ctrl: ctrl, shift: shift};
		}),
	A2($elm$json$Json$Decode$field, 'altKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'ctrlKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool));
var $author$project$Main$loadedGraph0 = _Platform_incomingPort(
	'loadedGraph0',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (_v0) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (_v1) {
									return $elm$json$Json$Decode$succeed(
										_Utils_Tuple2(_v0, _v1));
								},
								A2(
									$elm$json$Json$Decode$index,
									1,
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (to) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (label) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (id) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (from) {
																		return $elm$json$Json$Decode$succeed(
																			{from: from, id: id, label: label, to: to});
																	},
																	A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
															},
															A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
													},
													A2(
														$elm$json$Json$Decode$field,
														'label',
														A2(
															$elm$json$Json$Decode$andThen,
															function (style) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (label) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (bend) {
																				return $elm$json$Json$Decode$succeed(
																					{bend: bend, label: label, style: style});
																			},
																			A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																	},
																	A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
															},
															A2(
																$elm$json$Json$Decode$field,
																'style',
																A2(
																	$elm$json$Json$Decode$andThen,
																	function (tail) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (head) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (_double) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (dashed) {
																								return $elm$json$Json$Decode$succeed(
																									{dashed: dashed, _double: _double, head: head, tail: tail});
																							},
																							A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																					},
																					A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																			},
																			A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																	},
																	A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))))));
											},
											A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
						},
						A2(
							$elm$json$Json$Decode$index,
							0,
							$elm$json$Json$Decode$list(
								A2(
									$elm$json$Json$Decode$andThen,
									function (label) {
										return A2(
											$elm$json$Json$Decode$andThen,
											function (id) {
												return $elm$json$Json$Decode$succeed(
													{id: id, label: label});
											},
											A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
									},
									A2(
										$elm$json$Json$Decode$field,
										'label',
										A2(
											$elm$json$Json$Decode$andThen,
											function (pos) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (label) {
														return $elm$json$Json$Decode$succeed(
															{label: label, pos: pos});
													},
													A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
											},
											A2(
												$elm$json$Json$Decode$field,
												'pos',
												A2(
													$elm$json$Json$Decode$andThen,
													function (_v0) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (_v1) {
																return $elm$json$Json$Decode$succeed(
																	_Utils_Tuple2(_v0, _v1));
															},
															A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
													},
													A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph1 = _Platform_incomingPort(
	'loadedGraph1',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (nodes) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (edges) {
									return $elm$json$Json$Decode$succeed(
										{edges: edges, nodes: nodes});
								},
								A2(
									$elm$json$Json$Decode$field,
									'edges',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (to) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (label) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (id) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (from) {
																		return $elm$json$Json$Decode$succeed(
																			{from: from, id: id, label: label, to: to});
																	},
																	A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
															},
															A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
													},
													A2(
														$elm$json$Json$Decode$field,
														'label',
														A2(
															$elm$json$Json$Decode$andThen,
															function (style) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (label) {
																		return $elm$json$Json$Decode$succeed(
																			{label: label, style: style});
																	},
																	A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
															},
															A2(
																$elm$json$Json$Decode$field,
																'style',
																A2(
																	$elm$json$Json$Decode$andThen,
																	function (tail) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (head) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (_double) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (dashed) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (bend) {
																										return $elm$json$Json$Decode$succeed(
																											{bend: bend, dashed: dashed, _double: _double, head: head, tail: tail});
																									},
																									A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																							},
																							A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																					},
																					A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																			},
																			A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																	},
																	A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))))));
											},
											A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
						},
						A2(
							$elm$json$Json$Decode$field,
							'nodes',
							$elm$json$Json$Decode$list(
								A2(
									$elm$json$Json$Decode$andThen,
									function (label) {
										return A2(
											$elm$json$Json$Decode$andThen,
											function (id) {
												return $elm$json$Json$Decode$succeed(
													{id: id, label: label});
											},
											A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
									},
									A2(
										$elm$json$Json$Decode$field,
										'label',
										A2(
											$elm$json$Json$Decode$andThen,
											function (pos) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (label) {
														return $elm$json$Json$Decode$succeed(
															{label: label, pos: pos});
													},
													A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
											},
											A2(
												$elm$json$Json$Decode$field,
												'pos',
												A2(
													$elm$json$Json$Decode$andThen,
													function (_v0) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (_v1) {
																return $elm$json$Json$Decode$succeed(
																	_Utils_Tuple2(_v0, _v1));
															},
															A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
													},
													A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph2 = _Platform_incomingPort(
	'loadedGraph2',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (nodes) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (edges) {
									return $elm$json$Json$Decode$succeed(
										{edges: edges, nodes: nodes});
								},
								A2(
									$elm$json$Json$Decode$field,
									'edges',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (to) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (label) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (id) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (from) {
																		return $elm$json$Json$Decode$succeed(
																			{from: from, id: id, label: label, to: to});
																	},
																	A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
															},
															A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
													},
													A2(
														$elm$json$Json$Decode$field,
														'label',
														A2(
															$elm$json$Json$Decode$andThen,
															function (style) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (label) {
																		return $elm$json$Json$Decode$succeed(
																			{label: label, style: style});
																	},
																	A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
															},
															A2(
																$elm$json$Json$Decode$field,
																'style',
																A2(
																	$elm$json$Json$Decode$andThen,
																	function (tail) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (position) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (head) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (_double) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (dashed) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (bend) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (alignment) {
																														return $elm$json$Json$Decode$succeed(
																															{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																													},
																													A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																											},
																											A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																									},
																									A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																							},
																							A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																					},
																					A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																			},
																			A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																	},
																	A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))))));
											},
											A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
						},
						A2(
							$elm$json$Json$Decode$field,
							'nodes',
							$elm$json$Json$Decode$list(
								A2(
									$elm$json$Json$Decode$andThen,
									function (label) {
										return A2(
											$elm$json$Json$Decode$andThen,
											function (id) {
												return $elm$json$Json$Decode$succeed(
													{id: id, label: label});
											},
											A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
									},
									A2(
										$elm$json$Json$Decode$field,
										'label',
										A2(
											$elm$json$Json$Decode$andThen,
											function (pos) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (label) {
														return $elm$json$Json$Decode$succeed(
															{label: label, pos: pos});
													},
													A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
											},
											A2(
												$elm$json$Json$Decode$field,
												'pos',
												A2(
													$elm$json$Json$Decode$andThen,
													function (_v0) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (_v1) {
																return $elm$json$Json$Decode$succeed(
																	_Utils_Tuple2(_v0, _v1));
															},
															A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
													},
													A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph3 = _Platform_incomingPort(
	'loadedGraph3',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (sizeGrid) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (nodes) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (edges) {
											return $elm$json$Json$Decode$succeed(
												{edges: edges, nodes: nodes, sizeGrid: sizeGrid});
										},
										A2(
											$elm$json$Json$Decode$field,
											'edges',
											$elm$json$Json$Decode$list(
												A2(
													$elm$json$Json$Decode$andThen,
													function (to) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (id) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (from) {
																				return $elm$json$Json$Decode$succeed(
																					{from: from, id: id, label: label, to: to});
																			},
																			A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
																	},
																	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
															},
															A2(
																$elm$json$Json$Decode$field,
																'label',
																A2(
																	$elm$json$Json$Decode$andThen,
																	function (style) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (label) {
																				return $elm$json$Json$Decode$succeed(
																					{label: label, style: style});
																			},
																			A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'style',
																		A2(
																			$elm$json$Json$Decode$andThen,
																			function (tail) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (position) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (head) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (_double) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (dashed) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (bend) {
																														return A2(
																															$elm$json$Json$Decode$andThen,
																															function (alignment) {
																																return $elm$json$Json$Decode$succeed(
																																	{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																															},
																															A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																													},
																													A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																											},
																											A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																									},
																									A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																							},
																							A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																					},
																					A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																			},
																			A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))))));
													},
													A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
								},
								A2(
									$elm$json$Json$Decode$field,
									'nodes',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (label) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (id) {
														return $elm$json$Json$Decode$succeed(
															{id: id, label: label});
													},
													A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
											},
											A2(
												$elm$json$Json$Decode$field,
												'label',
												A2(
													$elm$json$Json$Decode$andThen,
													function (pos) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return $elm$json$Json$Decode$succeed(
																	{label: label, pos: pos});
															},
															A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
													},
													A2(
														$elm$json$Json$Decode$field,
														'pos',
														A2(
															$elm$json$Json$Decode$andThen,
															function (_v0) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (_v1) {
																		return $elm$json$Json$Decode$succeed(
																			_Utils_Tuple2(_v0, _v1));
																	},
																	A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
															},
															A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))));
						},
						A2($elm$json$Json$Decode$field, 'sizeGrid', $elm$json$Json$Decode$int))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph4 = _Platform_incomingPort(
	'loadedGraph4',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (sizeGrid) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (nodes) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (edges) {
											return $elm$json$Json$Decode$succeed(
												{edges: edges, nodes: nodes, sizeGrid: sizeGrid});
										},
										A2(
											$elm$json$Json$Decode$field,
											'edges',
											$elm$json$Json$Decode$list(
												A2(
													$elm$json$Json$Decode$andThen,
													function (to) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (id) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (from) {
																				return $elm$json$Json$Decode$succeed(
																					{from: from, id: id, label: label, to: to});
																			},
																			A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
																	},
																	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
															},
															A2(
																$elm$json$Json$Decode$field,
																'label',
																A2(
																	$elm$json$Json$Decode$andThen,
																	function (style) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (label) {
																				return $elm$json$Json$Decode$succeed(
																					{label: label, style: style});
																			},
																			A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'style',
																		A2(
																			$elm$json$Json$Decode$andThen,
																			function (tail) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (position) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (head) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (_double) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (dashed) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (bend) {
																														return A2(
																															$elm$json$Json$Decode$andThen,
																															function (alignment) {
																																return $elm$json$Json$Decode$succeed(
																																	{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																															},
																															A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																													},
																													A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																											},
																											A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																									},
																									A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																							},
																							A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																					},
																					A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																			},
																			A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))))));
													},
													A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
								},
								A2(
									$elm$json$Json$Decode$field,
									'nodes',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (label) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (id) {
														return $elm$json$Json$Decode$succeed(
															{id: id, label: label});
													},
													A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
											},
											A2(
												$elm$json$Json$Decode$field,
												'label',
												A2(
													$elm$json$Json$Decode$andThen,
													function (pos) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (isMath) {
																		return $elm$json$Json$Decode$succeed(
																			{isMath: isMath, label: label, pos: pos});
																	},
																	A2($elm$json$Json$Decode$field, 'isMath', $elm$json$Json$Decode$bool));
															},
															A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
													},
													A2(
														$elm$json$Json$Decode$field,
														'pos',
														A2(
															$elm$json$Json$Decode$andThen,
															function (_v0) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (_v1) {
																		return $elm$json$Json$Decode$succeed(
																			_Utils_Tuple2(_v0, _v1));
																	},
																	A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
															},
															A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))));
						},
						A2($elm$json$Json$Decode$field, 'sizeGrid', $elm$json$Json$Decode$int))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph5 = _Platform_incomingPort(
	'loadedGraph5',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (sizeGrid) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (nodes) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (latexPreamble) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (edges) {
													return $elm$json$Json$Decode$succeed(
														{edges: edges, latexPreamble: latexPreamble, nodes: nodes, sizeGrid: sizeGrid});
												},
												A2(
													$elm$json$Json$Decode$field,
													'edges',
													$elm$json$Json$Decode$list(
														A2(
															$elm$json$Json$Decode$andThen,
															function (to) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (label) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (id) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (from) {
																						return $elm$json$Json$Decode$succeed(
																							{from: from, id: id, label: label, to: to});
																					},
																					A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
																			},
																			A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'label',
																		A2(
																			$elm$json$Json$Decode$andThen,
																			function (style) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (label) {
																						return $elm$json$Json$Decode$succeed(
																							{label: label, style: style});
																					},
																					A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
																			},
																			A2(
																				$elm$json$Json$Decode$field,
																				'style',
																				A2(
																					$elm$json$Json$Decode$andThen,
																					function (tail) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (position) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (head) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (_double) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (dashed) {
																														return A2(
																															$elm$json$Json$Decode$andThen,
																															function (bend) {
																																return A2(
																																	$elm$json$Json$Decode$andThen,
																																	function (alignment) {
																																		return $elm$json$Json$Decode$succeed(
																																			{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																																	},
																																	A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																															},
																															A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																													},
																													A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																											},
																											A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																									},
																									A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																							},
																							A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																					},
																					A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))))));
															},
															A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
										},
										A2($elm$json$Json$Decode$field, 'latexPreamble', $elm$json$Json$Decode$string));
								},
								A2(
									$elm$json$Json$Decode$field,
									'nodes',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (label) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (id) {
														return $elm$json$Json$Decode$succeed(
															{id: id, label: label});
													},
													A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
											},
											A2(
												$elm$json$Json$Decode$field,
												'label',
												A2(
													$elm$json$Json$Decode$andThen,
													function (pos) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (isMath) {
																		return $elm$json$Json$Decode$succeed(
																			{isMath: isMath, label: label, pos: pos});
																	},
																	A2($elm$json$Json$Decode$field, 'isMath', $elm$json$Json$Decode$bool));
															},
															A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
													},
													A2(
														$elm$json$Json$Decode$field,
														'pos',
														A2(
															$elm$json$Json$Decode$andThen,
															function (_v0) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (_v1) {
																		return $elm$json$Json$Decode$succeed(
																			_Utils_Tuple2(_v0, _v1));
																	},
																	A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
															},
															A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))));
						},
						A2($elm$json$Json$Decode$field, 'sizeGrid', $elm$json$Json$Decode$int))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph6 = _Platform_incomingPort(
	'loadedGraph6',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (sizeGrid) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (nodes) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (latexPreamble) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (edges) {
													return $elm$json$Json$Decode$succeed(
														{edges: edges, latexPreamble: latexPreamble, nodes: nodes, sizeGrid: sizeGrid});
												},
												A2(
													$elm$json$Json$Decode$field,
													'edges',
													$elm$json$Json$Decode$list(
														A2(
															$elm$json$Json$Decode$andThen,
															function (to) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (label) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (id) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (from) {
																						return $elm$json$Json$Decode$succeed(
																							{from: from, id: id, label: label, to: to});
																					},
																					A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
																			},
																			A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'label',
																		A2(
																			$elm$json$Json$Decode$andThen,
																			function (style) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (label) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (isPullback) {
																								return $elm$json$Json$Decode$succeed(
																									{isPullback: isPullback, label: label, style: style});
																							},
																							A2($elm$json$Json$Decode$field, 'isPullback', $elm$json$Json$Decode$bool));
																					},
																					A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
																			},
																			A2(
																				$elm$json$Json$Decode$field,
																				'style',
																				A2(
																					$elm$json$Json$Decode$andThen,
																					function (tail) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (position) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (head) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (_double) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (dashed) {
																														return A2(
																															$elm$json$Json$Decode$andThen,
																															function (bend) {
																																return A2(
																																	$elm$json$Json$Decode$andThen,
																																	function (alignment) {
																																		return $elm$json$Json$Decode$succeed(
																																			{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																																	},
																																	A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																															},
																															A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																													},
																													A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																											},
																											A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																									},
																									A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																							},
																							A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																					},
																					A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))))));
															},
															A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
										},
										A2($elm$json$Json$Decode$field, 'latexPreamble', $elm$json$Json$Decode$string));
								},
								A2(
									$elm$json$Json$Decode$field,
									'nodes',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (label) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (id) {
														return $elm$json$Json$Decode$succeed(
															{id: id, label: label});
													},
													A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
											},
											A2(
												$elm$json$Json$Decode$field,
												'label',
												A2(
													$elm$json$Json$Decode$andThen,
													function (pos) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (isMath) {
																		return $elm$json$Json$Decode$succeed(
																			{isMath: isMath, label: label, pos: pos});
																	},
																	A2($elm$json$Json$Decode$field, 'isMath', $elm$json$Json$Decode$bool));
															},
															A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
													},
													A2(
														$elm$json$Json$Decode$field,
														'pos',
														A2(
															$elm$json$Json$Decode$andThen,
															function (_v0) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (_v1) {
																		return $elm$json$Json$Decode$succeed(
																			_Utils_Tuple2(_v0, _v1));
																	},
																	A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
															},
															A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))));
						},
						A2($elm$json$Json$Decode$field, 'sizeGrid', $elm$json$Json$Decode$int))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph7 = _Platform_incomingPort(
	'loadedGraph7',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (sizeGrid) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (nodes) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (latexPreamble) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (edges) {
													return $elm$json$Json$Decode$succeed(
														{edges: edges, latexPreamble: latexPreamble, nodes: nodes, sizeGrid: sizeGrid});
												},
												A2(
													$elm$json$Json$Decode$field,
													'edges',
													$elm$json$Json$Decode$list(
														A2(
															$elm$json$Json$Decode$andThen,
															function (to) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (label) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (id) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (from) {
																						return $elm$json$Json$Decode$succeed(
																							{from: from, id: id, label: label, to: to});
																					},
																					A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
																			},
																			A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'label',
																		A2(
																			$elm$json$Json$Decode$andThen,
																			function (zindex) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (style) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (label) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (isPullback) {
																										return $elm$json$Json$Decode$succeed(
																											{isPullback: isPullback, label: label, style: style, zindex: zindex});
																									},
																									A2($elm$json$Json$Decode$field, 'isPullback', $elm$json$Json$Decode$bool));
																							},
																							A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
																					},
																					A2(
																						$elm$json$Json$Decode$field,
																						'style',
																						A2(
																							$elm$json$Json$Decode$andThen,
																							function (tail) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (position) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (head) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (_double) {
																														return A2(
																															$elm$json$Json$Decode$andThen,
																															function (dashed) {
																																return A2(
																																	$elm$json$Json$Decode$andThen,
																																	function (bend) {
																																		return A2(
																																			$elm$json$Json$Decode$andThen,
																																			function (alignment) {
																																				return $elm$json$Json$Decode$succeed(
																																					{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																																			},
																																			A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																																	},
																																	A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																															},
																															A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																													},
																													A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																											},
																											A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																									},
																									A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																							},
																							A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))));
																			},
																			A2($elm$json$Json$Decode$field, 'zindex', $elm$json$Json$Decode$int))));
															},
															A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
										},
										A2($elm$json$Json$Decode$field, 'latexPreamble', $elm$json$Json$Decode$string));
								},
								A2(
									$elm$json$Json$Decode$field,
									'nodes',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (label) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (id) {
														return $elm$json$Json$Decode$succeed(
															{id: id, label: label});
													},
													A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
											},
											A2(
												$elm$json$Json$Decode$field,
												'label',
												A2(
													$elm$json$Json$Decode$andThen,
													function (pos) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (isMath) {
																		return $elm$json$Json$Decode$succeed(
																			{isMath: isMath, label: label, pos: pos});
																	},
																	A2($elm$json$Json$Decode$field, 'isMath', $elm$json$Json$Decode$bool));
															},
															A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
													},
													A2(
														$elm$json$Json$Decode$field,
														'pos',
														A2(
															$elm$json$Json$Decode$andThen,
															function (_v0) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (_v1) {
																		return $elm$json$Json$Decode$succeed(
																			_Utils_Tuple2(_v0, _v1));
																	},
																	A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
															},
															A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))));
						},
						A2($elm$json$Json$Decode$field, 'sizeGrid', $elm$json$Json$Decode$int))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Main$loadedGraph8 = _Platform_incomingPort(
	'loadedGraph8',
	A2(
		$elm$json$Json$Decode$andThen,
		function (scenario) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (graph) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (fileName) {
							return $elm$json$Json$Decode$succeed(
								{fileName: fileName, graph: graph, scenario: scenario});
						},
						A2($elm$json$Json$Decode$field, 'fileName', $elm$json$Json$Decode$string));
				},
				A2(
					$elm$json$Json$Decode$field,
					'graph',
					A2(
						$elm$json$Json$Decode$andThen,
						function (sizeGrid) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (nodes) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (latexPreamble) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (edges) {
													return $elm$json$Json$Decode$succeed(
														{edges: edges, latexPreamble: latexPreamble, nodes: nodes, sizeGrid: sizeGrid});
												},
												A2(
													$elm$json$Json$Decode$field,
													'edges',
													$elm$json$Json$Decode$list(
														A2(
															$elm$json$Json$Decode$andThen,
															function (to) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (label) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (id) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (from) {
																						return $elm$json$Json$Decode$succeed(
																							{from: from, id: id, label: label, to: to});
																					},
																					A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int));
																			},
																			A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'label',
																		A2(
																			$elm$json$Json$Decode$andThen,
																			function (zindex) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (style) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (label) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (isPullshout) {
																										return $elm$json$Json$Decode$succeed(
																											{isPullshout: isPullshout, label: label, style: style, zindex: zindex});
																									},
																									A2($elm$json$Json$Decode$field, 'isPullshout', $elm$json$Json$Decode$bool));
																							},
																							A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
																					},
																					A2(
																						$elm$json$Json$Decode$field,
																						'style',
																						A2(
																							$elm$json$Json$Decode$andThen,
																							function (tail) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (position) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (head) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (_double) {
																														return A2(
																															$elm$json$Json$Decode$andThen,
																															function (dashed) {
																																return A2(
																																	$elm$json$Json$Decode$andThen,
																																	function (bend) {
																																		return A2(
																																			$elm$json$Json$Decode$andThen,
																																			function (alignment) {
																																				return $elm$json$Json$Decode$succeed(
																																					{alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail});
																																			},
																																			A2($elm$json$Json$Decode$field, 'alignment', $elm$json$Json$Decode$string));
																																	},
																																	A2($elm$json$Json$Decode$field, 'bend', $elm$json$Json$Decode$float));
																															},
																															A2($elm$json$Json$Decode$field, 'dashed', $elm$json$Json$Decode$bool));
																													},
																													A2($elm$json$Json$Decode$field, 'double', $elm$json$Json$Decode$bool));
																											},
																											A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string));
																									},
																									A2($elm$json$Json$Decode$field, 'position', $elm$json$Json$Decode$float));
																							},
																							A2($elm$json$Json$Decode$field, 'tail', $elm$json$Json$Decode$string))));
																			},
																			A2($elm$json$Json$Decode$field, 'zindex', $elm$json$Json$Decode$int))));
															},
															A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int)))));
										},
										A2($elm$json$Json$Decode$field, 'latexPreamble', $elm$json$Json$Decode$string));
								},
								A2(
									$elm$json$Json$Decode$field,
									'nodes',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (label) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (id) {
														return $elm$json$Json$Decode$succeed(
															{id: id, label: label});
													},
													A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
											},
											A2(
												$elm$json$Json$Decode$field,
												'label',
												A2(
													$elm$json$Json$Decode$andThen,
													function (pos) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (label) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (isMath) {
																		return $elm$json$Json$Decode$succeed(
																			{isMath: isMath, label: label, pos: pos});
																	},
																	A2($elm$json$Json$Decode$field, 'isMath', $elm$json$Json$Decode$bool));
															},
															A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string));
													},
													A2(
														$elm$json$Json$Decode$field,
														'pos',
														A2(
															$elm$json$Json$Decode$andThen,
															function (_v0) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (_v1) {
																		return $elm$json$Json$Decode$succeed(
																			_Utils_Tuple2(_v0, _v1));
																	},
																	A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
															},
															A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)))))))));
						},
						A2($elm$json$Json$Decode$field, 'sizeGrid', $elm$json$Json$Decode$int))));
		},
		A2($elm$json$Json$Decode$field, 'scenario', $elm$json$Json$Decode$string)));
var $author$project$Msg$mapLoadGraphInfo = F2(
	function (f, _v0) {
		var graph = _v0.graph;
		var fileName = _v0.fileName;
		var scenario = _v0.scenario;
		return {
			fileName: fileName,
			graph: f(graph),
			scenario: scenario
		};
	});
var $author$project$Msg$noOp = $author$project$Msg$Do($elm$core$Platform$Cmd$none);
var $elm$core$Basics$not = _Basics_not;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onClick = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'click');
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $author$project$Main$onCopy = _Platform_incomingPort(
	'onCopy',
	$elm$json$Json$Decode$null(_Utils_Tuple0));
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$onKeyDownActive = _Platform_incomingPort('onKeyDownActive', $elm$json$Json$Decode$value);
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keyup');
var $author$project$Main$onMouseMoveFromJS = _Platform_incomingPort(
	'onMouseMoveFromJS',
	A2(
		$elm$json$Json$Decode$andThen,
		function (_v0) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (_v1) {
					return $elm$json$Json$Decode$succeed(
						_Utils_Tuple2(_v0, _v1));
				},
				A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
		},
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$float)));
var $author$project$Main$preventDefault = _Platform_outgoingPort('preventDefault', $elm$core$Basics$identity);
var $author$project$Main$promptedEquation = _Platform_incomingPort('promptedEquation', $elm$json$Json$Decode$string);
var $author$project$Main$savedGraph = _Platform_incomingPort('savedGraph', $elm$json$Json$Decode$string);
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Main$subscriptions = function (m) {
	return $elm$core$Platform$Sub$batch(
		_Utils_ap(
			_List_fromArray(
				[
					$author$project$Main$findReplace($author$project$Msg$FindReplace),
					$author$project$Main$loadedGraph0(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version0$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph1(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version1$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph2(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version2$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph3(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version3$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph4(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version4$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph5(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version5$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph6(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version6$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph7(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version7$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$loadedGraph8(
					A2(
						$elm$core$Basics$composeR,
						$author$project$Msg$mapLoadGraphInfo($author$project$Format$Version8$fromJSGraph),
						$author$project$Msg$Loaded)),
					$author$project$Main$clipboardGraph(
					A2($elm$core$Basics$composeR, $author$project$Format$LastVersion$fromJSGraph, $author$project$Msg$PasteGraph)),
					$author$project$Main$savedGraph($author$project$Msg$FileName),
					$elm$browser$Browser$Events$onClick(
					$elm$json$Json$Decode$succeed($author$project$Msg$MouseClick))
				]),
			_Utils_ap(
				m.autoSave ? _List_fromArray(
					[
						A2(
						$elm$time$Time$every,
						60000,
						$elm$core$Basics$always($author$project$Msg$MinuteTick))
					]) : _List_Nil,
				function () {
					var _v0 = m.mode;
					switch (_v0.$) {
						case 'ResizeMode':
							return false;
						case 'QuickInputMode':
							return false;
						default:
							return !m.mouseOnCanvas;
					}
				}() ? _List_Nil : _List_fromArray(
					[
						$elm$browser$Browser$Events$onKeyUp(
						A3(
							$elm$json$Json$Decode$map2,
							$author$project$Msg$KeyChanged(false),
							$author$project$HtmlDefs$keysDecoder,
							$author$project$HtmlDefs$keyDecoder)),
						$author$project$Main$onCopy(
						$elm$core$Basics$always($author$project$Msg$CopyGraph)),
						$author$project$Main$promptedEquation(
						$author$project$Msg$QuickInput(true)),
						$author$project$Main$onMouseMoveFromJS($author$project$Msg$MouseMove),
						$author$project$Main$onKeyDownActive(
						function (e) {
							return A2(
								$elm$core$Result$withDefault,
								$author$project$Msg$noOp,
								A2(
									$elm$json$Json$Decode$decodeValue,
									A3(
										$elm$json$Json$Decode$map2,
										F2(
											function (ks, k) {
												var checkCtrl = (ks.ctrl && _Utils_eq(m.mode, $author$project$Modes$DefaultMode)) ? $author$project$Msg$Do(
													$author$project$Main$preventDefault(e)) : $author$project$Msg$noOp;
												_v1$3:
												while (true) {
													if (k.$ === 'Character') {
														switch (k.a.valueOf()) {
															case '/':
																var _v2 = m.mode;
																switch (_v2.$) {
																	case 'DefaultMode':
																		return $author$project$Msg$Do(
																			$author$project$Main$preventDefault(e));
																	case 'SplitArrow':
																		return $author$project$Msg$Do(
																			$author$project$Main$preventDefault(e));
																	default:
																		return $author$project$Msg$noOp;
																}
															case 'a':
																return checkCtrl;
															default:
																break _v1$3;
														}
													} else {
														if (k.a === 'Tab') {
															var _v3 = m.mode;
															switch (_v3.$) {
																case 'SquareMode':
																	return $author$project$Msg$Do(
																		$author$project$Main$preventDefault(e));
																case 'SplitArrow':
																	return $author$project$Msg$Do(
																		$author$project$Main$preventDefault(e));
																case 'NewArrow':
																	return $author$project$Msg$Do(
																		$author$project$Main$preventDefault(e));
																default:
																	return $author$project$Msg$noOp;
															}
														} else {
															break _v1$3;
														}
													}
												}
												return $author$project$Msg$noOp;
											}),
										$author$project$HtmlDefs$keysDecoder,
										$author$project$HtmlDefs$keyDecoder),
									e));
						})
					]))));
};
var $author$project$Model$noCmd = function (m) {
	return _Utils_Tuple2(m, $elm$core$Platform$Cmd$none);
};
var $author$project$Msg$Exercise1 = {$: 'Exercise1'};
var $author$project$Modes$QuickInputMode = function (a) {
	return {$: 'QuickInputMode', a: a};
};
var $author$project$Model$clearHistory = function (m) {
	return _Utils_update(
		m,
		{history: _List_Nil});
};
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$HtmlDefs$computeLayout = _Platform_outgoingPort(
	'computeLayout',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Polygraph$Edge = F4(
	function (id, from, to, label) {
		return {from: from, id: id, label: label, to: to};
	});
var $author$project$Polygraph$objEdge = F2(
	function (id, o) {
		if (o.$ === 'EdgeObj') {
			var i1 = o.a;
			var i2 = o.b;
			var e = o.c;
			return $elm$core$Maybe$Just(
				{from: i1, id: id, label: e, to: i2});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm_community$intdict$IntDict$foldr = F3(
	function (f, acc, dict) {
		foldr:
		while (true) {
			switch (dict.$) {
				case 'Empty':
					return acc;
				case 'Leaf':
					var l = dict.a;
					return A3(f, l.key, l.value, acc);
				default:
					var i = dict.a;
					var $temp$f = f,
						$temp$acc = A3($elm_community$intdict$IntDict$foldr, f, acc, i.right),
						$temp$dict = i.left;
					f = $temp$f;
					acc = $temp$acc;
					dict = $temp$dict;
					continue foldr;
			}
		}
	});
var $elm_community$intdict$IntDict$toList = function (dict) {
	return A3(
		$elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $author$project$Polygraph$edges = function (_v0) {
	var g = _v0.a;
	var mkEdge = F2(
		function (id, _v2) {
			var i1 = _v2.a;
			var i2 = _v2.b;
			var e = _v2.c;
			return A4($author$project$Polygraph$Edge, id, i1, i2, e);
		});
	return A2(
		$elm$core$List$filterMap,
		function (_v1) {
			var id = _v1.a;
			var e = _v1.b;
			return A2($author$project$Polygraph$objEdge, id, e);
		},
		$elm_community$intdict$IntDict$toList(g));
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$GraphDefs$filterNormalEdges = function (d) {
	if (d.$ === 'PullshoutEdge') {
		return $elm$core$Maybe$Nothing;
	} else {
		var l = d.a;
		return $elm$core$Maybe$Just(l);
	}
};
var $author$project$GraphDefs$mapDetails = F2(
	function (f, e) {
		return {
			details: f(e.details),
			selected: e.selected,
			weaklySelected: e.weaklySelected,
			zindex: e.zindex
		};
	});
var $author$project$GraphDefs$flattenDetails = function (e) {
	return A2(
		$elm$core$Maybe$map,
		function (d) {
			return A2(
				$author$project$GraphDefs$mapDetails,
				$elm$core$Basics$always(d),
				e);
		},
		e.details);
};
var $author$project$GraphDefs$filterLabelNormal = A2(
	$elm$core$Basics$composeR,
	$author$project$GraphDefs$mapDetails($author$project$GraphDefs$filterNormalEdges),
	$author$project$GraphDefs$flattenDetails);
var $author$project$IntDictExtra$filterMap = F2(
	function (f, d) {
		return $elm_community$intdict$IntDict$fromList(
			A2(
				$elm$core$List$filterMap,
				function (_v0) {
					var i = _v0.a;
					var o = _v0.b;
					return A2(
						$elm$core$Maybe$map,
						function (b) {
							return _Utils_Tuple2(i, b);
						},
						A2(f, i, o));
				},
				$elm_community$intdict$IntDict$toList(d)));
	});
var $author$project$Polygraph$rawFilterMapIds = F2(
	function (fn, fe) {
		return $author$project$IntDictExtra$filterMap(
			F2(
				function (_v0, o) {
					if (o.$ === 'EdgeObj') {
						var id1 = o.a;
						var id2 = o.b;
						var e = o.c;
						return A2(
							$elm$core$Maybe$map,
							A2($author$project$Polygraph$EdgeObj, id1, id2),
							A3(fe, id1, id2, e));
					} else {
						var n = o.a;
						return A2(
							$elm$core$Maybe$map,
							$author$project$Polygraph$NodeObj,
							fn(n));
					}
				}));
	});
var $author$project$Polygraph$rawFilterMap = F2(
	function (fn, fe) {
		return A2(
			$author$project$Polygraph$rawFilterMapIds,
			fn,
			F2(
				function (_v0, _v1) {
					return fe;
				}));
	});
var $author$project$Polygraph$Input = function (a) {
	return {$: 'Input', a: a};
};
var $elm_community$intdict$IntDict$keys = function (dict) {
	return A3(
		$elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Polygraph$Output = function (a) {
	return {$: 'Output', a: a};
};
var $author$project$Polygraph$Waiting = F3(
	function (a, b, c) {
		return {$: 'Waiting', a: a, b: b, c: c};
	});
var $elm_community$intdict$IntDict$get = F2(
	function (key, dict) {
		get:
		while (true) {
			switch (dict.$) {
				case 'Empty':
					return $elm$core$Maybe$Nothing;
				case 'Leaf':
					var l = dict.a;
					return _Utils_eq(l.key, key) ? $elm$core$Maybe$Just(l.value) : $elm$core$Maybe$Nothing;
				default:
					var i = dict.a;
					if (!A2($elm_community$intdict$IntDict$prefixMatches, i.prefix, key)) {
						return $elm$core$Maybe$Nothing;
					} else {
						if (A2($elm_community$intdict$IntDict$isBranchingBitSet, i.prefix, key)) {
							var $temp$key = key,
								$temp$dict = i.right;
							key = $temp$key;
							dict = $temp$dict;
							continue get;
						} else {
							var $temp$key = key,
								$temp$dict = i.left;
							key = $temp$key;
							dict = $temp$dict;
							continue get;
						}
					}
			}
		}
	});
var $author$project$Polygraph$mapRecAux = F6(
	function (cn, ce, fn, fe, dict, ids) {
		var getA = function (o) {
			if (o.$ === 'NodeObj') {
				var n = o.a;
				return cn(n);
			} else {
				var e = o.c;
				return ce(e);
			}
		};
		var rec = A4($author$project$Polygraph$mapRecAux, cn, ce, fn, fe);
		var ins = F2(
			function (id, o) {
				return A3($elm_community$intdict$IntDict$insert, id, o, dict);
			});
		if (!ids.b) {
			return dict;
		} else {
			var id = ids.a;
			var tailIds = ids.b;
			var _v1 = A2($elm_community$intdict$IntDict$get, id, dict);
			_v1$3:
			while (true) {
				if (_v1.$ === 'Just') {
					switch (_v1.a.$) {
						case 'Input':
							if (_v1.a.a.$ === 'NodeObj') {
								var n = _v1.a.a.a;
								return A2(
									rec,
									A2(
										ins,
										id,
										$author$project$Polygraph$Output(
											$author$project$Polygraph$NodeObj(
												A2(fn, id, n)))),
									tailIds);
							} else {
								var _v2 = _v1.a.a;
								var i1 = _v2.a;
								var i2 = _v2.b;
								var e = _v2.c;
								return A2(
									rec,
									A2(
										ins,
										id,
										A3($author$project$Polygraph$Waiting, i1, i2, e)),
									A2(
										$elm$core$List$cons,
										i1,
										A2(
											$elm$core$List$cons,
											i2,
											A2($elm$core$List$cons, id, tailIds))));
							}
						case 'Waiting':
							var _v3 = _v1.a;
							var i1 = _v3.a;
							var i2 = _v3.b;
							var e = _v3.c;
							var _v4 = _Utils_Tuple2(
								A2($elm_community$intdict$IntDict$get, i1, dict),
								A2($elm_community$intdict$IntDict$get, i2, dict));
							if ((((_v4.a.$ === 'Just') && (_v4.a.a.$ === 'Output')) && (_v4.b.$ === 'Just')) && (_v4.b.a.$ === 'Output')) {
								var o1 = _v4.a.a.a;
								var o2 = _v4.b.a.a;
								var a2 = getA(o2);
								var a1 = getA(o1);
								return A2(
									rec,
									A2(
										ins,
										id,
										$author$project$Polygraph$Output(
											A3(
												$author$project$Polygraph$EdgeObj,
												i1,
												i2,
												A4(fe, id, a1, a2, e)))),
									tailIds);
							} else {
								return A2(rec, dict, tailIds);
							}
						default:
							break _v1$3;
					}
				} else {
					break _v1$3;
				}
			}
			return A2(rec, dict, tailIds);
		}
	});
var $author$project$Polygraph$invalidEdges = function (_v0) {
	var g = _v0.a;
	var dict = A6(
		$author$project$Polygraph$mapRecAux,
		$elm$core$Basics$always(_Utils_Tuple0),
		$elm$core$Basics$always(_Utils_Tuple0),
		$elm$core$Basics$always($elm$core$Basics$identity),
		F3(
			function (_v3, _v4, _v5) {
				return $elm$core$Basics$identity;
			}),
		A2(
			$elm_community$intdict$IntDict$map,
			function (_v6) {
				return $author$project$Polygraph$Input;
			},
			g),
		$elm_community$intdict$IntDict$keys(g));
	var l = $elm_community$intdict$IntDict$toList(dict);
	var missings = A2(
		$elm$core$List$filterMap,
		function (_v1) {
			var id = _v1.a;
			var o = _v1.b;
			if (o.$ === 'Waiting') {
				var i1 = o.a;
				var i2 = o.b;
				var e = o.c;
				return $elm$core$Maybe$Just(
					{from: i1, id: id, label: e, to: i2});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		},
		l);
	return missings;
};
var $elm_community$intdict$IntDict$remove = F2(
	function (key, dict) {
		return A3(
			$elm_community$intdict$IntDict$update,
			key,
			$elm$core$Basics$always($elm$core$Maybe$Nothing),
			dict);
	});
var $author$project$IntDictExtra$removeList = F2(
	function (l, d) {
		return A3($elm$core$List$foldl, $elm_community$intdict$IntDict$remove, d, l);
	});
var $author$project$Polygraph$sanitise = function (g) {
	var d = g.a;
	var ids = A2(
		$elm$core$List$map,
		function ($) {
			return $.id;
		},
		$author$project$Polygraph$invalidEdges(g));
	return $author$project$Polygraph$Graph(
		A2($author$project$IntDictExtra$removeList, ids, d));
};
var $author$project$Polygraph$filterMap = F3(
	function (fn, fe, _v0) {
		var g = _v0.a;
		var g2 = A3($author$project$Polygraph$rawFilterMap, fn, fe, g);
		return $author$project$Polygraph$sanitise(
			$author$project$Polygraph$Graph(g2));
	});
var $author$project$GraphDefs$keepNormalEdges = A2($author$project$Polygraph$filterMap, $elm$core$Maybe$Just, $author$project$GraphDefs$filterLabelNormal);
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Polygraph$Node = F2(
	function (id, label) {
		return {id: id, label: label};
	});
var $author$project$Polygraph$objNode = function (o) {
	if (o.$ === 'NodeObj') {
		var n = o.a;
		return $elm$core$Maybe$Just(n);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Polygraph$nodes = function (_v0) {
	var g = _v0.a;
	return A2(
		$elm$core$List$filterMap,
		function (_v1) {
			var id = _v1.a;
			var n = _v1.b;
			return A2(
				$elm$core$Maybe$map,
				$author$project$Polygraph$Node(id),
				$author$project$Polygraph$objNode(n));
		},
		$elm_community$intdict$IntDict$toList(g));
};
var $author$project$Polygraph$mapRec = F6(
	function (cn, ce, fn, fe, ids, _v0) {
		var g = _v0.a;
		var dict = A6(
			$author$project$Polygraph$mapRecAux,
			cn,
			ce,
			fn,
			fe,
			A2(
				$elm_community$intdict$IntDict$map,
				function (_v2) {
					return $author$project$Polygraph$Input;
				},
				g),
			ids);
		var gf = A2(
			$author$project$IntDictExtra$filterMap,
			F2(
				function (id, o) {
					if (o.$ === 'Output') {
						var o2 = o.a;
						return $elm$core$Maybe$Just(o2);
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}),
			dict);
		return $author$project$Polygraph$Graph(gf);
	});
var $author$project$Polygraph$mapRecAll = F5(
	function (cn, ce, fn, fe, _v0) {
		var g = _v0.a;
		return A6(
			$author$project$Polygraph$mapRec,
			cn,
			ce,
			fn,
			fe,
			$elm_community$intdict$IntDict$keys(g),
			$author$project$Polygraph$Graph(g));
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Polygraph$computeDimensions = A4(
	$author$project$Polygraph$mapRecAll,
	$elm$core$Basics$always(0),
	$elm$core$Tuple$second,
	$elm$core$Basics$always($elm$core$Basics$identity),
	F4(
		function (_v0, n1, n2, e) {
			return _Utils_Tuple2(
				e,
				1 + A2($elm$core$Basics$max, n1, n2));
		}));
var $author$project$Polygraph$graphRep = function (_v0) {
	var g = _v0.a;
	return g;
};
var $author$project$Polygraph$normalise = function (g) {
	var getDim = function (_v7) {
		var o = _v7.b;
		if (o.$ === 'NodeObj') {
			return 0;
		} else {
			var _v6 = o.c;
			var dim = _v6.b;
			return dim;
		}
	};
	var gWithDims = A2(
		$elm$core$List$sortBy,
		getDim,
		$elm_community$intdict$IntDict$toList(
			$author$project$Polygraph$graphRep(
				$author$project$Polygraph$computeDimensions(g))));
	var idDict = A3(
		$elm$core$List$foldl,
		F2(
			function (_v4, d) {
				var id = _v4.a;
				return A3(
					$elm_community$intdict$IntDict$insert,
					id,
					$elm_community$intdict$IntDict$size(d),
					d);
			}),
		$elm_community$intdict$IntDict$empty,
		gWithDims);
	var getId = function (id) {
		var _v3 = A2($elm_community$intdict$IntDict$get, id, idDict);
		if (_v3.$ === 'Nothing') {
			return id;
		} else {
			var i = _v3.a;
			return i;
		}
	};
	var updateId = function (o) {
		if (o.$ === 'NodeObj') {
			var l = o.a;
			return $author$project$Polygraph$NodeObj(l);
		} else {
			var i1 = o.a;
			var i2 = o.b;
			var _v2 = o.c;
			var e = _v2.a;
			var dim = _v2.b;
			return A3(
				$author$project$Polygraph$EdgeObj,
				getId(i1),
				getId(i2),
				e);
		}
	};
	return $author$project$Polygraph$Graph(
		$elm_community$intdict$IntDict$fromList(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var id = _v0.a;
					var o = _v0.b;
					return _Utils_Tuple2(
						getId(id),
						updateId(o));
				},
				gWithDims)));
};
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$ArrowStyle$quiverStyle = function (st) {
	var _v0 = st;
	var tail = _v0.tail;
	var head = _v0.head;
	var _double = _v0._double;
	var dashed = _v0.dashed;
	var makeIf = F2(
		function (b, x) {
			return b ? _List_fromArray(
				[x]) : _List_Nil;
		});
	var headStyle = function () {
		switch (head.$) {
			case 'DefaultHead':
				return _List_Nil;
			case 'TwoHeads':
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'head',
						_List_fromArray(
							[
								_Utils_Tuple2('name', 'epi')
							]))
					]);
			default:
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'head',
						_List_fromArray(
							[
								_Utils_Tuple2('name', 'none')
							]))
					]);
		}
	}();
	var tailStyle = function () {
		switch (tail.$) {
			case 'DefaultTail':
				return _List_Nil;
			case 'Hook':
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'tail',
						_List_fromArray(
							[
								_Utils_Tuple2('name', 'hook'),
								_Utils_Tuple2('side', 'top')
							]))
					]);
			default:
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'tail',
						_List_fromArray(
							[
								_Utils_Tuple2('name', 'hook'),
								_Utils_Tuple2('side', 'bottom')
							]))
					]);
		}
	}();
	var style = A2(
		$elm$core$List$map,
		function (_v1) {
			var x = _v1.a;
			var y = _v1.b;
			return _Utils_Tuple2(
				x,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$map,
						function (_v2) {
							var s = _v2.a;
							var l = _v2.b;
							return _Utils_Tuple2(
								s,
								$elm$json$Json$Encode$string(l));
						},
						y)));
		},
		_Utils_ap(
			headStyle,
			_Utils_ap(
				tailStyle,
				A2(
					makeIf,
					dashed,
					_Utils_Tuple2(
						'body',
						_List_fromArray(
							[
								_Utils_Tuple2('name', 'dashed')
							]))))));
	return _Utils_ap(
		A2(
			makeIf,
			_double,
			_Utils_Tuple2(
				'level',
				$elm$json$Json$Encode$int(2))),
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'style',
					$elm$json$Json$Encode$object(style))
				]),
			_Utils_ap(
				A2(
					makeIf,
					!(!st.bend),
					_Utils_Tuple2(
						'curve',
						$elm$json$Json$Encode$int(
							$elm$core$Basics$floor(st.bend * 10)))),
				A2(
					makeIf,
					st.labelPosition !== 0.5,
					_Utils_Tuple2(
						'label_position',
						$elm$json$Json$Encode$int(
							$elm$core$Basics$floor(st.labelPosition * 100)))))));
};
var $author$project$GraphDefs$exportQuiver = F2(
	function (sizeGrid, g) {
		var gnorm = $author$project$Polygraph$normalise(
			$author$project$GraphDefs$keepNormalEdges(g));
		var nodes = $author$project$Polygraph$nodes(gnorm);
		var edges = $author$project$Polygraph$edges(gnorm);
		var coordInt = function (x) {
			return $elm$json$Json$Encode$int(
				$elm$core$Basics$floor(x / sizeGrid));
		};
		var encodePos = function (_v0) {
			var x = _v0.a;
			var y = _v0.b;
			return _List_fromArray(
				[
					coordInt(x),
					coordInt(y)
				]);
		};
		var encodeNode = function (n) {
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_Utils_ap(
					encodePos(n.label.pos),
					_List_fromArray(
						[
							$elm$json$Json$Encode$string(
							(n.label.label === '') ? '\\bullet' : n.label.label)
						])));
		};
		var encodeEdge = function (e) {
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$int(e.from),
						$elm$json$Json$Encode$int(e.to),
						$elm$json$Json$Encode$string(e.label.details.label),
						$elm$json$Json$Encode$int(
						_Utils_eq(e.label.details.style.labelAlignment, $author$project$ArrowStyle$Right) ? 2 : 0),
						$elm$json$Json$Encode$object(
						$author$project$ArrowStyle$quiverStyle(e.label.details.style))
					]));
		};
		var jnodes = A2($elm$core$List$map, encodeNode, nodes);
		var jedges = A2($elm$core$List$map, encodeEdge, edges);
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$json$Json$Encode$int(0),
						$elm$json$Json$Encode$int(
						$elm$core$List$length(nodes))
					]),
				_Utils_ap(jnodes, jedges)));
	});
var $author$project$Main$exportQuiver = _Platform_outgoingPort('exportQuiver', $elm$core$Basics$identity);
var $elm_community$intdict$IntDict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			switch (dict.$) {
				case 'Empty':
					return acc;
				case 'Leaf':
					var l = dict.a;
					return A3(f, l.key, l.value, acc);
				default:
					var i = dict.a;
					var $temp$f = f,
						$temp$acc = A3($elm_community$intdict$IntDict$foldl, f, acc, i.left),
						$temp$dict = i.right;
					f = $temp$f;
					acc = $temp$acc;
					dict = $temp$dict;
					continue foldl;
			}
		}
	});
var $elm_community$intdict$IntDict$filter = F2(
	function (predicate, dict) {
		var add = F3(
			function (k, v, d) {
				return A2(predicate, k, v) ? A3($elm_community$intdict$IntDict$insert, k, v, d) : d;
			});
		return A3($elm_community$intdict$IntDict$foldl, add, $elm_community$intdict$IntDict$empty, dict);
	});
var $elm_community$intdict$IntDict$isEmpty = function (dict) {
	if (dict.$ === 'Empty') {
		return true;
	} else {
		return false;
	}
};
var $author$project$IntDictExtra$any = F2(
	function (f, d) {
		return !$elm_community$intdict$IntDict$isEmpty(
			A2(
				$elm_community$intdict$IntDict$filter,
				$elm$core$Basics$always(f),
				d));
	});
var $author$project$Polygraph$any = F3(
	function (fn, fe, _v0) {
		var g = _v0.a;
		return A2(
			$author$project$IntDictExtra$any,
			function (o) {
				if (o.$ === 'NodeObj') {
					var n = o.a;
					return fn(n);
				} else {
					var e = o.c;
					return fe(e);
				}
			},
			g);
	});
var $author$project$GraphDefs$fieldSelect = function (g) {
	return A3(
		$author$project$Polygraph$any,
		function ($) {
			return $.selected;
		},
		function ($) {
			return $.selected;
		},
		g) ? function ($) {
		return $.selected;
	} : function ($) {
		return $.weaklySelected;
	};
};
var $author$project$GraphDefs$mapEdgeType = F2(
	function (f, e) {
		if (e.$ === 'PullshoutEdge') {
			return $author$project$GraphDefs$PullshoutEdge;
		} else {
			var l = e.a;
			return $author$project$GraphDefs$NormalEdge(
				f(l));
		}
	});
var $author$project$GraphDefs$mapNormalEdge = A2($elm$core$Basics$composeR, $author$project$GraphDefs$mapEdgeType, $author$project$GraphDefs$mapDetails);
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $author$project$GraphDefs$findReplaceInSelected = F2(
	function (g, r) {
		var repl = F2(
			function (sel, s) {
				return sel ? A3($elm$core$String$replace, r.search, r.replace, s) : s;
			});
		var f = $author$project$GraphDefs$fieldSelect(g);
		return A3(
			$author$project$Polygraph$map,
			F2(
				function (_v0, n) {
					return _Utils_update(
						n,
						{
							label: A2(
								repl,
								f(n),
								n.label)
						});
				}),
			F2(
				function (_v1, e) {
					return A2(
						$author$project$GraphDefs$mapNormalEdge,
						function (l) {
							return _Utils_update(
								l,
								{
									label: A2(
										repl,
										f(e),
										l.label)
								});
						},
						e);
				}),
			g);
	});
var $author$project$Main$onMouseMove = _Platform_outgoingPort('onMouseMove', $elm$core$Basics$identity);
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$json$Json$Encode$float = _Json_wrap;
var $author$project$Main$quicksaveGraph = _Platform_outgoingPort(
	'quicksaveGraph',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'feedback',
					$elm$json$Json$Encode$bool($.feedback)),
					_Utils_Tuple2(
					'info',
					function ($) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'fileName',
									$elm$json$Json$Encode$string($.fileName)),
									_Utils_Tuple2(
									'graph',
									function ($) {
										return $elm$json$Json$Encode$object(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'edges',
													$elm$json$Json$Encode$list(
														function ($) {
															return $elm$json$Json$Encode$object(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'from',
																		$elm$json$Json$Encode$int($.from)),
																		_Utils_Tuple2(
																		'id',
																		$elm$json$Json$Encode$int($.id)),
																		_Utils_Tuple2(
																		'label',
																		function ($) {
																			return $elm$json$Json$Encode$object(
																				_List_fromArray(
																					[
																						_Utils_Tuple2(
																						'isPullshout',
																						$elm$json$Json$Encode$bool($.isPullshout)),
																						_Utils_Tuple2(
																						'label',
																						$elm$json$Json$Encode$string($.label)),
																						_Utils_Tuple2(
																						'style',
																						function ($) {
																							return $elm$json$Json$Encode$object(
																								_List_fromArray(
																									[
																										_Utils_Tuple2(
																										'alignment',
																										$elm$json$Json$Encode$string($.alignment)),
																										_Utils_Tuple2(
																										'bend',
																										$elm$json$Json$Encode$float($.bend)),
																										_Utils_Tuple2(
																										'dashed',
																										$elm$json$Json$Encode$bool($.dashed)),
																										_Utils_Tuple2(
																										'double',
																										$elm$json$Json$Encode$bool($._double)),
																										_Utils_Tuple2(
																										'head',
																										$elm$json$Json$Encode$string($.head)),
																										_Utils_Tuple2(
																										'position',
																										$elm$json$Json$Encode$float($.position)),
																										_Utils_Tuple2(
																										'tail',
																										$elm$json$Json$Encode$string($.tail))
																									]));
																						}($.style)),
																						_Utils_Tuple2(
																						'zindex',
																						$elm$json$Json$Encode$int($.zindex))
																					]));
																		}($.label)),
																		_Utils_Tuple2(
																		'to',
																		$elm$json$Json$Encode$int($.to))
																	]));
														})($.edges)),
													_Utils_Tuple2(
													'latexPreamble',
													$elm$json$Json$Encode$string($.latexPreamble)),
													_Utils_Tuple2(
													'nodes',
													$elm$json$Json$Encode$list(
														function ($) {
															return $elm$json$Json$Encode$object(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'id',
																		$elm$json$Json$Encode$int($.id)),
																		_Utils_Tuple2(
																		'label',
																		function ($) {
																			return $elm$json$Json$Encode$object(
																				_List_fromArray(
																					[
																						_Utils_Tuple2(
																						'isMath',
																						$elm$json$Json$Encode$bool($.isMath)),
																						_Utils_Tuple2(
																						'label',
																						$elm$json$Json$Encode$string($.label)),
																						_Utils_Tuple2(
																						'pos',
																						function ($) {
																							var a = $.a;
																							var b = $.b;
																							return A2(
																								$elm$json$Json$Encode$list,
																								$elm$core$Basics$identity,
																								_List_fromArray(
																									[
																										$elm$json$Json$Encode$float(a),
																										$elm$json$Json$Encode$float(b)
																									]));
																						}($.pos))
																					]));
																		}($.label))
																	]));
														})($.nodes)),
													_Utils_Tuple2(
													'sizeGrid',
													$elm$json$Json$Encode$int($.sizeGrid))
												]));
									}($.graph)),
									_Utils_Tuple2(
									'version',
									$elm$json$Json$Encode$int($.version))
								]));
					}($.info))
				]));
	});
var $author$project$Main$saveGraph = _Platform_outgoingPort(
	'saveGraph',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'fileName',
					$elm$json$Json$Encode$string($.fileName)),
					_Utils_Tuple2(
					'graph',
					function ($) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'edges',
									$elm$json$Json$Encode$list(
										function ($) {
											return $elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'from',
														$elm$json$Json$Encode$int($.from)),
														_Utils_Tuple2(
														'id',
														$elm$json$Json$Encode$int($.id)),
														_Utils_Tuple2(
														'label',
														function ($) {
															return $elm$json$Json$Encode$object(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'isPullshout',
																		$elm$json$Json$Encode$bool($.isPullshout)),
																		_Utils_Tuple2(
																		'label',
																		$elm$json$Json$Encode$string($.label)),
																		_Utils_Tuple2(
																		'style',
																		function ($) {
																			return $elm$json$Json$Encode$object(
																				_List_fromArray(
																					[
																						_Utils_Tuple2(
																						'alignment',
																						$elm$json$Json$Encode$string($.alignment)),
																						_Utils_Tuple2(
																						'bend',
																						$elm$json$Json$Encode$float($.bend)),
																						_Utils_Tuple2(
																						'dashed',
																						$elm$json$Json$Encode$bool($.dashed)),
																						_Utils_Tuple2(
																						'double',
																						$elm$json$Json$Encode$bool($._double)),
																						_Utils_Tuple2(
																						'head',
																						$elm$json$Json$Encode$string($.head)),
																						_Utils_Tuple2(
																						'position',
																						$elm$json$Json$Encode$float($.position)),
																						_Utils_Tuple2(
																						'tail',
																						$elm$json$Json$Encode$string($.tail))
																					]));
																		}($.style)),
																		_Utils_Tuple2(
																		'zindex',
																		$elm$json$Json$Encode$int($.zindex))
																	]));
														}($.label)),
														_Utils_Tuple2(
														'to',
														$elm$json$Json$Encode$int($.to))
													]));
										})($.edges)),
									_Utils_Tuple2(
									'latexPreamble',
									$elm$json$Json$Encode$string($.latexPreamble)),
									_Utils_Tuple2(
									'nodes',
									$elm$json$Json$Encode$list(
										function ($) {
											return $elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'id',
														$elm$json$Json$Encode$int($.id)),
														_Utils_Tuple2(
														'label',
														function ($) {
															return $elm$json$Json$Encode$object(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'isMath',
																		$elm$json$Json$Encode$bool($.isMath)),
																		_Utils_Tuple2(
																		'label',
																		$elm$json$Json$Encode$string($.label)),
																		_Utils_Tuple2(
																		'pos',
																		function ($) {
																			var a = $.a;
																			var b = $.b;
																			return A2(
																				$elm$json$Json$Encode$list,
																				$elm$core$Basics$identity,
																				_List_fromArray(
																					[
																						$elm$json$Json$Encode$float(a),
																						$elm$json$Json$Encode$float(b)
																					]));
																		}($.pos))
																	]));
														}($.label))
													]));
										})($.nodes)),
									_Utils_Tuple2(
									'sizeGrid',
									$elm$json$Json$Encode$int($.sizeGrid))
								]));
					}($.graph)),
					_Utils_Tuple2(
					'version',
					$elm$json$Json$Encode$int($.version))
				]));
	});
var $author$project$Msg$scenarioOfString = function (s) {
	if (s === 'exercise1') {
		return $author$project$Msg$Exercise1;
	} else {
		return $author$project$Msg$Standard;
	}
};
var $elm_community$maybe_extra$Maybe$Extra$filter = F2(
	function (f, m) {
		if (m.$ === 'Just') {
			var a = m.a;
			return f(a) ? m : $elm$core$Maybe$Nothing;
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Polygraph$rawFilterIds = F2(
	function (fn, fe) {
		return A2(
			$author$project$Polygraph$rawFilterMapIds,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$Just,
				$elm_community$maybe_extra$Maybe$Extra$filter(fn)),
			F2(
				function (id1, id2) {
					return A2(
						$elm$core$Basics$composeR,
						$elm$core$Maybe$Just,
						$elm_community$maybe_extra$Maybe$Extra$filter(
							A2(fe, id1, id2)));
				}));
	});
var $author$project$Polygraph$rawFilter = F2(
	function (fn, fe) {
		return A2(
			$author$project$Polygraph$rawFilterIds,
			fn,
			F2(
				function (_v0, _v1) {
					return fe;
				}));
	});
var $author$project$Polygraph$keepBelow = F3(
	function (fn, fe, _v0) {
		var g = _v0.a;
		var g2 = A3($author$project$Polygraph$rawFilter, fn, fe, g);
		var dict = A6(
			$author$project$Polygraph$mapRec,
			$elm$core$Basics$always(_Utils_Tuple0),
			$elm$core$Basics$always(_Utils_Tuple0),
			function (_v1) {
				return $elm$core$Basics$identity;
			},
			F3(
				function (_v2, _v3, _v4) {
					return $elm$core$Basics$identity;
				}),
			$elm_community$intdict$IntDict$keys(g2),
			$author$project$Polygraph$Graph(g));
		return dict;
	});
var $author$project$GraphDefs$selectedGraph = function (g) {
	var f = $author$project$GraphDefs$fieldSelect(g);
	return A3($author$project$Polygraph$keepBelow, f, f, g);
};
var $author$project$Model$depthHistory = 20;
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$Model$toGraphInfo = function (m) {
	return {graph: m.graph, latexPreamble: m.latexPreamble, sizeGrid: m.sizeGrid};
};
var $author$project$Model$pushHistory = function (m) {
	return _Utils_update(
		m,
		{
			history: A2(
				$elm$core$List$take,
				$author$project$Model$depthHistory,
				A2(
					$elm$core$List$cons,
					$author$project$Model$toGraphInfo(m),
					m.history))
		});
};
var $author$project$Model$setSaveGraph = F2(
	function (m, g) {
		var m2 = $author$project$Model$pushHistory(m);
		return _Utils_update(
			m2,
			{graph: g});
	});
var $author$project$ArrowStyle$alignmentToString = function (tail) {
	switch (tail.$) {
		case 'Centre':
			return 'centre';
		case 'Over':
			return 'over';
		case 'Left':
			return 'left';
		default:
			return 'right';
	}
};
var $author$project$ArrowStyle$headToString = function (head) {
	switch (head.$) {
		case 'DefaultHead':
			return 'default';
		case 'TwoHeads':
			return 'twoheads';
		default:
			return 'none';
	}
};
var $author$project$Format$Version8$Edge = F4(
	function (label, style, isPullshout, zindex) {
		return {isPullshout: isPullshout, label: label, style: style, zindex: zindex};
	});
var $author$project$Format$Version8$ArrowStyle = F7(
	function (tail, head, _double, dashed, bend, alignment, position) {
		return {alignment: alignment, bend: bend, dashed: dashed, _double: _double, head: head, position: position, tail: tail};
	});
var $author$project$Format$Version8$emptyArrowStyle = A7($author$project$Format$Version8$ArrowStyle, '', '', false, false, 0, '', 0);
var $author$project$Format$Version8$pullshoutEdge = function (z) {
	return A4($author$project$Format$Version8$Edge, '', $author$project$Format$Version8$emptyArrowStyle, true, z);
};
var $author$project$ArrowStyle$tailToString = function (tail) {
	switch (tail.$) {
		case 'DefaultTail':
			return 'none';
		case 'Hook':
			return 'hook';
		default:
			return 'hookalt';
	}
};
var $author$project$Format$Version8$fromEdgeLabel = function (e) {
	var _v0 = e.details;
	if (_v0.$ === 'PullshoutEdge') {
		return $author$project$Format$Version8$pullshoutEdge(e.zindex);
	} else {
		var label = _v0.a.label;
		var style = _v0.a.style;
		return {
			isPullshout: false,
			label: label,
			style: {
				alignment: $author$project$ArrowStyle$alignmentToString(style.labelAlignment),
				bend: style.bend,
				dashed: style.dashed,
				_double: style._double,
				head: $author$project$ArrowStyle$headToString(style.head),
				position: style.labelPosition,
				tail: $author$project$ArrowStyle$tailToString(style.tail)
			},
			zindex: e.zindex
		};
	}
};
var $author$project$Format$Version8$fromNodeLabel = function (_v0) {
	var pos = _v0.pos;
	var label = _v0.label;
	var isMath = _v0.isMath;
	return {isMath: isMath, label: label, pos: pos};
};
var $author$project$Format$Version8$toJSGraph = function (m) {
	var g = m.graph;
	var gjs = $author$project$Polygraph$normalise(
		A3(
			$author$project$Polygraph$map,
			function (_v0) {
				return $author$project$Format$Version8$fromNodeLabel;
			},
			function (_v1) {
				return $author$project$Format$Version8$fromEdgeLabel;
			},
			g));
	var nodes = $author$project$Polygraph$nodes(gjs);
	var edges = $author$project$Polygraph$edges(gjs);
	return {edges: edges, latexPreamble: m.latexPreamble, nodes: nodes, sizeGrid: m.sizeGrid};
};
var $author$project$Format$LastVersion$toJSGraph = $author$project$Format$Version8$toJSGraph;
var $author$project$Format$Version8$version = 8;
var $author$project$Format$LastVersion$version = $author$project$Format$Version8$version;
var $author$project$Main$toJsGraphInfo = function (model) {
	return {
		fileName: model.fileName,
		graph: $author$project$Format$LastVersion$toJSGraph(
			$author$project$Model$toGraphInfo(model)),
		version: $author$project$Format$LastVersion$version
	};
};
var $author$project$Modes$NewArrow = function (a) {
	return {$: 'NewArrow', a: a};
};
var $author$project$Modes$Pullback = {$: 'Pullback'};
var $author$project$Modes$PullshoutMode = function (a) {
	return {$: 'PullshoutMode', a: a};
};
var $author$project$Modes$Pushout = {$: 'Pushout'};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Polygraph$getEdge = F2(
	function (id, _v0) {
		var g = _v0.a;
		return A2(
			$elm$core$Maybe$andThen,
			$author$project$Polygraph$objEdge(id),
			A2($elm_community$intdict$IntDict$get, id, g));
	});
var $author$project$Polygraph$incomings = F2(
	function (id, g) {
		return A2(
			$elm$core$List$filter,
			function (_v0) {
				var to = _v0.to;
				return _Utils_eq(to, id);
			},
			$author$project$Polygraph$edges(g));
	});
var $author$project$GraphDefs$isPullshout = function (e) {
	return _Utils_eq(e.details, $author$project$GraphDefs$PullshoutEdge);
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm_community$list_extra$List$Extra$notMember = function (x) {
	return A2(
		$elm$core$Basics$composeL,
		$elm$core$Basics$not,
		$elm$core$List$member(x));
};
var $author$project$Polygraph$outgoings = F2(
	function (id, g) {
		return A2(
			$elm$core$List$filter,
			function (_v0) {
				var from = _v0.from;
				return _Utils_eq(from, id);
			},
			$author$project$Polygraph$edges(g));
	});
var $elm_community$list_extra$List$Extra$reverseAppend = F2(
	function (list1, list2) {
		return A3($elm$core$List$foldl, $elm$core$List$cons, list2, list1);
	});
var $elm_community$list_extra$List$Extra$removeHelp = F4(
	function (list, x, xs, previousElements) {
		removeHelp:
		while (true) {
			if (!xs.b) {
				return list;
			} else {
				var y = xs.a;
				var ys = xs.b;
				if (_Utils_eq(x, y)) {
					return A2($elm_community$list_extra$List$Extra$reverseAppend, previousElements, ys);
				} else {
					var $temp$list = list,
						$temp$x = x,
						$temp$xs = ys,
						$temp$previousElements = A2($elm$core$List$cons, y, previousElements);
					list = $temp$list;
					x = $temp$x;
					xs = $temp$xs;
					previousElements = $temp$previousElements;
					continue removeHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$remove = F2(
	function (x, xs) {
		return A4($elm_community$list_extra$List$Extra$removeHelp, xs, x, xs, _List_Nil);
	});
var $author$project$Modes$Pullshout$possibleDests = F3(
	function (g, id, k) {
		var l = A2(
			$elm_community$list_extra$List$Extra$remove,
			id,
			A2(
				$elm$core$List$map,
				function ($) {
					return $.id;
				},
				A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					A2(
						$elm$core$Maybe$map,
						function (n) {
							return A2(
								_Utils_eq(k, $author$project$Modes$Pullback) ? $author$project$Polygraph$outgoings : $author$project$Polygraph$incomings,
								n,
								g);
						},
						A2(
							$elm$core$Maybe$map,
							_Utils_eq(k, $author$project$Modes$Pullback) ? function ($) {
								return $.from;
							} : function ($) {
								return $.to;
							},
							A2($author$project$Polygraph$getEdge, id, g))))));
		var pbks = A2(
			$elm$core$List$map,
			function ($) {
				return $.to;
			},
			A2(
				$elm$core$List$filter,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.label;
					},
					$author$project$GraphDefs$isPullshout),
				A2($author$project$Polygraph$outgoings, id, g)));
		return A2(
			$elm$core$List$filter,
			function (i) {
				return A2($elm_community$list_extra$List$Extra$notMember, i, pbks);
			},
			l);
	});
var $author$project$Modes$Pullshout$initialise = F3(
	function (g, id, k) {
		var _v0 = A3($author$project$Modes$Pullshout$possibleDests, g, id, k);
		if (_v0.b) {
			var t = _v0.a;
			var q = _v0.b;
			return $elm$core$Maybe$Just(
				{chosenEdge: id, currentDest: t, kind: k, possibilities: q, source: 0, target: 0});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$GraphDefs$clearSelection = function (g) {
	return A3(
		$author$project$Polygraph$map,
		F2(
			function (_v0, n) {
				return _Utils_update(
					n,
					{selected: false});
			}),
		F2(
			function (_v1, e) {
				return _Utils_update(
					e,
					{selected: false});
			}),
		g);
};
var $author$project$Polygraph$get = F4(
	function (id, fn, fe, _v0) {
		var g = _v0.a;
		var _v1 = A2($elm_community$intdict$IntDict$get, id, g);
		if (_v1.$ === 'Just') {
			if (_v1.a.$ === 'NodeObj') {
				var n = _v1.a.a;
				return $elm$core$Maybe$Just(
					fn(n));
			} else {
				var _v2 = _v1.a;
				var i1 = _v2.a;
				var i2 = _v2.b;
				var e = _v2.c;
				return $elm$core$Maybe$Just(
					fe(e));
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm_community$maybe_extra$Maybe$Extra$join = function (mx) {
	if (mx.$ === 'Just') {
		var x = mx.a;
		return x;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$GraphDefs$getLabelLabel = F2(
	function (id, g) {
		return $elm_community$maybe_extra$Maybe$Extra$join(
			A4(
				$author$project$Polygraph$get,
				id,
				A2(
					$elm$core$Basics$composeL,
					$elm$core$Maybe$Just,
					function ($) {
						return $.label;
					}),
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.details;
					},
					A2(
						$elm$core$Basics$composeR,
						$author$project$GraphDefs$filterNormalEdges,
						$elm$core$Maybe$map(
							function ($) {
								return $.label;
							}))),
				g));
	});
var $author$project$Modes$RenameMode = F2(
	function (a, b) {
		return {$: 'RenameMode', a: a, b: b};
	});
var $author$project$Model$initialise_RenameModeWithDefault = F3(
	function (save, l, m) {
		if (!l.b) {
			return _Utils_update(
				m,
				{mode: $author$project$Modes$DefaultMode});
		} else {
			return _Utils_update(
				m,
				{
					mode: A2($author$project$Modes$RenameMode, save, l)
				});
		}
	});
var $author$project$Geometry$Point$add = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return _Utils_Tuple2(x1 + x2, y1 + y2);
	});
var $author$project$InputPosition$deltaKeyboardPos = F2(
	function (offsetKeyboardPos, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(x * offsetKeyboardPos, y * offsetKeyboardPos);
	});
var $author$project$Polygraph$getNode = F2(
	function (id, _v0) {
		var g = _v0.a;
		return A2(
			$elm$core$Maybe$andThen,
			$author$project$Polygraph$objNode,
			A2($elm_community$intdict$IntDict$get, id, g));
	});
var $author$project$Model$keyboardPosToPoint = F3(
	function (m, chosenNode, p) {
		var _v0 = A2($author$project$Polygraph$getNode, chosenNode, m.graph);
		if (_v0.$ === 'Nothing') {
			return m.mousePos;
		} else {
			var pos = _v0.a.pos;
			var delta = A2($author$project$InputPosition$deltaKeyboardPos, m.sizeGrid, p);
			return A2($author$project$Geometry$Point$add, pos, delta);
		}
	});
var $author$project$Polygraph$filterNodes = F2(
	function (g, f) {
		return A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeL,
				f,
				function ($) {
					return $.label;
				}),
			$author$project$Polygraph$nodes(g));
	});
var $author$project$GraphDefs$defaultDims = function (s) {
	var height = 16;
	var size = 1;
	return _Utils_Tuple2((height / 2) * size, height);
};
var $author$project$GraphDefs$getNodeDims = function (n) {
	var _v0 = n.dims;
	if (_v0.$ === 'Nothing') {
		return $author$project$GraphDefs$defaultDims(n.label);
	} else {
		var p = _v0.a;
		return p;
	}
};
var $author$project$Geometry$Point$resize = F2(
	function (s, _v0) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		return _Utils_Tuple2(x1 * s, y1 * s);
	});
var $author$project$GraphDefs$getNodePos = function (n) {
	return n.isMath ? n.pos : A2(
		$author$project$Geometry$Point$add,
		n.pos,
		A2(
			$author$project$Geometry$Point$resize,
			0.5,
			$author$project$GraphDefs$getNodeDims(n)));
};
var $author$project$Geometry$isInRect = F2(
	function (_v0, _v1) {
		var topLeft = _v0.topLeft;
		var bottomRight = _v0.bottomRight;
		var x = _v1.a;
		var y = _v1.b;
		var _v2 = bottomRight;
		var x2 = _v2.a;
		var y2 = _v2.b;
		var _v3 = topLeft;
		var x1 = _v3.a;
		var y1 = _v3.b;
		return (_Utils_cmp(x1, x) < 0) && ((_Utils_cmp(x, x2) < 0) && ((_Utils_cmp(y1, y) < 0) && (_Utils_cmp(y, y2) < 0)));
	});
var $author$project$Geometry$Rect = F2(
	function (topLeft, bottomRight) {
		return {bottomRight: bottomRight, topLeft: topLeft};
	});
var $author$project$Geometry$Point$subtract = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return _Utils_Tuple2(x1 - x2, y1 - y2);
	});
var $author$project$Geometry$rectFromPosDims = function (_v0) {
	var pos = _v0.pos;
	var dims = _v0.dims;
	var dims2 = A2($author$project$Geometry$Point$resize, 0.5, dims);
	return A2(
		$author$project$Geometry$Rect,
		A2($author$project$Geometry$Point$subtract, pos, dims2),
		A2($author$project$Geometry$Point$add, pos, dims2));
};
var $author$project$Geometry$isInPosDims = F2(
	function (dims, p) {
		return A2(
			$author$project$Geometry$isInRect,
			$author$project$Geometry$rectFromPosDims(dims),
			p);
	});
var $author$project$GraphDefs$getNodesAt = F2(
	function (g, p) {
		return A2(
			$elm$core$List$map,
			function ($) {
				return $.id;
			},
			A2(
				$author$project$Polygraph$filterNodes,
				g,
				function (n) {
					return A2(
						$author$project$Geometry$isInPosDims,
						{
							dims: $author$project$GraphDefs$getNodeDims(n),
							pos: $author$project$GraphDefs$getNodePos(n)
						},
						p);
				}));
	});
var $author$project$Polygraph$newNode = F2(
	function (g, n) {
		return A2(
			$author$project$Polygraph$newObject,
			g,
			$author$project$Polygraph$NodeObj(n));
	});
var $author$project$Model$mayCreateTargetNodeAt = F3(
	function (m, pos, s) {
		var _v0 = A2($author$project$GraphDefs$getNodesAt, m.graph, pos);
		if (_v0.b && (!_v0.b.b)) {
			var n = _v0.a;
			return _Utils_Tuple2(
				_Utils_Tuple2(m.graph, n),
				false);
		} else {
			return _Utils_Tuple2(
				A2(
					$author$project$Polygraph$newNode,
					m.graph,
					A3($author$project$GraphDefs$newNodeLabel, pos, s, true)),
				true);
		}
	});
var $author$project$Modes$NewArrow$moveNodeInfo = F2(
	function (m, state) {
		var makeInfo = function (pos) {
			return A3($author$project$Model$mayCreateTargetNodeAt, m, pos, '');
		};
		var _v0 = function () {
			var _v2 = state.pos;
			switch (_v2.$) {
				case 'InputPosMouse':
					return makeInfo(m.mousePos);
				case 'InputPosKeyboard':
					var p = _v2.a;
					return makeInfo(
						A3($author$project$Model$keyboardPosToPoint, m, state.chosenNode, p));
				default:
					var id = _v2.a;
					return _Utils_Tuple2(
						_Utils_Tuple2(m.graph, id),
						false);
			}
		}();
		var _v1 = _v0.a;
		var graph = _v1.a;
		var movedNode = _v1.b;
		var created = _v0.b;
		var _v3 = state.inverted ? _Utils_Tuple2(movedNode, state.chosenNode) : _Utils_Tuple2(state.chosenNode, movedNode);
		var source = _v3.a;
		var target = _v3.b;
		var _v4 = A4(
			$author$project$Polygraph$newEdge,
			graph,
			source,
			target,
			A2($author$project$GraphDefs$newEdgeLabel, '', state.style));
		var g = _v4.a;
		var edgeId = _v4.b;
		return {created: created, edgeId: edgeId, graph: g, movedNode: movedNode};
	});
var $author$project$Model$switch_Default = function (m) {
	return $author$project$Model$noCmd(
		_Utils_update(
			m,
			{mode: $author$project$Modes$DefaultMode}));
};
var $author$project$Polygraph$update = F3(
	function (i, fn, fe) {
		return $author$project$Polygraph$mapRep(
			A2(
				$elm_community$intdict$IntDict$update,
				i,
				$elm$core$Maybe$map(
					A2($author$project$Polygraph$mapObj, fn, fe))));
	});
var $author$project$GraphDefs$weaklySelect = function (id) {
	return A2(
		$elm$core$Basics$composeR,
		A2(
			$author$project$Polygraph$map,
			F2(
				function (_v0, n) {
					return _Utils_update(
						n,
						{weaklySelected: false});
				}),
			F2(
				function (_v1, e) {
					return _Utils_update(
						e,
						{weaklySelected: false});
				})),
		A3(
			$author$project$Polygraph$update,
			id,
			function (n) {
				return _Utils_update(
					n,
					{weaklySelected: true});
			},
			function (e) {
				return _Utils_update(
					e,
					{weaklySelected: true});
			}));
};
var $author$project$Modes$NewArrow$nextStep = F3(
	function (model, finish, state) {
		var info = A2($author$project$Modes$NewArrow$moveNodeInfo, model, state);
		var m2 = A2(
			$author$project$Model$setSaveGraph,
			model,
			A2(
				$author$project$GraphDefs$weaklySelect,
				info.movedNode,
				$author$project$GraphDefs$clearSelection(info.graph)));
		if (finish) {
			return $author$project$Model$switch_Default(m2);
		} else {
			var ids = info.created ? _List_fromArray(
				[info.movedNode, info.edgeId]) : _List_fromArray(
				[info.edgeId]);
			var label = A2(
				$elm$core$Maybe$withDefault,
				'',
				A2($author$project$GraphDefs$getLabelLabel, state.chosenNode, info.graph));
			var ids_labels = A2(
				$elm$core$List$map,
				function (id) {
					return _Utils_Tuple2(id, label);
				},
				ids);
			return $author$project$Model$noCmd(
				A3($author$project$Model$initialise_RenameModeWithDefault, false, ids_labels, m2));
		}
	});
var $author$project$InputPosition$InputPosKeyboard = function (a) {
	return {$: 'InputPosKeyboard', a: a};
};
var $author$project$InputPosition$getKeyboardPos = function (pos) {
	if (pos.$ === 'InputPosKeyboard') {
		var p = pos.a;
		return p;
	} else {
		return _Utils_Tuple2(0, 0);
	}
};
var $author$project$InputPosition$InputPosGraph = function (a) {
	return {$: 'InputPosGraph', a: a};
};
var $author$project$InputPosition$InputPosMouse = {$: 'InputPosMouse'};
var $author$project$InputPosition$updateNoKeyboard = F2(
	function (pos, msg) {
		switch (msg.$) {
			case 'MouseMove':
				return $author$project$InputPosition$InputPosMouse;
			case 'MouseOn':
				var id = msg.a;
				return $author$project$InputPosition$InputPosGraph(id);
			default:
				return pos;
		}
	});
var $author$project$InputPosition$update = F2(
	function (pos, msg) {
		var offsetPos = F2(
			function (x, y) {
				var _v1 = $author$project$InputPosition$getKeyboardPos(pos);
				var curx = _v1.a;
				var cury = _v1.b;
				return $author$project$InputPosition$InputPosKeyboard(
					_Utils_Tuple2(x + curx, y + cury));
			});
		_v0$4:
		while (true) {
			if (((msg.$ === 'KeyChanged') && (!msg.a)) && (msg.c.$ === 'Character')) {
				switch (msg.c.a.valueOf()) {
					case 'h':
						return A2(offsetPos, -1, 0);
					case 'j':
						return A2(offsetPos, 0, 1);
					case 'k':
						return A2(offsetPos, 0, -1);
					case 'l':
						return A2(offsetPos, 1, 0);
					default:
						break _v0$4;
				}
			} else {
				break _v0$4;
			}
		}
		return A2($author$project$InputPosition$updateNoKeyboard, pos, msg);
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$Geometry$Epsilon$epsilon = A2($elm$core$Basics$pow, 10, -10);
var $author$project$Geometry$Epsilon$norm0 = function (x) {
	return (_Utils_cmp(
		$elm$core$Basics$abs(x),
		$author$project$Geometry$Epsilon$epsilon) < 1) ? 0 : x;
};
var $author$project$ArrowStyle$toggleDashed = function (s) {
	return _Utils_update(
		s,
		{dashed: !s.dashed});
};
var $author$project$ArrowStyle$toggleDouble = function (s) {
	return _Utils_update(
		s,
		{_double: !s._double});
};
var $author$project$ListExtraExtra$prevInList = F2(
	function (l, a) {
		prevInList:
		while (true) {
			if (!l.b) {
				return a;
			} else {
				if (!l.b.b) {
					var c = l.a;
					return c;
				} else {
					var b = l.a;
					var _v1 = l.b;
					var c = _v1.a;
					var t = _v1.b;
					if (_Utils_eq(a, c)) {
						return b;
					} else {
						var $temp$l = A2($elm$core$List$cons, c, t),
							$temp$a = a;
						l = $temp$l;
						a = $temp$a;
						continue prevInList;
					}
				}
			}
		}
	});
var $author$project$ListExtraExtra$nextInList = F2(
	function (l, a) {
		return A2(
			$author$project$ListExtraExtra$prevInList,
			$elm$core$List$reverse(l),
			a);
	});
var $author$project$ArrowStyle$toggleHead = function (s) {
	return _Utils_update(
		s,
		{
			head: A2(
				$author$project$ListExtraExtra$nextInList,
				_List_fromArray(
					[$author$project$ArrowStyle$DefaultHead, $author$project$ArrowStyle$NoHead, $author$project$ArrowStyle$TwoHeads]),
				s.head)
		});
};
var $author$project$ArrowStyle$toggleHook = function (s) {
	return _Utils_update(
		s,
		{
			tail: A2(
				$author$project$ListExtraExtra$nextInList,
				_List_fromArray(
					[$author$project$ArrowStyle$DefaultTail, $author$project$ArrowStyle$Hook, $author$project$ArrowStyle$HookAlt]),
				s.tail)
		});
};
var $author$project$ArrowStyle$toggleLabelAlignement = function (s) {
	return _Utils_update(
		s,
		{
			labelAlignment: A2(
				$author$project$ListExtraExtra$nextInList,
				_List_fromArray(
					[$author$project$ArrowStyle$Left, $author$project$ArrowStyle$Right]),
				s.labelAlignment)
		});
};
var $author$project$ArrowStyle$keyMaybeUpdateStyle = F2(
	function (k, style) {
		_v0$9:
		while (true) {
			if (k.$ === 'Character') {
				switch (k.a.valueOf()) {
					case '>':
						return $elm$core$Maybe$Just(
							$author$project$ArrowStyle$toggleHead(style));
					case '(':
						return $elm$core$Maybe$Just(
							$author$project$ArrowStyle$toggleHook(style));
					case '=':
						return $elm$core$Maybe$Just(
							$author$project$ArrowStyle$toggleDouble(style));
					case '-':
						return $elm$core$Maybe$Just(
							$author$project$ArrowStyle$toggleDashed(style));
					case 'b':
						return $elm$core$Maybe$Just(
							_Utils_update(
								style,
								{
									bend: $author$project$Geometry$Epsilon$norm0(style.bend + 0.1)
								}));
					case 'B':
						return $elm$core$Maybe$Just(
							_Utils_update(
								style,
								{
									bend: $author$project$Geometry$Epsilon$norm0(style.bend - 0.1)
								}));
					case 'A':
						return $elm$core$Maybe$Just(
							$author$project$ArrowStyle$toggleLabelAlignement(style));
					case ']':
						return $elm$core$Maybe$Just(
							_Utils_update(
								style,
								{
									labelPosition: A2($elm$core$Basics$min, 0.9, style.labelPosition + 0.1)
								}));
					case '[':
						return $elm$core$Maybe$Just(
							_Utils_update(
								style,
								{
									labelPosition: A2($elm$core$Basics$max, 0.1, style.labelPosition - 0.1)
								}));
					default:
						break _v0$9;
				}
			} else {
				break _v0$9;
			}
		}
		return $elm$core$Maybe$Nothing;
	});
var $author$project$Msg$mayUpdateArrowStyle = F2(
	function (m, style) {
		if ((m.$ === 'KeyChanged') && (!m.a)) {
			var k = m.c;
			return A2($author$project$ArrowStyle$keyMaybeUpdateStyle, k, style);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Msg$updateArrowStyle = F2(
	function (m, style) {
		return A2(
			$elm$core$Maybe$withDefault,
			style,
			A2($author$project$Msg$mayUpdateArrowStyle, m, style));
	});
var $author$project$Modes$NewArrow$updateState = F2(
	function (m, state) {
		return _Utils_update(
			m,
			{
				mode: $author$project$Modes$NewArrow(state)
			});
	});
var $author$project$Modes$NewArrow$update = F3(
	function (state, msg, model) {
		var next = function (finish) {
			return A3($author$project$Modes$NewArrow$nextStep, model, finish, state);
		};
		var pullshoutMode = function (k) {
			return $author$project$Model$noCmd(
				_Utils_update(
					model,
					{
						mode: A2(
							$elm$core$Maybe$withDefault,
							$author$project$Modes$NewArrow(state),
							A2(
								$elm$core$Maybe$map,
								$author$project$Modes$PullshoutMode,
								A3($author$project$Modes$Pullshout$initialise, model.graph, state.chosenNode, k)))
					}));
		};
		_v0$7:
		while (true) {
			switch (msg.$) {
				case 'MouseClick':
					return next(false);
				case 'KeyChanged':
					if (!msg.a) {
						if (msg.c.$ === 'Control') {
							switch (msg.c.a) {
								case 'Escape':
									return $author$project$Model$switch_Default(model);
								case 'Enter':
									return next(true);
								case 'Tab':
									return next(false);
								default:
									break _v0$7;
							}
						} else {
							switch (msg.c.a.valueOf()) {
								case 'i':
									return $author$project$Model$noCmd(
										A2(
											$author$project$Modes$NewArrow$updateState,
											model,
											_Utils_update(
												state,
												{inverted: !state.inverted})));
								case 'p':
									return pullshoutMode($author$project$Modes$Pullback);
								case 'P':
									return pullshoutMode($author$project$Modes$Pushout);
								default:
									break _v0$7;
							}
						}
					} else {
						break _v0$7;
					}
				default:
					break _v0$7;
			}
		}
		var st2 = _Utils_update(
			state,
			{
				style: A2($author$project$Msg$updateArrowStyle, msg, state.style)
			});
		var st3 = _Utils_update(
			st2,
			{
				pos: A2($author$project$InputPosition$update, st2.pos, msg)
			});
		return $author$project$Model$noCmd(
			A2($author$project$Modes$NewArrow$updateState, model, st3));
	});
var $author$project$GraphDefs$newPullshout = $author$project$GraphDefs$newGenericLabel($author$project$GraphDefs$PullshoutEdge);
var $author$project$Modes$Pullshout$graph = F2(
	function (m, s) {
		return A4($author$project$Polygraph$newEdge, m.graph, s.chosenEdge, s.currentDest, $author$project$GraphDefs$newPullshout).a;
	});
var $author$project$Modes$Pullshout$nextPullshout = F3(
	function (m, k, st) {
		var recompute = function (_v2) {
			var _v1 = A3($author$project$Modes$Pullshout$possibleDests, m.graph, st.chosenEdge, k);
			if (!_v1.b) {
				return st;
			} else {
				var t = _v1.a;
				var q = _v1.b;
				return _Utils_update(
					st,
					{currentDest: t, kind: k, possibilities: q});
			}
		};
		if (!_Utils_eq(k, st.kind)) {
			return recompute(_Utils_Tuple0);
		} else {
			var _v0 = st.possibilities;
			if (_v0.b) {
				var t = _v0.a;
				var q = _v0.b;
				return _Utils_update(
					st,
					{currentDest: t, possibilities: q});
			} else {
				return recompute(_Utils_Tuple0);
			}
		}
	});
var $author$project$Modes$Pullshout$update = F3(
	function (state, msg, model) {
		var updateState = function (st) {
			return _Utils_update(
				model,
				{
					mode: $author$project$Modes$PullshoutMode(st)
				});
		};
		_v0$4:
		while (true) {
			if ((msg.$ === 'KeyChanged') && (!msg.a)) {
				if (msg.c.$ === 'Character') {
					switch (msg.c.a.valueOf()) {
						case 'p':
							return $author$project$Model$noCmd(
								updateState(
									A3($author$project$Modes$Pullshout$nextPullshout, model, $author$project$Modes$Pullback, state)));
						case 'P':
							return $author$project$Model$noCmd(
								updateState(
									A3($author$project$Modes$Pullshout$nextPullshout, model, $author$project$Modes$Pushout, state)));
						default:
							break _v0$4;
					}
				} else {
					switch (msg.c.a) {
						case 'Escape':
							return $author$project$Model$switch_Default(model);
						case 'Enter':
							return $author$project$Model$switch_Default(
								A2(
									$author$project$Model$setSaveGraph,
									model,
									A2($author$project$Modes$Pullshout$graph, model, state)));
						default:
							break _v0$4;
					}
				}
			} else {
				break _v0$4;
			}
		}
		return $author$project$Model$noCmd(model);
	});
var $author$project$Modes$SplitArrow = function (a) {
	return {$: 'SplitArrow', a: a};
};
var $author$project$GraphDefs$addOrSetSel = F3(
	function (keep, o, gi) {
		var g = keep ? gi : $author$project$GraphDefs$clearSelection(gi);
		var g2 = A4(
			$author$project$Polygraph$update,
			o,
			function (n) {
				return _Utils_update(
					n,
					{selected: true});
			},
			function (n) {
				return _Utils_update(
					n,
					{selected: true});
			},
			g);
		return g2;
	});
var $author$project$Model$addOrSetSel = F3(
	function (keep, o, m) {
		return _Utils_update(
			m,
			{
				graph: A3($author$project$GraphDefs$addOrSetSel, keep, o, m.graph)
			});
	});
var $author$project$GraphDefs$emptyEdge = A2($author$project$GraphDefs$newEdgeLabel, '', $author$project$ArrowStyle$empty);
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$IntDictExtra$getList = F2(
	function (l, d) {
		var d2 = A2($elm_community$intdict$IntDict$map, $elm$core$Tuple$pair, d);
		return A2(
			$elm$core$List$filterMap,
			function (i) {
				return A2($elm_community$intdict$IntDict$get, i, d2);
			},
			l);
	});
var $author$project$Polygraph$getNodes = F2(
	function (l, _v0) {
		var g = _v0.a;
		return A2(
			$elm$core$List$filterMap,
			function (_v1) {
				var id = _v1.a;
				var e = _v1.b;
				return A2(
					$elm$core$Maybe$map,
					$author$project$Polygraph$Node(id),
					$author$project$Polygraph$objNode(e));
			},
			A2($author$project$IntDictExtra$getList, l, g));
	});
var $author$project$Geometry$Point$middle = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return _Utils_Tuple2((x1 + x2) / 2, (y1 + y2) / 2);
	});
var $author$project$Modes$SplitArrow$guessPosition = F2(
	function (m, s) {
		var _v0 = A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.label;
				},
				function ($) {
					return $.pos;
				}),
			A2(
				$author$project$Polygraph$getNodes,
				_List_fromArray(
					[s.source, s.target]),
				m.graph));
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var p1 = _v0.a;
			var _v1 = _v0.b;
			var p2 = _v1.a;
			return A2($author$project$Geometry$Point$middle, p1, p2);
		} else {
			return m.mousePos;
		}
	});
var $author$project$Polygraph$removeList = F2(
	function (l, _v0) {
		var d = _v0.a;
		return $author$project$Polygraph$sanitise(
			$author$project$Polygraph$Graph(
				A2($author$project$IntDictExtra$removeList, l, d)));
	});
var $author$project$Polygraph$remove = function (id) {
	return $author$project$Polygraph$removeList(
		_List_fromArray(
			[id]));
};
var $author$project$Polygraph$removeEdge = $author$project$Polygraph$remove;
var $author$project$Modes$SplitArrow$stateInfo = F2(
	function (m, state) {
		var otherLabel = A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$author$project$GraphDefs$getLabelLabel,
				state.labelOnSource ? state.target : state.source,
				m.graph));
		var _v0 = function () {
			var makeInfo = function (pos) {
				return A3($author$project$Model$mayCreateTargetNodeAt, m, pos, otherLabel);
			};
			if (state.guessPos) {
				return makeInfo(
					A2($author$project$Modes$SplitArrow$guessPosition, m, state));
			} else {
				var _v2 = state.pos;
				if (_v2.$ === 'InputPosGraph') {
					var id = _v2.a;
					return _Utils_Tuple2(
						_Utils_Tuple2(m.graph, id),
						false);
				} else {
					return makeInfo(m.mousePos);
				}
			}
		}();
		var _v1 = _v0.a;
		var g = _v1.a;
		var n = _v1.b;
		var created = _v0.b;
		var _v3 = function () {
			var existingLabels = _Utils_Tuple2(
				A2($author$project$GraphDefs$mapDetails, $author$project$GraphDefs$NormalEdge, state.label),
				state.label.details.label);
			var newLabel = _Utils_Tuple2($author$project$GraphDefs$emptyEdge, otherLabel);
			return state.labelOnSource ? _Utils_Tuple2(existingLabels, newLabel) : _Utils_Tuple2(newLabel, existingLabels);
		}();
		var _v4 = _v3.a;
		var l1 = _v4.a;
		var d1 = _v4.b;
		var _v5 = _v3.b;
		var l2 = _v5.a;
		var d2 = _v5.b;
		var _v6 = A4($author$project$Polygraph$newEdge, g, state.source, n, l1);
		var g1 = _v6.a;
		var ne1 = _v6.b;
		var _v7 = A4($author$project$Polygraph$newEdge, g1, n, state.target, l2);
		var g2 = _v7.a;
		var ne2 = _v7.b;
		return {
			created: created,
			graph: A2($author$project$Polygraph$removeEdge, state.chosenEdge, g2),
			le1: d1,
			le2: d2,
			movedNode: n,
			ne1: ne1,
			ne2: ne2
		};
	});
var $author$project$Modes$SplitArrow$nextStep = F3(
	function (model, finish, state) {
		var info = A2($author$project$Modes$SplitArrow$stateInfo, model, state);
		var m2 = A3(
			$author$project$Model$addOrSetSel,
			false,
			info.movedNode,
			A2($author$project$Model$setSaveGraph, model, info.graph));
		if (finish) {
			return _Utils_Tuple2(
				_Utils_update(
					m2,
					{mode: $author$project$Modes$DefaultMode}),
				$author$project$HtmlDefs$computeLayout(_Utils_Tuple0));
		} else {
			var ne2 = _Utils_Tuple2(info.ne2, info.le2);
			var ne1 = _Utils_Tuple2(info.ne1, info.le1);
			var ids = info.created ? _List_fromArray(
				[
					_Utils_Tuple2(
					info.movedNode,
					A2(
						$elm$core$Maybe$withDefault,
						'',
						A2($author$project$GraphDefs$getLabelLabel, info.movedNode, info.graph))),
					ne1,
					ne2
				]) : _List_fromArray(
				[ne1, ne2]);
			return _Utils_Tuple2(
				A3($author$project$Model$initialise_RenameModeWithDefault, false, ids, m2),
				$author$project$HtmlDefs$computeLayout(_Utils_Tuple0));
		}
	});
var $author$project$Modes$SplitArrow$update = F3(
	function (state, msg, model) {
		var next = function (finish) {
			return A3($author$project$Modes$SplitArrow$nextStep, model, finish, state);
		};
		var updateState = function (st) {
			return _Utils_update(
				model,
				{
					mode: $author$project$Modes$SplitArrow(st)
				});
		};
		var updatePos = function (st) {
			return A2($author$project$InputPosition$updateNoKeyboard, st.pos, msg);
		};
		_v0$5:
		while (true) {
			switch (msg.$) {
				case 'MouseClick':
					return next(false);
				case 'KeyChanged':
					if (!msg.a) {
						if (msg.c.$ === 'Character') {
							if ('/' === msg.c.a.valueOf()) {
								return $author$project$Model$noCmd(
									updateState(
										_Utils_update(
											state,
											{labelOnSource: !state.labelOnSource})));
							} else {
								break _v0$5;
							}
						} else {
							switch (msg.c.a) {
								case 'Escape':
									return $author$project$Model$switch_Default(model);
								case 'Enter':
									return next(true);
								case 'Tab':
									return next(false);
								default:
									break _v0$5;
							}
						}
					} else {
						break _v0$5;
					}
				default:
					break _v0$5;
			}
		}
		var newPos = A2($author$project$InputPosition$updateNoKeyboard, state.pos, msg);
		var guessPos = function () {
			var _v1 = _Utils_Tuple2(msg, newPos);
			if (_v1.a.$ === 'MouseMove') {
				return false;
			} else {
				if (_v1.b.$ === 'InputPosMouse') {
					var _v2 = _v1.b;
					return state.guessPos;
				} else {
					return false;
				}
			}
		}();
		return $author$project$Model$noCmd(
			updateState(
				_Utils_update(
					state,
					{guessPos: guessPos, pos: newPos})));
	});
var $author$project$Modes$SquareMode = function (a) {
	return {$: 'SquareMode', a: a};
};
var $author$project$Model$initialise_RenameMode = F3(
	function (save, l, m) {
		var ls = A2(
			$elm$core$List$filterMap,
			function (id) {
				return A2(
					$elm$core$Maybe$map,
					function (s) {
						return _Utils_Tuple2(id, s);
					},
					A2($author$project$GraphDefs$getLabelLabel, id, m.graph));
			},
			l);
		return A3($author$project$Model$initialise_RenameModeWithDefault, save, ls, m);
	});
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$Modes$Square$chooseAmong = F2(
	function (l, n) {
		if (l.b) {
			var t = l.a;
			var q = l.b;
			return A2(
				$elm$core$List$cons,
				A2($elm$core$Basics$modBy, t, n),
				A2($author$project$Modes$Square$chooseAmong, q, (n / t) | 0));
		} else {
			return _List_Nil;
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? $elm$core$Maybe$Nothing : $elm$core$List$head(
			A2($elm$core$List$drop, idx, xs));
	});
var $author$project$Geometry$Point$diamondPave = F3(
	function (p1, p2, p3) {
		return A2(
			$author$project$Geometry$Point$add,
			p1,
			A2($author$project$Geometry$Point$subtract, p3, p2));
	});
var $author$project$Modes$Square$guessPosition = F2(
	function (m, s) {
		var _v0 = A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.label;
				},
				function ($) {
					return $.pos;
				}),
			A2(
				$author$project$Polygraph$getNodes,
				_List_fromArray(
					[s.n1, s.chosenNode, s.n2]),
				m.graph));
		if (((_v0.b && _v0.b.b) && _v0.b.b.b) && (!_v0.b.b.b.b)) {
			var p1 = _v0.a;
			var _v1 = _v0.b;
			var p2 = _v1.a;
			var _v2 = _v1.b;
			var p3 = _v2.a;
			return A3($author$project$Geometry$Point$diamondPave, p1, p2, p3);
		} else {
			return m.mousePos;
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Modes$Square$makeEdges = F3(
	function (data, ne1, ne2) {
		return {e1: data.e1.id, e2: data.e2.id, ne1: ne1, ne2: ne2};
	});
var $author$project$Modes$Square$nToMoved = F2(
	function (nToChosen, otherNToChosen) {
		return _Utils_eq(nToChosen, otherNToChosen) ? (!nToChosen) : nToChosen;
	});
var $elm$core$List$product = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$mul, 1, numbers);
};
var $elm$core$String$fromList = _String_fromList;
var $elm_community$list_extra$List$Extra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, n, xs),
			A2($elm$core$List$drop, n, xs));
	});
var $author$project$MyDiff$apply = F2(
	function (c, l) {
		var _v0 = A2($elm_community$list_extra$List$Extra$splitAt, c.index, l);
		var l1 = _v0.a;
		var l2 = _v0.b;
		return _Utils_ap(
			l1,
			_Utils_ap(
				$elm$core$List$reverse(c.rep),
				A2($elm$core$List$drop, c.length, l2)));
	});
var $author$project$MyDiff$applyAll = $elm$core$List$foldl($author$project$MyDiff$apply);
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm_community$list_extra$List$Extra$andThen = $elm$core$List$concatMap;
var $author$project$MyDiff$changeIndices = F4(
	function (c1, c2, p, q) {
		return _Utils_Tuple2(
			_Utils_update(
				c2,
				{index: q}),
			_Utils_update(
				c1,
				{index: p}));
	});
var $author$project$MyDiff$offset = function (c) {
	return $elm$core$List$length(c.rep) - c.length;
};
var $author$project$MyDiff$commuteLR = F2(
	function (c1, c2) {
		return _Utils_ap(
			(_Utils_cmp(c1.index + c1.length, c2.index) < 1) ? _List_fromArray(
				[
					A4(
					$author$project$MyDiff$changeIndices,
					c1,
					c2,
					c1.index,
					c2.index + $author$project$MyDiff$offset(c1))
				]) : _List_Nil,
			(_Utils_cmp(c2.index + c2.length, c1.index) < 1) ? _List_fromArray(
				[
					A4(
					$author$project$MyDiff$changeIndices,
					c1,
					c2,
					c1.index + $author$project$MyDiff$offset(c2),
					c2.index)
				]) : _List_Nil);
	});
var $author$project$MyDiff$commuteRL = F2(
	function (c1, c2) {
		return _Utils_ap(
			(_Utils_cmp(
				c1.index + $elm$core$List$length(c1.rep),
				c2.index) < 1) ? _List_fromArray(
				[
					A4(
					$author$project$MyDiff$changeIndices,
					c1,
					c2,
					c1.index,
					c2.index - $author$project$MyDiff$offset(c1))
				]) : _List_Nil,
			(_Utils_cmp(c2.index + c2.length, c1.index) < 1) ? _List_fromArray(
				[
					A4(
					$author$project$MyDiff$changeIndices,
					c1,
					c2,
					c1.index + $author$project$MyDiff$offset(c2),
					c2.index)
				]) : _List_Nil);
	});
var $author$project$MyDiff$commute = function (lr) {
	return lr ? $author$project$MyDiff$commuteLR : $author$project$MyDiff$commuteRL;
};
var $author$project$MyDiff$commuteList = F3(
	function (lr, c, l) {
		if (!l.b) {
			return _List_fromArray(
				[_List_Nil]);
		} else {
			var t = l.a;
			var q = l.b;
			return A2(
				$elm_community$list_extra$List$Extra$andThen,
				function (_v1) {
					var t2 = _v1.a;
					var c2 = _v1.b;
					return A2(
						$elm$core$List$map,
						$elm$core$List$cons(t2),
						A3($author$project$MyDiff$commuteList, lr, c2, q));
				},
				A3($author$project$MyDiff$commute, lr, c, t));
		}
	});
var $author$project$MyDiff$commuteAll = F3(
	function (lr, l, cl) {
		return A3(
			$elm$core$List$foldl,
			A2(
				$elm$core$Basics$composeL,
				$elm_community$list_extra$List$Extra$andThen,
				$author$project$MyDiff$commuteList(lr)),
			_List_fromArray(
				[l]),
			$elm$core$List$reverse(cl));
	});
var $author$project$MyDiff$compile = F2(
	function (i, l) {
		compile:
		while (true) {
			if (!l.b) {
				return _List_Nil;
			} else {
				if (l.a.$ === 'NoChange') {
					var q = l.b;
					var $temp$i = i + 1,
						$temp$l = q;
					i = $temp$i;
					l = $temp$l;
					continue compile;
				} else {
					return A3(
						$author$project$MyDiff$compileChange,
						i,
						l,
						{index: i, length: 0, rep: _List_Nil});
				}
			}
		}
	});
var $author$project$MyDiff$compileChange = F3(
	function (i, l, c) {
		compileChange:
		while (true) {
			if (!l.b) {
				return _List_fromArray(
					[c]);
			} else {
				switch (l.a.$) {
					case 'Added':
						var x = l.a.a;
						var q = l.b;
						var $temp$i = i + 1,
							$temp$l = q,
							$temp$c = _Utils_update(
							c,
							{
								rep: A2($elm$core$List$cons, x, c.rep)
							});
						i = $temp$i;
						l = $temp$l;
						c = $temp$c;
						continue compileChange;
					case 'Removed':
						var q = l.b;
						var $temp$i = i,
							$temp$l = q,
							$temp$c = _Utils_update(
							c,
							{length: c.length + 1});
						i = $temp$i;
						l = $temp$l;
						c = $temp$c;
						continue compileChange;
					default:
						var q = l.b;
						return A2(
							$elm$core$List$cons,
							c,
							A2($author$project$MyDiff$compile, i + 1, q));
				}
			}
		}
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $jinjor$elm_diff$Diff$Added = function (a) {
	return {$: 'Added', a: a};
};
var $jinjor$elm_diff$Diff$CannotGetA = function (a) {
	return {$: 'CannotGetA', a: a};
};
var $jinjor$elm_diff$Diff$CannotGetB = function (a) {
	return {$: 'CannotGetB', a: a};
};
var $jinjor$elm_diff$Diff$NoChange = function (a) {
	return {$: 'NoChange', a: a};
};
var $jinjor$elm_diff$Diff$Removed = function (a) {
	return {$: 'Removed', a: a};
};
var $jinjor$elm_diff$Diff$UnexpectedPath = F2(
	function (a, b) {
		return {$: 'UnexpectedPath', a: a, b: b};
	});
var $jinjor$elm_diff$Diff$makeChangesHelp = F5(
	function (changes, getA, getB, _v0, path) {
		makeChangesHelp:
		while (true) {
			var x = _v0.a;
			var y = _v0.b;
			if (!path.b) {
				return $elm$core$Result$Ok(changes);
			} else {
				var _v2 = path.a;
				var prevX = _v2.a;
				var prevY = _v2.b;
				var tail = path.b;
				var change = function () {
					if (_Utils_eq(x - 1, prevX) && _Utils_eq(y - 1, prevY)) {
						var _v4 = getA(x);
						if (_v4.$ === 'Just') {
							var a = _v4.a;
							return $elm$core$Result$Ok(
								$jinjor$elm_diff$Diff$NoChange(a));
						} else {
							return $elm$core$Result$Err(
								$jinjor$elm_diff$Diff$CannotGetA(x));
						}
					} else {
						if (_Utils_eq(x, prevX)) {
							var _v5 = getB(y);
							if (_v5.$ === 'Just') {
								var b = _v5.a;
								return $elm$core$Result$Ok(
									$jinjor$elm_diff$Diff$Added(b));
							} else {
								return $elm$core$Result$Err(
									$jinjor$elm_diff$Diff$CannotGetB(y));
							}
						} else {
							if (_Utils_eq(y, prevY)) {
								var _v6 = getA(x);
								if (_v6.$ === 'Just') {
									var a = _v6.a;
									return $elm$core$Result$Ok(
										$jinjor$elm_diff$Diff$Removed(a));
								} else {
									return $elm$core$Result$Err(
										$jinjor$elm_diff$Diff$CannotGetA(x));
								}
							} else {
								return $elm$core$Result$Err(
									A2(
										$jinjor$elm_diff$Diff$UnexpectedPath,
										_Utils_Tuple2(x, y),
										path));
							}
						}
					}
				}();
				if (change.$ === 'Ok') {
					var c = change.a;
					var $temp$changes = A2($elm$core$List$cons, c, changes),
						$temp$getA = getA,
						$temp$getB = getB,
						$temp$_v0 = _Utils_Tuple2(prevX, prevY),
						$temp$path = tail;
					changes = $temp$changes;
					getA = $temp$getA;
					getB = $temp$getB;
					_v0 = $temp$_v0;
					path = $temp$path;
					continue makeChangesHelp;
				} else {
					var e = change.a;
					return $elm$core$Result$Err(e);
				}
			}
		}
	});
var $jinjor$elm_diff$Diff$makeChanges = F3(
	function (getA, getB, path) {
		if (!path.b) {
			return $elm$core$Result$Ok(_List_Nil);
		} else {
			var latest = path.a;
			var tail = path.b;
			return A5($jinjor$elm_diff$Diff$makeChangesHelp, _List_Nil, getA, getB, latest, tail);
		}
	});
var $jinjor$elm_diff$Diff$Continue = function (a) {
	return {$: 'Continue', a: a};
};
var $jinjor$elm_diff$Diff$Found = function (a) {
	return {$: 'Found', a: a};
};
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $jinjor$elm_diff$Diff$step = F4(
	function (snake_, offset, k, v) {
		var fromTop = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k + 1) + offset, v));
		var fromLeft = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k - 1) + offset, v));
		var _v0 = function () {
			var _v2 = _Utils_Tuple2(fromLeft, fromTop);
			if (!_v2.a.b) {
				if (!_v2.b.b) {
					return _Utils_Tuple2(
						_List_Nil,
						_Utils_Tuple2(0, 0));
				} else {
					var _v3 = _v2.b;
					var _v4 = _v3.a;
					var topX = _v4.a;
					var topY = _v4.b;
					return _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			} else {
				if (!_v2.b.b) {
					var _v5 = _v2.a;
					var _v6 = _v5.a;
					var leftX = _v6.a;
					var leftY = _v6.b;
					return _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1));
				} else {
					var _v7 = _v2.a;
					var _v8 = _v7.a;
					var leftX = _v8.a;
					var leftY = _v8.b;
					var _v9 = _v2.b;
					var _v10 = _v9.a;
					var topX = _v10.a;
					var topY = _v10.b;
					return (_Utils_cmp(leftY + 1, topY) > -1) ? _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1)) : _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			}
		}();
		var path = _v0.a;
		var _v1 = _v0.b;
		var x = _v1.a;
		var y = _v1.b;
		var _v11 = A3(
			snake_,
			x + 1,
			y + 1,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(x, y),
				path));
		var newPath = _v11.a;
		var goal = _v11.b;
		return goal ? $jinjor$elm_diff$Diff$Found(newPath) : $jinjor$elm_diff$Diff$Continue(
			A3($elm$core$Array$set, k + offset, newPath, v));
	});
var $jinjor$elm_diff$Diff$onpLoopK = F4(
	function (snake_, offset, ks, v) {
		onpLoopK:
		while (true) {
			if (!ks.b) {
				return $jinjor$elm_diff$Diff$Continue(v);
			} else {
				var k = ks.a;
				var ks_ = ks.b;
				var _v1 = A4($jinjor$elm_diff$Diff$step, snake_, offset, k, v);
				if (_v1.$ === 'Found') {
					var path = _v1.a;
					return $jinjor$elm_diff$Diff$Found(path);
				} else {
					var v_ = _v1.a;
					var $temp$snake_ = snake_,
						$temp$offset = offset,
						$temp$ks = ks_,
						$temp$v = v_;
					snake_ = $temp$snake_;
					offset = $temp$offset;
					ks = $temp$ks;
					v = $temp$v;
					continue onpLoopK;
				}
			}
		}
	});
var $jinjor$elm_diff$Diff$onpLoopP = F5(
	function (snake_, delta, offset, p, v) {
		onpLoopP:
		while (true) {
			var ks = (delta > 0) ? _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, delta + p)),
				A2($elm$core$List$range, -p, delta)) : _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, p)),
				A2($elm$core$List$range, (-p) + delta, delta));
			var _v0 = A4($jinjor$elm_diff$Diff$onpLoopK, snake_, offset, ks, v);
			if (_v0.$ === 'Found') {
				var path = _v0.a;
				return path;
			} else {
				var v_ = _v0.a;
				var $temp$snake_ = snake_,
					$temp$delta = delta,
					$temp$offset = offset,
					$temp$p = p + 1,
					$temp$v = v_;
				snake_ = $temp$snake_;
				delta = $temp$delta;
				offset = $temp$offset;
				p = $temp$p;
				v = $temp$v;
				continue onpLoopP;
			}
		}
	});
var $jinjor$elm_diff$Diff$snake = F5(
	function (getA, getB, nextX, nextY, path) {
		snake:
		while (true) {
			var _v0 = _Utils_Tuple2(
				getA(nextX),
				getB(nextY));
			_v0$2:
			while (true) {
				if (_v0.a.$ === 'Just') {
					if (_v0.b.$ === 'Just') {
						var a = _v0.a.a;
						var b = _v0.b.a;
						if (_Utils_eq(a, b)) {
							var $temp$getA = getA,
								$temp$getB = getB,
								$temp$nextX = nextX + 1,
								$temp$nextY = nextY + 1,
								$temp$path = A2(
								$elm$core$List$cons,
								_Utils_Tuple2(nextX, nextY),
								path);
							getA = $temp$getA;
							getB = $temp$getB;
							nextX = $temp$nextX;
							nextY = $temp$nextY;
							path = $temp$path;
							continue snake;
						} else {
							return _Utils_Tuple2(path, false);
						}
					} else {
						break _v0$2;
					}
				} else {
					if (_v0.b.$ === 'Nothing') {
						var _v1 = _v0.a;
						var _v2 = _v0.b;
						return _Utils_Tuple2(path, true);
					} else {
						break _v0$2;
					}
				}
			}
			return _Utils_Tuple2(path, false);
		}
	});
var $jinjor$elm_diff$Diff$onp = F4(
	function (getA, getB, m, n) {
		var v = A2(
			$elm$core$Array$initialize,
			(m + n) + 1,
			$elm$core$Basics$always(_List_Nil));
		var delta = n - m;
		return A5(
			$jinjor$elm_diff$Diff$onpLoopP,
			A2($jinjor$elm_diff$Diff$snake, getA, getB),
			delta,
			m,
			0,
			v);
	});
var $jinjor$elm_diff$Diff$testDiff = F2(
	function (a, b) {
		var arrB = $elm$core$Array$fromList(b);
		var getB = function (y) {
			return A2($elm$core$Array$get, y - 1, arrB);
		};
		var n = $elm$core$Array$length(arrB);
		var arrA = $elm$core$Array$fromList(a);
		var getA = function (x) {
			return A2($elm$core$Array$get, x - 1, arrA);
		};
		var m = $elm$core$Array$length(arrA);
		var path = A4($jinjor$elm_diff$Diff$onp, getA, getB, m, n);
		return A3($jinjor$elm_diff$Diff$makeChanges, getA, getB, path);
	});
var $jinjor$elm_diff$Diff$diff = F2(
	function (a, b) {
		var _v0 = A2($jinjor$elm_diff$Diff$testDiff, a, b);
		if (_v0.$ === 'Ok') {
			var changes = _v0.a;
			return changes;
		} else {
			return _List_Nil;
		}
	});
var $author$project$MyDiff$swapDiff = F4(
	function (lr, l1, l2, l3) {
		var d2 = A2($jinjor$elm_diff$Diff$diff, l2, l3);
		var d1 = lr ? A2($jinjor$elm_diff$Diff$diff, l2, l1) : A2($jinjor$elm_diff$Diff$diff, l1, l2);
		var cl2 = A2($author$project$MyDiff$compile, 0, d2);
		var cl1 = A2($author$project$MyDiff$compile, 0, d1);
		return A2(
			$elm$core$List$map,
			$author$project$MyDiff$applyAll(l1),
			A3($author$project$MyDiff$commuteAll, lr, cl2, cl1));
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$MyDiff$swapDiffStr = F4(
	function (lr, s1, s2, s3) {
		return A2(
			$elm$core$List$map,
			$elm$core$String$fromList,
			A4(
				$author$project$MyDiff$swapDiff,
				lr,
				$elm$core$String$toList(s1),
				$elm$core$String$toList(s2),
				$elm$core$String$toList(s3)));
	});
var $author$project$Modes$Square$moveNodeViewInfo = F2(
	function (m, data) {
		var atLeast1 = function (l) {
			return $elm$core$List$isEmpty(l) ? _List_fromArray(
				['']) : l;
		};
		var commute = F2(
			function (str1, str2) {
				return ((str1 === '') || (str2 === '')) ? _List_fromArray(
					['']) : atLeast1(
					A4(
						$author$project$MyDiff$swapDiffStr,
						_Utils_eq(data.n1ToChosen, data.n2ToChosen),
						str1,
						data.chosenLabel,
						str2));
			});
		var labelsNode = A2(commute, data.n1Label, data.n2Label);
		var labelsEdge2 = A2(commute, data.e1.label.details.label, data.n2Label);
		var labelsEdge1 = A2(commute, data.n1Label, data.e2.label.details.label);
		var possibleLabels = _List_fromArray(
			[labelsNode, labelsEdge1, labelsEdge2]);
		var lens = A2($elm$core$List$map, $elm$core$List$length, possibleLabels);
		var labels = function () {
			if (A2(
				$elm$core$Basics$modBy,
				$elm$core$List$product(lens) + 1,
				data.labelConfiguration) === 1) {
				return _List_fromArray(
					['', '', '']);
			} else {
				var lconf = (!data.labelConfiguration) ? 0 : (data.labelConfiguration - 1);
				var ids = A2($author$project$Modes$Square$chooseAmong, lens, lconf);
				return A2(
					$elm$core$List$map,
					$elm$core$Maybe$withDefault('!!'),
					A3($elm$core$List$map2, $elm_community$list_extra$List$Extra$getAt, ids, possibleLabels));
			}
		}();
		var _v0 = function () {
			if (((labels.b && labels.b.b) && labels.b.b.b) && (!labels.b.b.b.b)) {
				var a = labels.a;
				var _v2 = labels.b;
				var b = _v2.a;
				var _v3 = _v2.b;
				var c = _v3.a;
				return _Utils_Tuple3(a, b, c);
			} else {
				return _Utils_Tuple3('!', '!', '!');
			}
		}();
		var labelNode = _v0.a;
		var labelEdge1 = _v0.b;
		var labelEdge2 = _v0.c;
		var newPos = data.guessPos ? A2($author$project$Modes$Square$guessPosition, m, data) : m.mousePos;
		var _v4 = A3($author$project$Model$mayCreateTargetNodeAt, m, newPos, labelNode);
		var _v5 = _v4.a;
		var g = _v5.a;
		var n = _v5.b;
		var created = _v4.b;
		var make_EdgeId = F3(
			function (n1, n2, isTo) {
				return isTo ? _Utils_Tuple2(n1, n2) : _Utils_Tuple2(n2, n1);
			});
		var _v6 = A3(
			make_EdgeId,
			data.n1,
			n,
			A2($author$project$Modes$Square$nToMoved, data.n1ToChosen, data.n2ToChosen));
		var e1n1 = _v6.a;
		var e1n2 = _v6.b;
		var _v7 = A3(
			make_EdgeId,
			data.n2,
			n,
			A2($author$project$Modes$Square$nToMoved, data.n2ToChosen, data.n1ToChosen));
		var e2n1 = _v7.a;
		var e2n2 = _v7.b;
		var _v8 = A4(
			$author$project$Polygraph$newEdge,
			g,
			e1n1,
			e1n2,
			A2($author$project$GraphDefs$newEdgeLabel, labelEdge1, $author$project$ArrowStyle$empty));
		var g1 = _v8.a;
		var ne1 = _v8.b;
		var _v9 = A4(
			$author$project$Polygraph$newEdge,
			g1,
			e2n1,
			e2n2,
			A2($author$project$GraphDefs$newEdgeLabel, labelEdge2, $author$project$ArrowStyle$empty));
		var g2 = _v9.a;
		var ne2 = _v9.b;
		var edges = A3($author$project$Modes$Square$makeEdges, data, ne1, ne2);
		return _Utils_Tuple3(
			{edges: edges, graph: g2},
			n,
			created);
	});
var $author$project$Modes$Square$nextStep = F3(
	function (model, finish, state) {
		var _v0 = A2($author$project$Modes$Square$moveNodeViewInfo, model, state);
		var info = _v0.a;
		var movedNode = _v0.b;
		var created = _v0.c;
		var m2 = A3(
			$author$project$Model$addOrSetSel,
			false,
			movedNode,
			A2($author$project$Model$setSaveGraph, model, info.graph));
		if (finish) {
			return _Utils_Tuple2(
				_Utils_update(
					m2,
					{mode: $author$project$Modes$DefaultMode}),
				$author$project$HtmlDefs$computeLayout(_Utils_Tuple0));
		} else {
			var ids = created ? _List_fromArray(
				[movedNode, info.edges.ne1, info.edges.ne2]) : _List_fromArray(
				[info.edges.ne1, info.edges.ne2]);
			return _Utils_Tuple2(
				A3($author$project$Model$initialise_RenameMode, false, ids, m2),
				$author$project$HtmlDefs$computeLayout(_Utils_Tuple0));
		}
	});
var $author$project$GraphDefs$filterEdgeNormal = function (e) {
	return A2(
		$elm$core$Maybe$map,
		function (l) {
			return A2(
				$author$project$Polygraph$edgeMap,
				$elm$core$Basics$always(l),
				e);
		},
		$author$project$GraphDefs$filterLabelNormal(e.label));
};
var $elm_community$list_extra$List$Extra$uniquePairs = function (xs) {
	if (!xs.b) {
		return _List_Nil;
	} else {
		var x = xs.a;
		var xs_ = xs.b;
		return _Utils_ap(
			A2(
				$elm$core$List$map,
				function (y) {
					return _Utils_Tuple2(x, y);
				},
				xs_),
			$elm_community$list_extra$List$Extra$uniquePairs(xs_));
	}
};
var $author$project$Modes$Square$possibleSquareStates = F2(
	function (g, id) {
		var _v0 = A2($author$project$GraphDefs$getLabelLabel, id, g);
		if (_v0.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var chosenLabel = _v0.a;
			var outs = A2(
				$elm$core$List$filterMap,
				function (x) {
					return A2(
						$elm$core$Maybe$map,
						function (labelNode) {
							return _Utils_Tuple3(
								x,
								_Utils_Tuple2(labelNode, x.to),
								false);
						},
						A2($author$project$GraphDefs$getLabelLabel, x.to, g));
				},
				A2(
					$elm$core$List$filterMap,
					$author$project$GraphDefs$filterEdgeNormal,
					A2($author$project$Polygraph$outgoings, id, g)));
			var ins = A2(
				$elm$core$List$filterMap,
				function (x) {
					return A2(
						$elm$core$Maybe$map,
						function (labelNode) {
							return _Utils_Tuple3(
								x,
								_Utils_Tuple2(labelNode, x.from),
								true);
						},
						A2($author$project$GraphDefs$getLabelLabel, x.from, g));
				},
				A2(
					$elm$core$List$filterMap,
					$author$project$GraphDefs$filterEdgeNormal,
					A2($author$project$Polygraph$incomings, id, g)));
			return A2(
				$elm$core$List$map,
				function (_v1) {
					var _v2 = _v1.a;
					var e1 = _v2.a;
					var _v3 = _v2.b;
					var l1 = _v3.a;
					var n1 = _v3.b;
					var i1 = _v2.c;
					var _v4 = _v1.b;
					var e2 = _v4.a;
					var _v5 = _v4.b;
					var l2 = _v5.a;
					var n2 = _v5.b;
					var i2 = _v4.c;
					return {chosenLabel: chosenLabel, chosenNode: id, configuration: 0, e1: e1, e2: e2, guessPos: true, labelConfiguration: 0, n1: n1, n1Label: l1, n1ToChosen: i1, n2: n2, n2Label: l2, n2ToChosen: i2};
				},
				$elm_community$list_extra$List$Extra$uniquePairs(
					_Utils_ap(ins, outs)));
		}
	});
var $author$project$Modes$Square$square_setPossibility = F3(
	function (idx, g, chosenNode) {
		var possibilities = A2($author$project$Modes$Square$possibleSquareStates, g, chosenNode);
		return A2(
			$elm$core$Maybe$map,
			function (s) {
				return _Utils_update(
					s,
					{
						configuration: A2(
							$elm$core$Basics$modBy,
							$elm$core$List$length(possibilities),
							idx + 1)
					});
			},
			A2($elm_community$list_extra$List$Extra$getAt, idx, possibilities));
	});
var $author$project$Modes$Square$square_updatePossibility = F3(
	function (m, idx, node) {
		return $author$project$Model$noCmd(
			A2(
				$elm$core$Maybe$withDefault,
				m,
				A2(
					$elm$core$Maybe$map,
					function (state) {
						return _Utils_update(
							m,
							{
								mode: $author$project$Modes$SquareMode(state)
							});
					},
					A3($author$project$Modes$Square$square_setPossibility, idx, m.graph, node))));
	});
var $author$project$Modes$Square$update = F3(
	function (state, msg, model) {
		var next = function (finish) {
			return A3($author$project$Modes$Square$nextStep, model, finish, state);
		};
		_v0$7:
		while (true) {
			switch (msg.$) {
				case 'MouseClick':
					return next(false);
				case 'MouseMove':
					return $author$project$Model$noCmd(
						_Utils_update(
							model,
							{
								mode: $author$project$Modes$SquareMode(
									_Utils_update(
										state,
										{guessPos: false}))
							}));
				case 'KeyChanged':
					if (!msg.a) {
						if (msg.c.$ === 'Character') {
							switch (msg.c.a.valueOf()) {
								case 's':
									return A3($author$project$Modes$Square$square_updatePossibility, model, state.configuration, state.chosenNode);
								case 'a':
									return $author$project$Model$noCmd(
										_Utils_update(
											model,
											{
												mode: $author$project$Modes$SquareMode(
													_Utils_update(
														state,
														{labelConfiguration: state.labelConfiguration + 1}))
											}));
								default:
									break _v0$7;
							}
						} else {
							switch (msg.c.a) {
								case 'Escape':
									return $author$project$Model$switch_Default(model);
								case 'Enter':
									return next(true);
								case 'Tab':
									return next(false);
								default:
									break _v0$7;
							}
						}
					} else {
						break _v0$7;
					}
				default:
					break _v0$7;
			}
		}
		return $author$project$Model$noCmd(model);
	});
var $author$project$Polygraph$updateNode = F3(
	function (i, fn, g) {
		return A4($author$project$Polygraph$update, i, fn, $elm$core$Basics$identity, g);
	});
var $author$project$Polygraph$updateEdge = F3(
	function (i, fe, g) {
		return A4($author$project$Polygraph$update, i, $elm$core$Basics$identity, fe, g);
	});
var $author$project$GraphDefs$updateNormalEdge = F2(
	function (id, f) {
		return A2(
			$author$project$Polygraph$updateEdge,
			id,
			$author$project$GraphDefs$mapNormalEdge(f));
	});
var $author$project$Model$updateWithGraphInfo = F2(
	function (m, _v0) {
		var graph = _v0.graph;
		var sizeGrid = _v0.sizeGrid;
		var latexPreamble = _v0.latexPreamble;
		return _Utils_update(
			m,
			{graph: graph, latexPreamble: latexPreamble, sizeGrid: sizeGrid});
	});
var $author$project$Geometry$centerRect = function (_v0) {
	var bottomRight = _v0.bottomRight;
	var topLeft = _v0.topLeft;
	return A2($author$project$Geometry$Point$middle, bottomRight, topLeft);
};
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $author$project$Geometry$rectEnveloppe = function (l) {
	var _v0 = $elm$core$List$unzip(l);
	var xs = _v0.a;
	var ys = _v0.b;
	var lmin = A2(
		$elm$core$Basics$composeR,
		$elm$core$List$minimum,
		$elm$core$Maybe$withDefault(0));
	var lmax = A2(
		$elm$core$Basics$composeR,
		$elm$core$List$maximum,
		$elm$core$Maybe$withDefault(0));
	return {
		bottomRight: _Utils_Tuple2(
			lmax(xs),
			lmax(ys)),
		topLeft: _Utils_Tuple2(
			lmin(xs),
			lmin(ys))
	};
};
var $author$project$GraphDefs$centerOfNodes = function (nodes) {
	return $author$project$Geometry$centerRect(
		$author$project$Geometry$rectEnveloppe(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeL,
					function ($) {
						return $.pos;
					},
					function ($) {
						return $.label;
					}),
				nodes)));
};
var $author$project$Polygraph$addId = F2(
	function (n, g) {
		return $elm_community$intdict$IntDict$fromList(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var id = _v0.a;
					var o = _v0.b;
					return _Utils_Tuple2(
						id + n,
						function () {
							if (o.$ === 'NodeObj') {
								return o;
							} else {
								var i1 = o.a;
								var i2 = o.b;
								var e = o.c;
								return A3($author$project$Polygraph$EdgeObj, i1 + n, i2 + n, e);
							}
						}());
				},
				$elm_community$intdict$IntDict$toList(g)));
	});
var $author$project$Polygraph$union = F2(
	function (_v0, _v1) {
		var base = _v0.a;
		var ext = _v1.a;
		var baseId = $author$project$Polygraph$supId(base);
		var extUp = A2($author$project$Polygraph$addId, baseId, ext);
		return $author$project$Polygraph$Graph(
			A2($elm_community$intdict$IntDict$union, base, extUp));
	});
var $author$project$GraphDefs$cloneSelected = F2(
	function (g, offset) {
		var g2 = A3(
			$author$project$Polygraph$map,
			F2(
				function (_v0, n) {
					return _Utils_update(
						n,
						{
							pos: A2($author$project$Geometry$Point$add, n.pos, offset),
							selected: true
						});
				}),
			F2(
				function (_v1, e) {
					return _Utils_update(
						e,
						{selected: true});
				}),
			$author$project$GraphDefs$selectedGraph(g));
		var gclearSel = $author$project$GraphDefs$clearSelection(g);
		return A2($author$project$Polygraph$union, gclearSel, g2);
	});
var $author$project$Main$graphClone = function (m) {
	var nodes = $author$project$Polygraph$nodes(
		$author$project$GraphDefs$selectedGraph(m.graph));
	var mouseDelta = A2(
		$author$project$Geometry$Point$subtract,
		m.mousePos,
		$author$project$GraphDefs$centerOfNodes(nodes));
	return A2($author$project$GraphDefs$cloneSelected, m.graph, mouseDelta);
};
var $author$project$Main$update_Clone = F2(
	function (msg, m) {
		var finalise = function (_v1) {
			var m2 = $author$project$Model$pushHistory(m);
			return _Utils_Tuple2(
				_Utils_update(
					m2,
					{
						graph: $author$project$Main$graphClone(m),
						mode: $author$project$Modes$DefaultMode
					}),
				$elm$core$Platform$Cmd$none);
		};
		_v0$3:
		while (true) {
			switch (msg.$) {
				case 'KeyChanged':
					if ((!msg.a) && (msg.c.$ === 'Control')) {
						switch (msg.c.a) {
							case 'Escape':
								return _Utils_Tuple2(
									_Utils_update(
										m,
										{mode: $author$project$Modes$DefaultMode}),
									$elm$core$Platform$Cmd$none);
							case 'Enter':
								return finalise(_Utils_Tuple0);
							default:
								break _v0$3;
						}
					} else {
						break _v0$3;
					}
				case 'MouseClick':
					return finalise(_Utils_Tuple0);
				default:
					break _v0$3;
			}
		}
		return $author$project$Model$noCmd(m);
	});
var $author$project$Modes$CutHead = function (a) {
	return {$: 'CutHead', a: a};
};
var $elm_community$list_extra$List$Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, pred),
			list);
	});
var $author$project$Polygraph$merge = F3(
	function (i1, i2, _v0) {
		var g = _v0.a;
		return $author$project$Polygraph$sanitise(
			$author$project$Polygraph$Graph(
				A2(
					$elm_community$intdict$IntDict$remove,
					i2,
					A2(
						$elm_community$intdict$IntDict$map,
						F2(
							function (_v1, o) {
								if (o.$ === 'EdgeObj') {
									var j1 = o.a;
									var j2 = o.b;
									var e = o.c;
									var repl = function (k) {
										return _Utils_eq(k, i2) ? i1 : k;
									};
									return A3(
										$author$project$Polygraph$EdgeObj,
										repl(j1),
										repl(j2),
										e);
								} else {
									return o;
								}
							}),
						g))));
	});
var $author$project$Polygraph$removeLoops = A2(
	$elm$core$Basics$composeR,
	$author$project$Polygraph$sanitise,
	$author$project$Polygraph$mapRep(
		A2(
			$author$project$Polygraph$rawFilterIds,
			$elm$core$Basics$always(true),
			F3(
				function (id1, id2, _v0) {
					return !_Utils_eq(id1, id2);
				}))));
var $author$project$GraphDefs$mergeWithSameLoc = F2(
	function (n, g) {
		var _v0 = A2(
			$elm_community$list_extra$List$Extra$filterNot,
			$elm$core$Basics$eq(n.id),
			A2($author$project$GraphDefs$getNodesAt, g, n.label.pos));
		if (_v0.b && (!_v0.b.b)) {
			var i = _v0.a;
			return _Utils_Tuple2(
				$author$project$Polygraph$removeLoops(
					A3($author$project$Polygraph$merge, i, n.id, g)),
				true);
		} else {
			return _Utils_Tuple2(g, false);
		}
	});
var $author$project$GraphDefs$unselect = function (id) {
	return A3(
		$author$project$Polygraph$update,
		id,
		function (n) {
			return _Utils_update(
				n,
				{selected: false});
		},
		function (e) {
			return _Utils_update(
				e,
				{selected: false});
		});
};
var $author$project$Main$graphCutHead = F2(
	function (_v0, m) {
		var id = _v0.id;
		var head = _v0.head;
		var duplicate = _v0.duplicate;
		var pos = m.mousePos;
		return A2(
			$elm$core$Maybe$withDefault,
			m.graph,
			A2(
				$elm$core$Maybe$andThen,
				function (e) {
					return A2(
						$elm$core$Maybe$map,
						function (nto) {
							var g1 = duplicate ? A2($author$project$GraphDefs$unselect, id, m.graph) : A2($author$project$Polygraph$removeEdge, id, m.graph);
							var label = _Utils_update(
								nto,
								{pos: pos});
							var _v1 = A2($author$project$Polygraph$newNode, g1, label);
							var g2 = _v1.a;
							var newId = _v1.b;
							var _v2 = head ? _Utils_Tuple2(e.from, newId) : _Utils_Tuple2(newId, e.to);
							var n1 = _v2.a;
							var n2 = _v2.b;
							var _v3 = A4($author$project$Polygraph$newEdge, g2, n1, n2, e.label);
							var g3 = _v3.a;
							var g4 = m.specialKeys.ctrl ? A2(
								$author$project$GraphDefs$mergeWithSameLoc,
								{id: newId, label: label},
								g3).a : g3;
							return g4;
						},
						A2(
							$author$project$Polygraph$getNode,
							head ? e.to : e.from,
							m.graph));
				},
				A2($author$project$Polygraph$getEdge, id, m.graph)));
	});
var $author$project$Main$update_CutHead = F3(
	function (state, msg, m) {
		var finalise = function (_v1) {
			return _Utils_Tuple2(
				_Utils_update(
					m,
					{
						graph: A2($author$project$Main$graphCutHead, state, m),
						mode: $author$project$Modes$DefaultMode
					}),
				$elm$core$Platform$Cmd$none);
		};
		var changeState = function (s) {
			return _Utils_update(
				m,
				{
					mode: $author$project$Modes$CutHead(s)
				});
		};
		_v0$5:
		while (true) {
			switch (msg.$) {
				case 'MouseClick':
					return finalise(_Utils_Tuple0);
				case 'KeyChanged':
					if (!msg.a) {
						if (msg.c.$ === 'Control') {
							switch (msg.c.a) {
								case 'Escape':
									return _Utils_Tuple2(
										_Utils_update(
											m,
											{mode: $author$project$Modes$DefaultMode}),
										$elm$core$Platform$Cmd$none);
								case 'Enter':
									return finalise(_Utils_Tuple0);
								default:
									break _v0$5;
							}
						} else {
							switch (msg.c.a.valueOf()) {
								case 'c':
									return _Utils_Tuple2(
										changeState(
											_Utils_update(
												state,
												{head: !state.head})),
										$elm$core$Platform$Cmd$none);
								case 'd':
									return _Utils_Tuple2(
										changeState(
											_Utils_update(
												state,
												{duplicate: !state.duplicate})),
										$elm$core$Platform$Cmd$none);
								default:
									break _v0$5;
							}
						}
					} else {
						break _v0$5;
					}
				default:
					break _v0$5;
			}
		}
		return $author$project$Model$noCmd(m);
	});
var $author$project$Main$update_DebugMode = F2(
	function (msg, model) {
		if ((((msg.$ === 'KeyChanged') && (!msg.a)) && (msg.c.$ === 'Control')) && (msg.c.a === 'Escape')) {
			return $author$project$Model$switch_Default(model);
		} else {
			return $author$project$Model$noCmd(model);
		}
	});
var $author$project$Modes$CloneMode = {$: 'CloneMode'};
var $author$project$Modes$DebugMode = {$: 'DebugMode'};
var $author$project$Modes$RectSelect = function (a) {
	return {$: 'RectSelect', a: a};
};
var $author$project$Main$alert = _Platform_outgoingPort('alert', $elm$json$Json$Encode$string);
var $elm$core$Basics$round = _Basics_round;
var $author$project$Geometry$Point$closeRemainder = F2(
	function (q, a) {
		return a - ($elm$core$Basics$round(a / q) * q);
	});
var $elm$core$Basics$pi = _Basics_pi;
var $author$project$Geometry$Point$normaliseAngle = function (alpha) {
	return A2($author$project$Geometry$Point$closeRemainder, 2 * $elm$core$Basics$pi, alpha);
};
var $author$project$Geometry$Point$distanceAngleSigned = F2(
	function (alpha, beta) {
		return $author$project$Geometry$Point$normaliseAngle(beta - alpha);
	});
var $author$project$Geometry$Point$distanceAngle = F2(
	function (alpha, beta) {
		return $elm$core$Basics$abs(
			A2($author$project$Geometry$Point$distanceAngleSigned, alpha, beta));
	});
var $author$project$Geometry$Point$angleWithInRange = F3(
	function (delta, alpha, beta) {
		return _Utils_cmp(
			A2($author$project$Geometry$Point$distanceAngle, alpha, beta),
			$elm$core$Basics$abs(delta)) < 1;
	});
var $author$project$HtmlDefs$bottomTextId = 'bottom-text';
var $author$project$Main$clipboardWriteGraph = _Platform_outgoingPort(
	'clipboardWriteGraph',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'graph',
					function ($) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'edges',
									$elm$json$Json$Encode$list(
										function ($) {
											return $elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'from',
														$elm$json$Json$Encode$int($.from)),
														_Utils_Tuple2(
														'id',
														$elm$json$Json$Encode$int($.id)),
														_Utils_Tuple2(
														'label',
														function ($) {
															return $elm$json$Json$Encode$object(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'isPullshout',
																		$elm$json$Json$Encode$bool($.isPullshout)),
																		_Utils_Tuple2(
																		'label',
																		$elm$json$Json$Encode$string($.label)),
																		_Utils_Tuple2(
																		'style',
																		function ($) {
																			return $elm$json$Json$Encode$object(
																				_List_fromArray(
																					[
																						_Utils_Tuple2(
																						'alignment',
																						$elm$json$Json$Encode$string($.alignment)),
																						_Utils_Tuple2(
																						'bend',
																						$elm$json$Json$Encode$float($.bend)),
																						_Utils_Tuple2(
																						'dashed',
																						$elm$json$Json$Encode$bool($.dashed)),
																						_Utils_Tuple2(
																						'double',
																						$elm$json$Json$Encode$bool($._double)),
																						_Utils_Tuple2(
																						'head',
																						$elm$json$Json$Encode$string($.head)),
																						_Utils_Tuple2(
																						'position',
																						$elm$json$Json$Encode$float($.position)),
																						_Utils_Tuple2(
																						'tail',
																						$elm$json$Json$Encode$string($.tail))
																					]));
																		}($.style)),
																		_Utils_Tuple2(
																		'zindex',
																		$elm$json$Json$Encode$int($.zindex))
																	]));
														}($.label)),
														_Utils_Tuple2(
														'to',
														$elm$json$Json$Encode$int($.to))
													]));
										})($.edges)),
									_Utils_Tuple2(
									'latexPreamble',
									$elm$json$Json$Encode$string($.latexPreamble)),
									_Utils_Tuple2(
									'nodes',
									$elm$json$Json$Encode$list(
										function ($) {
											return $elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'id',
														$elm$json$Json$Encode$int($.id)),
														_Utils_Tuple2(
														'label',
														function ($) {
															return $elm$json$Json$Encode$object(
																_List_fromArray(
																	[
																		_Utils_Tuple2(
																		'isMath',
																		$elm$json$Json$Encode$bool($.isMath)),
																		_Utils_Tuple2(
																		'label',
																		$elm$json$Json$Encode$string($.label)),
																		_Utils_Tuple2(
																		'pos',
																		function ($) {
																			var a = $.a;
																			var b = $.b;
																			return A2(
																				$elm$json$Json$Encode$list,
																				$elm$core$Basics$identity,
																				_List_fromArray(
																					[
																						$elm$json$Json$Encode$float(a),
																						$elm$json$Json$Encode$float(b)
																					]));
																		}($.pos))
																	]));
														}($.label))
													]));
										})($.nodes)),
									_Utils_Tuple2(
									'sizeGrid',
									$elm$json$Json$Encode$int($.sizeGrid))
								]));
					}($.graph)),
					_Utils_Tuple2(
					'version',
					$elm$json$Json$Encode$int($.version))
				]));
	});
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$Geometry$Point$radius = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$Basics$sqrt((x * x) + (y * y));
};
var $author$project$Geometry$Point$distance = F2(
	function (x, y) {
		return $author$project$Geometry$Point$radius(
			A2($author$project$Geometry$Point$subtract, y, x));
	});
var $author$project$Geometry$distanceToRect = F2(
	function (_v0, r) {
		var px = _v0.a;
		var py = _v0.b;
		var _v1 = r.topLeft;
		var minx = _v1.a;
		var miny = _v1.b;
		var _v2 = r.bottomRight;
		var maxx = _v2.a;
		var maxy = _v2.b;
		var dy = A2(
			$elm$core$Basics$max,
			miny - py,
			A2($elm$core$Basics$max, 0, py - maxy));
		var dx = A2(
			$elm$core$Basics$max,
			minx - px,
			A2($elm$core$Basics$max, 0, px - maxx));
		return $elm$core$Basics$sqrt((dx * dx) + (dy * dy));
	});
var $author$project$GraphDefs$distanceToNode = F2(
	function (p, n) {
		var posDims = {
			dims: $author$project$GraphDefs$getNodeDims(n),
			pos: n.pos
		};
		var rect = $author$project$Geometry$rectFromPosDims(posDims);
		return A2($author$project$Geometry$distanceToRect, p, rect);
	});
var $elm_community$list_extra$List$Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _v1) {
				var y = _v1.a;
				var fy = _v1.b;
				var fx = f(x);
				return (_Utils_cmp(fx, fy) < 0) ? _Utils_Tuple2(x, fx) : _Utils_Tuple2(y, fy);
			});
		if (ls.b) {
			if (!ls.b.b) {
				var l_ = ls.a;
				return $elm$core$Maybe$Just(l_);
			} else {
				var l_ = ls.a;
				var ls_ = ls.b;
				return $elm$core$Maybe$Just(
					A3(
						$elm$core$List$foldl,
						minBy,
						_Utils_Tuple2(
							l_,
							f(l_)),
						ls_).a);
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$GraphDefs$closest = F2(
	function (pos, ug) {
		var _v0 = A2($author$project$GraphDefs$getNodesAt, ug, pos);
		if (_v0.b) {
			var t = _v0.a;
			return t;
		} else {
			var ug2 = A5(
				$author$project$Polygraph$mapRecAll,
				function ($) {
					return $.pos;
				},
				function ($) {
					return $.pos;
				},
				F2(
					function (_v1, n) {
						return {
							distance: A2($author$project$GraphDefs$distanceToNode, pos, n),
							pos: n.pos
						};
					}),
				F4(
					function (_v2, p1, p2, e) {
						var epos = A2($author$project$Geometry$Point$middle, p1, p2);
						return {
							distance: A2(
								$author$project$Geometry$Point$distance,
								pos,
								A2($author$project$Geometry$Point$middle, p1, p2)),
							pos: epos
						};
					}),
				ug);
			var getEmptysDistance = function (l) {
				return A2(
					$elm$core$List$map,
					function (o) {
						return {distance: o.label.distance, id: o.id};
					},
					l);
			};
			var unnamedEdges = getEmptysDistance(
				$author$project$Polygraph$edges(ug2));
			var unnamedNodes = getEmptysDistance(
				$author$project$Polygraph$nodes(ug2));
			var unnamedAll = A2(
				$elm$core$Maybe$withDefault,
				0,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.id;
					},
					A2(
						$elm_community$list_extra$List$Extra$minimumBy,
						function ($) {
							return $.distance;
						},
						_Utils_ap(unnamedEdges, unnamedNodes))));
			return unnamedAll;
		}
	});
var $author$project$Polygraph$incidence = function (_v0) {
	var g = _v0.a;
	var es = $author$project$Polygraph$edges(
		$author$project$Polygraph$Graph(g));
	var emptyInfo = {incomings: _List_Nil, outgoings: _List_Nil};
	var insertIn = F2(
		function (e, i) {
			return _Utils_update(
				i,
				{
					incomings: A2($elm$core$List$cons, e, i.incomings)
				});
		});
	var insertOut = F2(
		function (e, i) {
			return _Utils_update(
				i,
				{
					outgoings: A2($elm$core$List$cons, e, i.outgoings)
				});
		});
	var aux = F2(
		function (l, d) {
			if (!l.b) {
				return d;
			} else {
				var e = l.a;
				var q = l.b;
				return A2(
					aux,
					q,
					A3(
						$elm_community$intdict$IntDict$update,
						e.from,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Maybe$withDefault(emptyInfo),
							A2(
								$elm$core$Basics$composeR,
								insertOut(e),
								$elm$core$Maybe$Just)),
						A3(
							$elm_community$intdict$IntDict$update,
							e.to,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Maybe$withDefault(emptyInfo),
								A2(
									$elm$core$Basics$composeR,
									insertIn(e),
									$elm$core$Maybe$Just)),
							d)));
			}
		});
	var di = A2(
		$elm_community$intdict$IntDict$map,
		F2(
			function (_v2, _v3) {
				return {incomings: _List_Nil, outgoings: _List_Nil};
			}),
		g);
	return A2(aux, es, di);
};
var $author$project$Polygraph$updateList = F4(
	function (l, fn, fe, g) {
		return A3(
			$elm$core$List$foldl,
			function (i) {
				return A3($author$project$Polygraph$update, i, fn, fe);
			},
			g,
			l);
	});
var $author$project$Polygraph$connectedClosure = F3(
	function (fn, fe, _v0) {
		var g = _v0.a;
		var li = $elm_community$intdict$IntDict$keys(
			A3($author$project$Polygraph$rawFilter, fn, fe, g));
		var inc = $author$project$Polygraph$incidence(
			$author$project$Polygraph$Graph(g));
		var aux = F2(
			function (d, l) {
				aux:
				while (true) {
					if (!l.b) {
						return d;
					} else {
						var t = l.a;
						var q = l.b;
						var _v2 = A2($elm_community$intdict$IntDict$get, t, d);
						if (_v2.$ === 'Nothing') {
							var $temp$d = d,
								$temp$l = q;
							d = $temp$d;
							l = $temp$l;
							continue aux;
						} else {
							var i = _v2.a;
							var lsuite = _Utils_ap(
								function () {
									var _v3 = A2(
										$author$project$Polygraph$getEdge,
										t,
										$author$project$Polygraph$Graph(g));
									if (_v3.$ === 'Nothing') {
										return _List_Nil;
									} else {
										var from = _v3.a.from;
										var to = _v3.a.to;
										return _List_fromArray(
											[from, to]);
									}
								}(),
								_Utils_ap(
									A2(
										$elm$core$List$map,
										function ($) {
											return $.id;
										},
										i.incomings),
									_Utils_ap(
										A2(
											$elm$core$List$map,
											function ($) {
												return $.id;
											},
											i.outgoings),
										q)));
							var $temp$d = A2($elm_community$intdict$IntDict$remove, t, d),
								$temp$l = lsuite;
							d = $temp$d;
							l = $temp$l;
							continue aux;
						}
					}
				}
			});
		var ids = $elm_community$intdict$IntDict$keys(
			A2(aux, inc, li));
		return A4(
			$author$project$Polygraph$updateList,
			ids,
			function (_v4) {
				var n = _v4.n;
				return {isIn: false, n: n};
			},
			function (_v5) {
				var e = _v5.e;
				return {e: e, isIn: false};
			},
			A3(
				$author$project$Polygraph$map,
				F2(
					function (id, n) {
						return {isIn: true, n: n};
					}),
				F2(
					function (id, e) {
						return {e: e, isIn: true};
					}),
				$author$project$Polygraph$Graph(g)));
	});
var $author$project$GraphProof$prefixProofStep = F2(
	function (id, r) {
		return _Utils_update(
			r,
			{
				endChain: A2($elm$core$List$cons, id, r.endChain),
				startOffset: r.startOffset + 1
			});
	});
var $elm_community$list_extra$List$Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				if (m.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					if (!m.a.b) {
						return $elm$core$Maybe$Nothing;
					} else {
						var _v1 = m.a;
						var x = _v1.a;
						var xs_ = _v1.b;
						return _Utils_eq(e, x) ? $elm$core$Maybe$Just(xs_) : $elm$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			$elm$core$List$foldl,
			step,
			$elm$core$Maybe$Just(xs),
			prefix);
	});
var $author$project$GraphProof$applyDiag = F2(
	function (d, l) {
		var _v0 = A2(
			$elm_community$list_extra$List$Extra$stripPrefix,
			A2(
				$elm$core$List$map,
				function ($) {
					return $.id;
				},
				d.lhs),
			l);
		if (_v0.$ === 'Nothing') {
			if (!l.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var t = l.a;
				var q = l.b;
				return A2(
					$elm$core$Maybe$map,
					$author$project$GraphProof$prefixProofStep(t),
					A2($author$project$GraphProof$applyDiag, d, q));
			}
		} else {
			var tail = _v0.a;
			return $elm$core$Maybe$Just(
				{
					backOffset: $elm$core$List$length(tail),
					diag: d,
					endChain: _Utils_ap(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.id;
							},
							d.rhs),
						tail),
					startOffset: 0
				});
		}
	});
var $elm_community$list_extra$List$Extra$findMap = F2(
	function (f, list) {
		findMap:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var a = list.a;
				var tail = list.b;
				var _v1 = f(a);
				if (_v1.$ === 'Just') {
					var b = _v1.a;
					return $elm$core$Maybe$Just(b);
				} else {
					var $temp$f = f,
						$temp$list = tail;
					f = $temp$f;
					list = $temp$list;
					continue findMap;
				}
			}
		}
	});
var $author$project$GraphProof$commuteProof = F2(
	function (diags, l) {
		var _v0 = A2(
			$elm_community$list_extra$List$Extra$findMap,
			function (d) {
				return A2($author$project$GraphProof$applyDiag, d, l);
			},
			diags);
		if (_v0.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var step = _v0.a;
			return A2(
				$elm$core$List$cons,
				step,
				A2(
					$author$project$GraphProof$commuteProof,
					A2($elm_community$list_extra$List$Extra$remove, step.diag, diags),
					step.endChain));
		}
	});
var $author$project$GraphProof$finishedProof = function (_v0) {
	var statement = _v0.statement;
	var proof = _v0.proof;
	return A2(
		$elm$core$Maybe$withDefault,
		false,
		A2(
			$elm$core$Maybe$map,
			function (h) {
				return _Utils_eq(
					h.endChain,
					A2(
						$elm$core$List$map,
						function ($) {
							return $.id;
						},
						statement.rhs));
			},
			$elm$core$List$head(
				$elm$core$List$reverse(proof))));
};
var $author$project$Geometry$Point$flipAngle = function (a) {
	return a + $elm$core$Basics$pi;
};
var $elm_community$list_extra$List$Extra$last = function (items) {
	last:
	while (true) {
		if (!items.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!items.b.b) {
				var x = items.a;
				return $elm$core$Maybe$Just(x);
			} else {
				var rest = items.b;
				var $temp$items = rest;
				items = $temp$items;
				continue last;
			}
		}
	}
};
var $author$project$ListExtraExtra$permute = function (l) {
	if (!l.b) {
		return _List_Nil;
	} else {
		var t = l.a;
		var q = l.b;
		return _Utils_ap(
			q,
			_List_fromArray(
				[t]));
	}
};
var $elm_community$list_extra$List$Extra$zip = $elm$core$List$map2($elm$core$Tuple$pair);
var $author$project$ListExtraExtra$succCyclePairs = function (l) {
	var _v0 = A2(
		$elm_community$list_extra$List$Extra$zip,
		l,
		$author$project$ListExtraExtra$permute(l));
	if (_v0.b && (!_v0.b.b)) {
		return _List_Nil;
	} else {
		var r = _v0;
		return r;
	}
};
var $author$project$GraphProof$getAllValidDiagrams = function (g) {
	var inc = A2(
		$elm_community$intdict$IntDict$map,
		F2(
			function (_v10, i) {
				return $author$project$ListExtraExtra$succCyclePairs(
					A2(
						$elm$core$List$sortBy,
						function (_v11) {
							var edge = _v11.edge;
							var incoming = _v11.incoming;
							return incoming ? edge.label.angle : $author$project$Geometry$Point$normaliseAngle(
								$author$project$Geometry$Point$flipAngle(edge.label.angle));
						},
						_Utils_ap(
							A2(
								$elm$core$List$map,
								function (e) {
									return {edge: e, incoming: true};
								},
								i.incomings),
							A2(
								$elm$core$List$map,
								function (e) {
									return {edge: e, incoming: false};
								},
								i.outgoings))));
			}),
		$author$project$Polygraph$incidence(g));
	var treatEdge = F2(
		function (_v8, _v9) {
			var e1 = _v8.a;
			var e2 = _v8.b;
			var start = _v9.a;
			var next1 = _v9.b;
			var next2 = _v9.c;
			var _v7 = _Utils_Tuple2(e1.incoming, e2.incoming);
			if (_v7.a) {
				if (_v7.b) {
					return _Utils_Tuple3(start, next1, next2);
				} else {
					return _Utils_Tuple3(
						start,
						A3($elm_community$intdict$IntDict$insert, e1.edge.id, e2.edge, next1),
						next2);
				}
			} else {
				if (!_v7.b) {
					return _Utils_Tuple3(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(e2.edge, e1.edge),
							start),
						next1,
						next2);
				} else {
					return _Utils_Tuple3(
						start,
						next1,
						A3($elm_community$intdict$IntDict$insert, e2.edge.id, e1.edge, next2));
				}
			}
		});
	var buildNextStarts = F2(
		function (es, _v6) {
			var start = _v6.a;
			var next1 = _v6.b;
			var next2 = _v6.c;
			return A3(
				$elm$core$List$foldl,
				treatEdge,
				_Utils_Tuple3(start, next1, next2),
				es);
		});
	var _v0 = A3(
		$elm_community$intdict$IntDict$foldl,
		function (_v1) {
			return buildNextStarts;
		},
		_Utils_Tuple3(_List_Nil, $elm_community$intdict$IntDict$empty, $elm_community$intdict$IntDict$empty),
		inc);
	var start = _v0.a;
	var next1 = _v0.b;
	var next2 = _v0.c;
	var buildBranch = F2(
		function (next, startEdge) {
			var _v2 = A2($elm_community$intdict$IntDict$get, startEdge.id, next);
			if (_v2.$ === 'Nothing') {
				return _List_fromArray(
					[startEdge]);
			} else {
				var e = _v2.a;
				return A2(
					$elm$core$List$cons,
					startEdge,
					A2(buildBranch, next, e));
			}
		});
	var diags = A2(
		$elm$core$List$map,
		function (_v5) {
			var rhs = _v5.a;
			var lhs = _v5.b;
			return {
				lhs: A2(buildBranch, next2, lhs),
				rhs: A2(buildBranch, next1, rhs)
			};
		},
		start);
	var validDiag = function (_v4) {
		var lhs = _v4.lhs;
		var rhs = _v4.rhs;
		var _v3 = _Utils_Tuple2(
			$elm_community$list_extra$List$Extra$last(lhs),
			$elm_community$list_extra$List$Extra$last(rhs));
		if ((_v3.a.$ === 'Just') && (_v3.b.$ === 'Just')) {
			var e1 = _v3.a.a;
			var e2 = _v3.b.a;
			return _Utils_eq(e1.to, e2.to);
		} else {
			return false;
		}
	};
	var validDiags = A2($elm$core$List$filter, validDiag, diags);
	return validDiags;
};
var $author$project$GraphProof$invertDiagram = function (_v0) {
	var lhs = _v0.lhs;
	var rhs = _v0.rhs;
	return {lhs: rhs, rhs: lhs};
};
var $author$project$Geometry$Point$countRoundsAngle = function (a) {
	return $elm$core$Basics$round(a / (2 * $elm$core$Basics$pi));
};
var $author$project$Geometry$Point$sumAngles = function (l) {
	if (!l.b) {
		return 0;
	} else {
		if (!l.b.b) {
			return 0;
		} else {
			var a = l.a;
			var _v1 = l.b;
			var b = _v1.a;
			var tl = _v1.b;
			var sr = A2($author$project$Geometry$Point$distanceAngleSigned, a, b);
			return sr + $author$project$Geometry$Point$sumAngles(
				A2($elm$core$List$cons, b, tl));
		}
	}
};
var $author$project$Geometry$Point$countRounds = function (l) {
	return $author$project$Geometry$Point$countRoundsAngle(
		$author$project$Geometry$Point$sumAngles(l));
};
var $author$project$GraphProof$isBorder = function (_v0) {
	var lhs = _v0.lhs;
	var rhs = _v0.rhs;
	var makeAngles = $elm$core$List$map(
		A2(
			$elm$core$Basics$composeR,
			function ($) {
				return $.label;
			},
			function ($) {
				return $.angle;
			}));
	var anglesRhs = A2(
		$elm$core$List$map,
		$author$project$Geometry$Point$flipAngle,
		makeAngles(rhs));
	var anglesLhs = makeAngles(lhs);
	var angles = A2(
		$elm$core$List$cons,
		A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$head(anglesRhs)),
		_Utils_ap(
			anglesLhs,
			$elm$core$List$reverse(anglesRhs)));
	var _v1 = A2(
		$elm$core$Debug$log,
		'lhs',
		A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.label;
				},
				function ($) {
					return $.label;
				}),
			lhs));
	var _v2 = A2($elm$core$Debug$log, 'lhs-angles', anglesLhs);
	return _Utils_eq(
		A2(
			$elm$core$Debug$log,
			'rounds',
			$author$project$Geometry$Point$countRounds(angles)),
		-1);
};
var $author$project$GraphProof$nameIdentities = A4(
	$author$project$Polygraph$mapRecAll,
	function (n) {
		return n.label;
	},
	function (n) {
		return n.label;
	},
	F2(
		function (_v0, n) {
			return n;
		}),
	F4(
		function (_v1, fromLabel, _v2, l) {
			return _Utils_update(
				l,
				{
					label: ((l.label === '') && l.identity) ? ('|' + (fromLabel + '|')) : l.label
				});
		}));
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $author$project$GraphProof$statementToString = function (d) {
	var edgeToString = A2(
		$elm$core$Basics$composeR,
		$elm$core$List$map(
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.label;
				},
				function ($) {
					return $.label;
				})),
		$elm$core$String$join(' · '));
	return '{ ' + (edgeToString(d.lhs) + (' = ' + (edgeToString(d.rhs) + ' }')));
};
var $author$project$GraphProof$fullProofs = function (g0) {
	var g = $author$project$GraphProof$nameIdentities(g0);
	var diags = $author$project$GraphProof$getAllValidDiagrams(g);
	var _v0 = A2($elm$core$List$partition, $author$project$GraphProof$isBorder, diags);
	var bigDiags = _v0.a;
	var smallDiags = _v0.b;
	var _v1 = A2(
		$elm$core$Debug$log,
		'nombre de gros diagrammes: ',
		$elm$core$List$length(bigDiags));
	var _v2 = A2(
		$elm$core$List$map,
		A2(
			$elm$core$Basics$composeL,
			$elm$core$Debug$log(' big:'),
			$author$project$GraphProof$statementToString),
		bigDiags);
	var _v3 = A2(
		$elm$core$Debug$log,
		'nombre de petits diagrammes: ',
		$elm$core$List$length(smallDiags));
	var _v4 = A2(
		$elm$core$List$map,
		A2(
			$elm$core$Basics$composeL,
			$elm$core$Debug$log(' small:'),
			$author$project$GraphProof$statementToString),
		smallDiags);
	return A2(
		$elm$core$List$filter,
		$author$project$GraphProof$finishedProof,
		A2(
			$elm$core$List$map,
			function (d) {
				return {
					proof: A2(
						$author$project$GraphProof$commuteProof,
						smallDiags,
						A2(
							$elm$core$List$map,
							function ($) {
								return $.id;
							},
							d.lhs)),
					statement: d
				};
			},
			A2($elm$core$List$map, $author$project$GraphProof$invertDiagram, bigDiags)));
};
var $author$project$GraphProof$angleDir = F2(
	function (dir, edge) {
		return dir ? edge.angle : $author$project$Geometry$Point$flipAngle(edge.angle);
	});
var $elm_community$list_extra$List$Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _v1) {
				var y = _v1.a;
				var fy = _v1.b;
				var fx = f(x);
				return (_Utils_cmp(fx, fy) > 0) ? _Utils_Tuple2(x, fx) : _Utils_Tuple2(y, fy);
			});
		if (ls.b) {
			if (!ls.b.b) {
				var l_ = ls.a;
				return $elm$core$Maybe$Just(l_);
			} else {
				var l_ = ls.a;
				var ls_ = ls.b;
				return $elm$core$Maybe$Just(
					A3(
						$elm$core$List$foldl,
						maxBy,
						_Utils_Tuple2(
							l_,
							f(l_)),
						ls_).a);
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$GraphProof$extremePath = F4(
	function (direction, target, gi, e) {
		var g = A2($author$project$Polygraph$removeEdge, e.id, gi);
		var ret = _Utils_Tuple2(e, direction);
		var end = function (dir) {
			return dir ? function ($) {
				return $.to;
			} : function ($) {
				return $.from;
			};
		};
		var nfId = A2(end, direction, e);
		if (_Utils_eq(nfId, target)) {
			return _List_fromArray(
				[ret]);
		} else {
			var angle = A2($author$project$GraphProof$angleDir, direction, e.label);
			var makeStuff = F2(
				function (dir, l) {
					return A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(x, dir);
						},
						l);
				});
			var outs = A2(
				makeStuff,
				true,
				A2($author$project$Polygraph$outgoings, nfId, g));
			var ins = A2(
				makeStuff,
				false,
				A2($author$project$Polygraph$incomings, nfId, g));
			var finalAngle = function (_v2) {
				var edge = _v2.a;
				var dir = _v2.b;
				return A2(
					$author$project$Geometry$Point$distanceAngleSigned,
					angle,
					A2($author$project$GraphProof$angleDir, dir, edge.label));
			};
			var l = _Utils_ap(ins, outs);
			var f = A2($elm_community$list_extra$List$Extra$maximumBy, finalAngle, l);
			if (f.$ === 'Nothing') {
				return _List_fromArray(
					[ret]);
			} else {
				var _v1 = f.a;
				var eNext = _v1.a;
				var dir = _v1.b;
				return A2(
					$elm$core$List$cons,
					ret,
					A4($author$project$GraphProof$extremePath, dir, target, g, eNext));
			}
		}
	});
var $author$project$GraphProof$loopFrom = F3(
	function (direction, g, e) {
		return A4(
			$author$project$GraphProof$extremePath,
			direction,
			direction ? e.from : e.to,
			g,
			e);
	});
var $author$project$GraphProof$loopToDiagram = function (edges) {
	var findInitial = function (l) {
		if (!l.b) {
			return 0;
		} else {
			if (((!l.a.b) && l.b.b) && l.b.a.b) {
				var _v1 = l.a;
				var _v2 = l.b;
				var _v3 = _v2.a;
				return 1;
			} else {
				var q = l.b;
				return findInitial(q) + 1;
			}
		}
	};
	var _v4 = A2(
		$elm_community$list_extra$List$Extra$splitAt,
		findInitial(edges),
		edges);
	var l1 = _v4.a;
	var l2 = _v4.b;
	var lordered = _Utils_ap(l2, l1);
	var _v5 = A2($elm$core$List$partition, $elm$core$Tuple$second, lordered);
	var lhs0 = _v5.a;
	var rhs0 = _v5.b;
	var rhs = A2(
		$elm$core$List$map,
		$elm$core$Tuple$first,
		$elm$core$List$reverse(rhs0));
	var lhs = A2($elm$core$List$map, $elm$core$Tuple$first, lhs0);
	return {lhs: lhs, rhs: rhs};
};
var $author$project$GraphProof$diagramFrom = F3(
	function (dir, g, e) {
		var loopEdges = A3($author$project$GraphProof$loopFrom, dir, g, e);
		return $author$project$GraphProof$loopToDiagram(loopEdges);
	});
var $author$project$GraphProof$invertProofstepDiag = function (s) {
	return _Utils_update(
		s,
		{
			diag: $author$project$GraphProof$invertDiagram(s.diag)
		});
};
var $author$project$GraphProof$isEmptyBranch = function (l) {
	if (l.b && (!l.b.b)) {
		var e = l.a;
		return (e.label.label === '') ? $elm$core$Maybe$Just(e.id) : $elm$core$Maybe$Nothing;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$maybe_extra$Maybe$Extra$or = F2(
	function (ma, mb) {
		if (ma.$ === 'Nothing') {
			return mb;
		} else {
			return ma;
		}
	});
var $author$project$GraphProof$generateIncompleteProofStepFromDiagram = F2(
	function (g, d) {
		var flipIf = F2(
			function (b, diag) {
				return b ? $author$project$GraphProof$invertDiagram(diag) : diag;
			});
		var surroundingDiag = function (dir) {
			var d2 = A2(flipIf, dir, d);
			return A2(
				$elm$core$Maybe$andThen,
				function (id) {
					var g2 = A2($author$project$Polygraph$removeEdge, id, g);
					return A2(
						$elm$core$Maybe$andThen,
						A2(
							$elm$core$Basics$composeR,
							A2($author$project$GraphProof$diagramFrom, !dir, g2),
							A2(
								$elm$core$Basics$composeR,
								flipIf(dir),
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.lhs;
									},
									A2(
										$elm$core$Basics$composeR,
										$elm$core$List$map(
											function ($) {
												return $.id;
											}),
										A2(
											$elm$core$Basics$composeR,
											$author$project$GraphProof$applyDiag(d2),
											$elm$core$Maybe$map(
												dir ? $author$project$GraphProof$invertProofstepDiag : $elm$core$Basics$identity)))))),
						$elm$core$List$head(d2.lhs));
				},
				$author$project$GraphProof$isEmptyBranch(d2.rhs));
		};
		var step = A2(
			$elm_community$maybe_extra$Maybe$Extra$or,
			surroundingDiag(true),
			surroundingDiag(false));
		return step;
	});
var $author$project$GraphProof$repeat = F2(
	function (n, s) {
		switch (n) {
			case 0:
				return _List_Nil;
			case 1:
				return _List_fromArray(
					['  ' + s]);
			default:
				return _List_fromArray(
					[
						'  do ' + ($elm$core$String$fromInt(n) + (' ' + s))
					]);
		}
	});
var $author$project$GraphProof$write0 = F2(
	function (n, s) {
		return (!n) ? _List_Nil : s;
	});
var $author$project$GraphProof$getToThePoint = F2(
	function (startOffset, backOffset) {
		return A2(
			$elm$core$String$join,
			'\n',
			_Utils_ap(
				A2($author$project$GraphProof$repeat, backOffset, 'apply cancel_postcomposition.'),
				A2(
					$author$project$GraphProof$write0,
					startOffset,
					_Utils_ap(
						_List_fromArray(
							['  repeat rewrite assoc\'.']),
						_Utils_ap(
							A2($author$project$GraphProof$repeat, startOffset, 'apply cancel_precomposition.'),
							_List_fromArray(
								['  repeat rewrite assoc.']))))));
	});
var $elm_community$maybe_extra$Maybe$Extra$isJust = function (m) {
	if (m.$ === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var $author$project$GraphProof$symmetryStr = 'apply pathsinv0.';
var $author$project$GraphProof$incompleteProofStepToString = function (_v0) {
	var startOffset = _v0.startOffset;
	var backOffset = _v0.backOffset;
	var diag = _v0.diag;
	var invert = $elm_community$maybe_extra$Maybe$Extra$isJust(
		$author$project$GraphProof$isEmptyBranch(diag.lhs));
	var symList = invert ? _List_fromArray(
		[$author$project$GraphProof$symmetryStr]) : _List_Nil;
	return A2(
		$elm$core$String$join,
		'\n',
		_Utils_ap(
			symList,
			_Utils_ap(
				_List_fromArray(
					[
						'etrans.',
						'{',
						A2($author$project$GraphProof$getToThePoint, startOffset, backOffset),
						'  (* remove this after copying the goal *)',
						'  duplicate_goal.',
						'  admit.',
						'  (* copy the result in the proof editor *)',
						'  norm_graph.',
						'  admit.',
						'}'
					]),
				_Utils_ap(
					A2(
						$author$project$GraphProof$write0,
						startOffset,
						_List_fromArray(
							['repeat rewrite assoc.'])),
					symList))));
};
var $author$project$GraphDefs$isNormal = A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$GraphDefs$isPullshout);
var $author$project$GraphDefs$isNormalId = F2(
	function (g, id) {
		return A2(
			$elm$core$Maybe$withDefault,
			false,
			A4(
				$author$project$Polygraph$get,
				id,
				$elm$core$Basics$always(true),
				$author$project$GraphDefs$isNormal,
				g));
	});
var $author$project$GraphDefs$selectedEdges = function (g) {
	return A2(
		$elm$core$List$filter,
		A2(
			$elm$core$Basics$composeR,
			function ($) {
				return $.label;
			},
			$author$project$GraphDefs$fieldSelect(g)),
		$author$project$Polygraph$edges(g));
};
var $author$project$GraphDefs$selectedNodes = function (g) {
	return A2(
		$elm$core$List$filter,
		A2(
			$elm$core$Basics$composeR,
			function ($) {
				return $.label;
			},
			$author$project$GraphDefs$fieldSelect(g)),
		$author$project$Polygraph$nodes(g));
};
var $author$project$GraphDefs$selectedId = function (g) {
	var _v0 = _Utils_ap(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.id;
			},
			$author$project$GraphDefs$selectedNodes(g)),
		A2(
			$elm$core$List$map,
			function ($) {
				return $.id;
			},
			$author$project$GraphDefs$selectedEdges(g)));
	if (_v0.b && (!_v0.b.b)) {
		var x = _v0.a;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Modes$NewArrow$initialise = function (m) {
	return $author$project$Model$noCmd(
		A2(
			$elm$core$Maybe$withDefault,
			m,
			A2(
				$elm$core$Maybe$map,
				function (chosenNode) {
					return _Utils_update(
						m,
						{
							mode: $author$project$Modes$NewArrow(
								{chosenNode: chosenNode, inverted: false, pos: $author$project$InputPosition$InputPosMouse, style: $author$project$ArrowStyle$empty})
						});
				},
				A2(
					$elm_community$maybe_extra$Maybe$Extra$filter,
					$author$project$GraphDefs$isNormalId(m.graph),
					$author$project$GraphDefs$selectedId(m.graph)))));
};
var $author$project$GraphDefs$selectedEdge = function (g) {
	var _v0 = $author$project$GraphDefs$selectedEdges(g);
	if (_v0.b && (!_v0.b.b)) {
		var x = _v0.a;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$GraphDefs$selectedEdgeId = A2(
	$elm$core$Basics$composeR,
	$author$project$GraphDefs$selectedEdge,
	$elm$core$Maybe$map(
		function ($) {
			return $.id;
		}));
var $author$project$Modes$SplitArrow$initialise = function (m) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Model$switch_Default(m),
		A2(
			$elm$core$Maybe$andThen,
			function (id) {
				return A2(
					$elm$core$Maybe$andThen,
					function (e) {
						return A2(
							$elm$core$Maybe$map,
							function (l) {
								return $author$project$Model$noCmd(
									_Utils_update(
										m,
										{
											mode: $author$project$Modes$SplitArrow(
												{
													chosenEdge: id,
													guessPos: true,
													label: A2(
														$author$project$GraphDefs$mapDetails,
														$elm$core$Basics$always(l),
														e.label),
													labelOnSource: true,
													pos: $author$project$InputPosition$InputPosMouse,
													source: e.from,
													target: e.to
												})
										}));
							},
							$author$project$GraphDefs$filterNormalEdges(e.label.details));
					},
					A2($author$project$Polygraph$getEdge, id, m.graph));
			},
			$author$project$GraphDefs$selectedEdgeId(m.graph)));
};
var $author$project$GraphDefs$selectedNode = function (g) {
	var _v0 = $author$project$GraphDefs$selectedNodes(g);
	if (_v0.b && (!_v0.b.b)) {
		var x = _v0.a;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Modes$Square$initialise = function (m) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Model$noCmd(m),
		A2(
			$elm$core$Maybe$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.id;
				},
				A2($author$project$Modes$Square$square_updatePossibility, m, 0)),
			$author$project$GraphDefs$selectedNode(m.graph)));
};
var $author$project$Modes$EnlargeMode = function (a) {
	return {$: 'EnlargeMode', a: a};
};
var $author$project$Main$initialiseEnlarge = function (model) {
	return _Utils_update(
		model,
		{
			mode: $author$project$Modes$EnlargeMode(
				{orig: model.mousePos, pos: $author$project$InputPosition$InputPosMouse})
		});
};
var $author$project$Modes$Move = function (a) {
	return {$: 'Move', a: a};
};
var $author$project$GraphDefs$isEmptySelection = function (go) {
	return (!A3(
		$author$project$Polygraph$any,
		function ($) {
			return $.selected;
		},
		function ($) {
			return $.selected;
		},
		go)) && (!A3(
		$author$project$Polygraph$any,
		function ($) {
			return $.weaklySelected;
		},
		function ($) {
			return $.weaklySelected;
		},
		go));
};
var $author$project$Main$initialiseMoveMode = F2(
	function (save, model) {
		return _Utils_update(
			model,
			{
				mode: $author$project$GraphDefs$isEmptySelection(model.graph) ? $author$project$Modes$DefaultMode : $author$project$Modes$Move(
					{orig: model.mousePos, pos: $author$project$InputPosition$InputPosMouse, save: save})
			});
	});
var $author$project$Modes$ResizeMode = function (a) {
	return {$: 'ResizeMode', a: a};
};
var $author$project$Main$initialise_Resize = function (model) {
	return _Utils_update(
		model,
		{
			mode: $author$project$Modes$ResizeMode(
				{onlyGrid: false, sizeGrid: model.sizeGrid})
		});
};
var $author$project$Polygraph$invertEdge = F2(
	function (id, _v0) {
		var g = _v0.a;
		return $author$project$Polygraph$Graph(
			A3(
				$elm_community$intdict$IntDict$update,
				id,
				function (e) {
					if ((e.$ === 'Just') && (e.a.$ === 'EdgeObj')) {
						var _v2 = e.a;
						var i1 = _v2.a;
						var i2 = _v2.b;
						var l = _v2.c;
						return $elm$core$Maybe$Just(
							A3($author$project$Polygraph$EdgeObj, i2, i1, l));
					} else {
						return e;
					}
				},
				g));
	});
var $author$project$Main$jumpToId = _Platform_outgoingPort('jumpToId', $elm$json$Json$Encode$string);
var $elm$core$Basics$atan = _Basics_atan;
var $author$project$Geometry$Point$pointToAngle = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return ((!y) && (x <= 0)) ? $elm$core$Basics$pi : (2 * $elm$core$Basics$atan(
		y / (x + $author$project$Geometry$Point$radius(
			_Utils_Tuple2(x, y)))));
};
var $author$project$Main$promptEquation = _Platform_outgoingPort(
	'promptEquation',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Main$promptFindReplace = _Platform_outgoingPort(
	'promptFindReplace',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$GraphProof$debugEdgeName = function (id) {
	return 'e' + $elm$core$String$fromInt(id);
};
var $author$project$GraphProof$edgesOfDiag = function (d) {
	var setOf = function (e) {
		return _Utils_Tuple2(
			e.id,
			{from: e.from, to: e.to});
	};
	return $elm_community$intdict$IntDict$fromList(
		_Utils_ap(
			A2($elm$core$List$map, setOf, d.lhs),
			A2($elm$core$List$map, setOf, d.rhs)));
};
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm_community$list_extra$List$Extra$reverseMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $author$project$GraphProof$nodesOfDiag = function (d) {
	return _Utils_ap(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.from;
			},
			d.lhs),
		A2(
			$elm_community$list_extra$List$Extra$reverseMap,
			function ($) {
				return $.to;
			},
			d.rhs));
};
var $author$project$GraphProof$proofStepToString = function (_v0) {
	var startOffset = _v0.startOffset;
	var backOffset = _v0.backOffset;
	var diag = _v0.diag;
	return A2(
		$elm$core$String$join,
		'\n',
		_Utils_ap(
			_List_fromArray(
				[
					'assert(eq : ' + ($author$project$GraphProof$statementToString(diag) + ').'),
					'{',
					'  admit.',
					'}',
					'etrans.',
					'{',
					A2($author$project$GraphProof$getToThePoint, startOffset, backOffset),
					'  apply eq.',
					'}'
				]),
			_Utils_ap(
				A2(
					$author$project$GraphProof$write0,
					startOffset,
					_List_fromArray(
						['repeat rewrite assoc.'])),
				_List_fromArray(
					['clear eq.']))));
};
var $author$project$GraphProof$renameDebugDiag = function (diag) {
	var renameEdge = function (e) {
		var label = e.label;
		return _Utils_update(
			e,
			{
				label: _Utils_update(
					label,
					{
						label: $author$project$GraphProof$debugEdgeName(e.id)
					})
			});
	};
	return {
		lhs: A2($elm$core$List$map, renameEdge, diag.lhs),
		rhs: A2($elm$core$List$map, renameEdge, diag.rhs)
	};
};
var $author$project$GraphProof$renameDebugProofStep = function (step) {
	return _Utils_update(
		step,
		{
			diag: $author$project$GraphProof$renameDebugDiag(step.diag)
		});
};
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0.a;
		var dict2 = _v1.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$union, dict1, dict2));
	});
var $author$project$GraphProof$proofStatementToDebugString = function (st) {
	var nodes = A3(
		$elm$core$List$foldl,
		$elm$core$Set$union,
		$elm$core$Set$empty,
		A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.diag;
				},
				A2($elm$core$Basics$composeR, $author$project$GraphProof$nodesOfDiag, $elm$core$Set$fromList)),
			st.proof));
	var edges = A3(
		$elm$core$List$foldl,
		$elm_community$intdict$IntDict$union,
		$elm_community$intdict$IntDict$empty,
		A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.diag;
				},
				$author$project$GraphProof$edgesOfDiag),
			st.proof));
	var nidS = function (id) {
		return 'o' + $elm$core$String$fromInt(id);
	};
	return 'Goal ' + ('∏ (C : category)\n  ' + (A2(
		$elm$core$String$join,
		'',
		A2(
			$elm$core$List$map,
			function (id) {
				return '(' + (nidS(id) + ' : C)');
			},
			$elm$core$Set$toList(nodes))) + ('\n  ' + (A2(
		$elm$core$String$join,
		'',
		A2(
			$elm$core$List$map,
			function (_v0) {
				var id = _v0.a;
				var from = _v0.b.from;
				var to = _v0.b.to;
				return '(' + ($author$project$GraphProof$debugEdgeName(id) + (' : ' + (nidS(from) + (' --> ' + (nidS(to) + ')')))));
			},
			$elm_community$intdict$IntDict$toList(edges))) + (',\n  ' + ($author$project$GraphProof$statementToString(
		$author$project$GraphProof$renameDebugDiag(st.statement)) + ('.\n\nintros.\n' + (A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			A2($elm$core$Basics$composeR, $author$project$GraphProof$renameDebugProofStep, $author$project$GraphProof$proofStepToString),
			st.proof)) + '\n apply idpath.'))))))));
};
var $author$project$GraphProof$proofStatementToString = function (st) {
	return 'Goal ' + ($author$project$GraphProof$statementToString(st.statement) + ('.\n\n' + (A2(
		$elm$core$String$join,
		'\n',
		A2($elm$core$List$map, $author$project$GraphProof$proofStepToString, st.proof)) + ('\n apply idpath.' + '\nQed.'))));
};
var $author$project$Polygraph$drop = F2(
	function (fn, fe) {
		return A2(
			$author$project$Polygraph$filterMap,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$Just,
				$elm_community$maybe_extra$Maybe$Extra$filter(
					A2($elm$core$Basics$composeL, $elm$core$Basics$not, fn))),
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$Just,
				$elm_community$maybe_extra$Maybe$Extra$filter(
					A2($elm$core$Basics$composeL, $elm$core$Basics$not, fe))));
	});
var $author$project$GraphDefs$removeSelected = function (g) {
	var f = $author$project$GraphDefs$fieldSelect(g);
	return A3($author$project$Polygraph$drop, f, f, g);
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$Main$rename = function (model) {
	var ids = A2(
		$elm$core$Maybe$withDefault,
		_List_Nil,
		A2(
			$elm$core$Maybe$map,
			$elm$core$List$singleton,
			$author$project$GraphDefs$selectedId(model.graph)));
	return $author$project$Model$noCmd(
		A3($author$project$Model$initialise_RenameMode, true, ids, model));
};
var $author$project$GraphDefs$addNodesSelection = F2(
	function (g, f) {
		return A5(
			$author$project$Polygraph$mapRecAll,
			function ($) {
				return $.selected;
			},
			function ($) {
				return $.selected;
			},
			F2(
				function (_v0, n) {
					return _Utils_update(
						n,
						{
							selected: f(n) || n.selected
						});
				}),
			F4(
				function (_v1, s1, s2, e) {
					return _Utils_update(
						e,
						{selected: (s1 && s2) || e.selected});
				}),
			g);
	});
var $author$project$GraphDefs$selectAll = function (g) {
	return A2(
		$author$project$GraphDefs$addNodesSelection,
		g,
		$elm$core$Basics$always(true));
};
var $author$project$GraphDefs$addWeaklySelected = A2(
	$author$project$Polygraph$map,
	F2(
		function (_v0, n) {
			return _Utils_update(
				n,
				{selected: n.weaklySelected || n.selected});
		}),
	F2(
		function (_v1, n) {
			return _Utils_update(
				n,
				{selected: n.weaklySelected || n.selected});
		}));
var $author$project$Main$selectByClick = function (model) {
	return model.mouseOnCanvas ? _Utils_update(
		model,
		{
			graph: $author$project$GraphDefs$addWeaklySelected(
				model.specialKeys.shift ? model.graph : $author$project$GraphDefs$clearSelection(model.graph))
		}) : model;
};
var $author$project$GraphDefs$toProofGraph = A2(
	$elm$core$Basics$composeR,
	$author$project$GraphDefs$keepNormalEdges,
	A4(
		$author$project$Polygraph$mapRecAll,
		function (n) {
			return n.pos;
		},
		function (n) {
			return n.pos;
		},
		F2(
			function (_v0, n) {
				return {label: n.label, pos: n.pos};
			}),
		F4(
			function (_v1, fromP, toP, _v2) {
				var details = _v2.details;
				return {
					angle: $author$project$Geometry$Point$pointToAngle(
						A2($author$project$Geometry$Point$subtract, toP, fromP)),
					identity: details.style._double,
					label: details.label,
					pos: A2($author$project$Geometry$Point$middle, fromP, toP)
				};
			})));
var $author$project$Main$selectLoop = F2(
	function (direction, model) {
		var g = $author$project$GraphDefs$toProofGraph(model.graph);
		var edges = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				A2($author$project$GraphProof$loopFrom, direction, g),
				A2(
					$elm$core$Maybe$andThen,
					function (id) {
						return A2($author$project$Polygraph$getEdge, id, g);
					},
					$author$project$GraphDefs$selectedEdgeId(model.graph))));
		var diag = $author$project$GraphProof$loopToDiagram(edges);
		var _v0 = A2(
			$elm$core$Debug$log,
			'sel lhs',
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.label;
					},
					function ($) {
						return $.label;
					}),
				diag.lhs));
		var _v1 = A2(
			$elm$core$Debug$log,
			'sel rhs',
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.label;
					},
					function ($) {
						return $.label;
					}),
				diag.rhs));
		var _v2 = A2(
			$elm$core$Debug$log,
			'isBorder?',
			$author$project$GraphProof$isBorder(diag));
		return _Utils_update(
			model,
			{
				graph: A3(
					$elm$core$List$foldl,
					function (e) {
						return A2(
							$author$project$Polygraph$updateEdge,
							e,
							function (n) {
								return _Utils_update(
									n,
									{selected: true});
							});
					},
					$author$project$GraphDefs$clearSelection(model.graph),
					A2(
						$elm$core$List$map,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$first,
							function ($) {
								return $.id;
							}),
						edges))
			});
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $author$project$Geometry$Point$isInPoly = F2(
	function (pos, l) {
		var angles = A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				$author$project$Geometry$Point$subtract(pos),
				$author$project$Geometry$Point$pointToAngle),
			l);
		var anglesLoop = $elm$core$List$sum(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var a = _v0.a;
					var b = _v0.b;
					return A2($author$project$Geometry$Point$distanceAngleSigned, a, b);
				},
				$author$project$ListExtraExtra$succCyclePairs(angles)));
		return $author$project$Geometry$Point$countRoundsAngle(anglesLoop) === 1;
	});
var $author$project$GraphDefs$getSurroundingDiagrams = F2(
	function (pos, gi) {
		var gp = $author$project$GraphDefs$toProofGraph(gi);
		var isInDiag = function (d) {
			return A2(
				$author$project$Geometry$Point$isInPoly,
				pos,
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.label;
						},
						function ($) {
							return $.pos;
						}),
					A2(
						$author$project$Polygraph$getNodes,
						$author$project$GraphProof$nodesOfDiag(d),
						gi)));
		};
		return A2(
			$elm$core$List$filter,
			isInDiag,
			$author$project$GraphProof$getAllValidDiagrams(gp));
	});
var $author$project$GraphDefs$selectEdges = $elm$core$List$foldl(
	function (e) {
		return A2(
			$author$project$Polygraph$updateEdge,
			e,
			function (n) {
				return _Utils_update(
					n,
					{selected: true});
			});
	});
var $author$project$GraphDefs$selectSurroundingDiagram = F2(
	function (pos, gi) {
		var _v0 = A2($author$project$GraphDefs$getSurroundingDiagrams, pos, gi);
		if (!_v0.b) {
			return gi;
		} else {
			var d = _v0.a;
			var edges = $elm_community$intdict$IntDict$keys(
				$author$project$GraphProof$edgesOfDiag(d));
			return A2(
				$author$project$GraphDefs$selectEdges,
				$author$project$GraphDefs$clearSelection(gi),
				edges);
		}
	});
var $author$project$Polygraph$getEdges = F2(
	function (l, _v0) {
		var g = _v0.a;
		return A2(
			$elm$core$List$filterMap,
			function (_v1) {
				var id = _v1.a;
				var e = _v1.b;
				return A2($author$project$Polygraph$objEdge, id, e);
			},
			A2($author$project$IntDictExtra$getList, l, g));
	});
var $author$project$GraphProof$getIncompleteDiagram = F2(
	function (g, l) {
		if (!l.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var e = l.a;
			var makeDiag = function (dir) {
				var d = A3($author$project$GraphProof$diagramFrom, dir, g, e);
				return _Utils_eq(
					$elm$core$List$sort(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.id;
							},
							l)),
					$elm$core$List$sort(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.id;
							},
							_Utils_ap(d.lhs, d.rhs)))) ? $elm$core$Maybe$Just(
					A2($elm$core$Debug$log, 'oui!', d)) : $elm$core$Maybe$Nothing;
			};
			return A2(
				$elm_community$maybe_extra$Maybe$Extra$or,
				makeDiag(true),
				makeDiag(false));
		}
	});
var $author$project$GraphDefs$selectedIncompleteDiagram = function (g) {
	var gc = $author$project$GraphDefs$toProofGraph(g);
	return A2(
		$author$project$GraphProof$getIncompleteDiagram,
		gc,
		A2(
			$author$project$Polygraph$getEdges,
			A2(
				$elm$core$List$map,
				function ($) {
					return $.id;
				},
				$author$project$GraphDefs$selectedEdges(g)),
			gc));
};
var $author$project$Geometry$Point$snapToGrid = F2(
	function (sizeGrid, _v0) {
		var px = _v0.a;
		var py = _v0.b;
		var approx = function (c) {
			return ($elm$core$Basics$floor(c / sizeGrid) * sizeGrid) + (sizeGrid / 2);
		};
		return _Utils_Tuple2(
			approx(px),
			approx(py));
	});
var $author$project$GraphDefs$snapNodeToGrid = F2(
	function (sizeGrid, n) {
		return _Utils_update(
			n,
			{
				pos: A2($author$project$Geometry$Point$snapToGrid, sizeGrid, n.pos)
			});
	});
var $author$project$Model$peekHistory = function (m) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Model$toGraphInfo(m),
		$elm$core$List$head(m.history));
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Model$popHistory = function (m) {
	return _Utils_update(
		m,
		{
			history: A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$elm$core$List$tail(m.history))
		});
};
var $author$project$Model$undo = function (m) {
	return $author$project$Model$popHistory(
		A2(
			$author$project$Model$updateWithGraphInfo,
			m,
			$author$project$Model$peekHistory(m)));
};
var $author$project$Main$update_DefaultMode = F2(
	function (msg, model) {
		var delta_angle = $elm$core$Basics$pi / 5;
		var move = function (angle) {
			return $author$project$Model$noCmd(
				A2(
					$elm$core$Maybe$withDefault,
					model,
					A2(
						$elm$core$Maybe$map,
						function (n) {
							return A3($author$project$Model$addOrSetSel, false, n.id, model);
						},
						A2(
							$elm$core$Maybe$andThen,
							function (p) {
								return A2(
									$elm_community$list_extra$List$Extra$minimumBy,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.label;
										},
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.pos;
											},
											$author$project$Geometry$Point$distance(p))),
									A2(
										$author$project$Polygraph$filterNodes,
										model.graph,
										function (n) {
											return (!_Utils_eq(n.pos, p)) && A3(
												$author$project$Geometry$Point$angleWithInRange,
												delta_angle,
												angle,
												$author$project$Geometry$Point$pointToAngle(
													A2($author$project$Geometry$Point$subtract, n.pos, p)));
										}));
							},
							A2(
								$elm$core$Maybe$map,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.label;
									},
									function ($) {
										return $.pos;
									}),
								$author$project$GraphDefs$selectedNode(model.graph))))));
		};
		var fillBottom = F2(
			function (s, err) {
				var c = (s === '') ? $author$project$Main$alert(err) : $author$project$Main$jumpToId($author$project$HtmlDefs$bottomTextId);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{bottomText: s}),
					c);
			});
		var generateProof = function (stToString) {
			var s = A2(
				$elm$core$String$join,
				'\n\n',
				A2(
					$elm$core$List$map,
					stToString,
					$author$project$GraphProof$fullProofs(
						$author$project$GraphDefs$toProofGraph(model.graph))));
			return A2(fillBottom, s, 'No diagram found!');
		};
		var weaklySelect = function (id) {
			return $author$project$Model$noCmd(
				model.specialKeys.shift ? _Utils_update(
					model,
					{
						graph: A3($author$project$GraphDefs$addOrSetSel, true, id, model.graph)
					}) : _Utils_update(
					model,
					{
						graph: A2($author$project$GraphDefs$weaklySelect, id, model.graph)
					}));
		};
		var clearSel = $author$project$Model$noCmd(
			_Utils_update(
				model,
				{
					graph: $author$project$GraphDefs$clearSelection(model.graph)
				}));
		var createPoint = function (isMath) {
			var _v11 = A2(
				$author$project$Polygraph$newNode,
				model.graph,
				A3($author$project$GraphDefs$newNodeLabel, model.mousePos, '', isMath));
			var newGraph = _v11.a;
			var newId = _v11.b;
			var newModel = A3(
				$author$project$Model$addOrSetSel,
				false,
				newId,
				A2($author$project$Model$setSaveGraph, model, newGraph));
			return $author$project$Model$noCmd(
				A3(
					$author$project$Model$initialise_RenameMode,
					false,
					_List_fromArray(
						[newId]),
					newModel));
		};
		var increaseZBy = function (offset) {
			var _v10 = $author$project$GraphDefs$selectedEdgeId(model.graph);
			if (_v10.$ === 'Nothing') {
				return $author$project$Model$noCmd(model);
			} else {
				var id = _v10.a;
				return $author$project$Model$noCmd(
					A2(
						$author$project$Model$setSaveGraph,
						model,
						A3(
							$author$project$Polygraph$updateEdge,
							id,
							function (e) {
								return _Utils_update(
									e,
									{zindex: e.zindex + offset});
							},
							model.graph)));
			}
		};
		_v0$41:
		while (true) {
			switch (msg.$) {
				case 'MouseOn':
					var id = msg.a;
					return weaklySelect(id);
				case 'MouseClick':
					return $author$project$Model$noCmd(
						$author$project$Main$selectByClick(model));
				case 'MouseMove':
					return weaklySelect(
						A2($author$project$GraphDefs$closest, model.mousePos, model.graph));
				case 'MouseDown':
					return $author$project$Model$noCmd(
						_Utils_update(
							model,
							{
								mode: $author$project$Modes$RectSelect(model.mousePos)
							}));
				case 'CopyGraph':
					return _Utils_Tuple2(
						model,
						$author$project$Main$clipboardWriteGraph(
							{
								graph: $author$project$Format$LastVersion$toJSGraph(
									{
										graph: $author$project$GraphDefs$selectedGraph(model.graph),
										latexPreamble: model.latexPreamble,
										sizeGrid: model.sizeGrid
									}),
								version: $author$project$Format$LastVersion$version
							}));
				case 'EltDoubleClick':
					var n = msg.a;
					var e = msg.b;
					return $author$project$Model$noCmd(
						A3(
							$author$project$Model$initialise_RenameMode,
							true,
							_List_fromArray(
								[n]),
							model));
				case 'PasteGraph':
					var g = msg.a;
					return $author$project$Model$noCmd(
						A2(
							$author$project$Main$initialiseMoveMode,
							false,
							A2(
								$author$project$Model$setSaveGraph,
								model,
								A2(
									$author$project$Polygraph$union,
									$author$project$GraphDefs$clearSelection(model.graph),
									$author$project$GraphDefs$selectAll(g.graph)))));
				case 'KeyChanged':
					if (!msg.a) {
						if (msg.c.$ === 'Control') {
							switch (msg.c.a) {
								case 'Escape':
									return clearSel;
								case 'Delete':
									return $author$project$Model$noCmd(
										A2(
											$author$project$Model$setSaveGraph,
											model,
											$author$project$GraphDefs$removeSelected(model.graph)));
								default:
									break _v0$41;
							}
						} else {
							switch (msg.c.a.valueOf()) {
								case 'w':
									return clearSel;
								case 'e':
									return $author$project$Model$noCmd(
										$author$project$Main$initialiseEnlarge(model));
								case 'E':
									return _Utils_Tuple2(
										model,
										$author$project$Main$promptEquation(_Utils_Tuple0));
								case 'a':
									var k = msg.b;
									return (!k.ctrl) ? $author$project$Modes$NewArrow$initialise(model) : $author$project$Model$noCmd(
										_Utils_update(
											model,
											{
												graph: $author$project$GraphDefs$selectAll(model.graph)
											}));
								case 'd':
									return $author$project$Model$noCmd(
										_Utils_update(
											model,
											{mode: $author$project$Modes$DebugMode}));
								case 'g':
									return $author$project$Model$noCmd(
										A2($author$project$Main$initialiseMoveMode, true, model));
								case 'i':
									return $author$project$Model$noCmd(
										function () {
											var _v1 = $author$project$GraphDefs$selectedEdgeId(model.graph);
											if (_v1.$ === 'Just') {
												var id = _v1.a;
												return A2(
													$author$project$Model$setSaveGraph,
													model,
													A2($author$project$Polygraph$invertEdge, id, model.graph));
											} else {
												return model;
											}
										}());
								case 'L':
									return $author$project$Model$noCmd(
										A2($author$project$Main$selectLoop, true, model));
								case 'H':
									return $author$project$Model$noCmd(
										A2($author$project$Main$selectLoop, false, model));
								case 'G':
									return generateProof($author$project$GraphProof$proofStatementToString);
								case 'T':
									return generateProof($author$project$GraphProof$proofStatementToDebugString);
								case 'S':
									return $author$project$Model$noCmd(
										_Utils_update(
											model,
											{
												graph: A2($author$project$GraphDefs$selectSurroundingDiagram, model.mousePos, model.graph)
											}));
								case 'c':
									var k = msg.b;
									if (k.ctrl) {
										return $author$project$Model$noCmd(model);
									} else {
										if (k.alt) {
											return $author$project$Model$noCmd(
												_Utils_update(
													model,
													{mode: $author$project$Modes$CloneMode}));
										} else {
											var _v2 = A2(
												$elm_community$maybe_extra$Maybe$Extra$filter,
												$author$project$GraphDefs$isNormalId(model.graph),
												$author$project$GraphDefs$selectedEdgeId(model.graph));
											if (_v2.$ === 'Nothing') {
												return $author$project$Model$noCmd(model);
											} else {
												var id = _v2.a;
												return $author$project$Model$noCmd(
													_Utils_update(
														model,
														{
															mode: $author$project$Modes$CutHead(
																{duplicate: false, head: true, id: id})
														}));
											}
										}
									}
								case 'C':
									var gc = $author$project$GraphDefs$toProofGraph(model.graph);
									var s = A2(
										$elm$core$Maybe$withDefault,
										'',
										A2(
											$elm$core$Maybe$andThen,
											A2(
												$elm$core$Basics$composeR,
												$author$project$GraphProof$generateIncompleteProofStepFromDiagram(gc),
												$elm$core$Maybe$map($author$project$GraphProof$incompleteProofStepToString)),
											$author$project$GraphDefs$selectedIncompleteDiagram(model.graph)));
									return A2(fillBottom, s, 'No selected subdiagram found!');
								case 'q':
									return _Utils_Tuple2(
										model,
										$author$project$Main$promptFindReplace(_Utils_Tuple0));
								case 'Q':
									return _Utils_Tuple2(
										model,
										$author$project$Main$quicksaveGraph(
											{
												feedback: true,
												info: $author$project$Main$toJsGraphInfo(model)
											}));
								case 'R':
									return $author$project$Model$noCmd(
										$author$project$Main$initialise_Resize(model));
								case 'r':
									return $author$project$Main$rename(model);
								case 's':
									return $author$project$Modes$Square$initialise(model);
								case 't':
									return createPoint(false);
								case 'p':
									return createPoint(true);
								case '/':
									return $author$project$Modes$SplitArrow$initialise(model);
								case 'x':
									return $author$project$Model$noCmd(
										A2(
											$author$project$Model$setSaveGraph,
											model,
											$author$project$GraphDefs$removeSelected(model.graph)));
								case 'f':
									return $author$project$Model$noCmd(
										function () {
											var isSel = $author$project$GraphDefs$fieldSelect(model.graph);
											var _v3 = $author$project$GraphDefs$selectedNodes(model.graph);
											if (!_v3.b) {
												return model;
											} else {
												return A2(
													$author$project$Model$setSaveGraph,
													model,
													A3(
														$author$project$Polygraph$map,
														F2(
															function (_v4, n) {
																return isSel(n) ? A2($author$project$GraphDefs$snapNodeToGrid, model.sizeGrid, n) : n;
															}),
														$elm$core$Basics$always($elm$core$Basics$identity),
														model.graph));
											}
										}());
								case 'h':
									return move($elm$core$Basics$pi);
								case 'j':
									return move($elm$core$Basics$pi / 2);
								case 'k':
									return move((3 * $elm$core$Basics$pi) / 2);
								case 'l':
									return move(0);
								case 'u':
									var f = $author$project$GraphDefs$fieldSelect(model.graph);
									return $author$project$Model$noCmd(
										_Utils_update(
											model,
											{
												graph: A3(
													$author$project$Polygraph$map,
													F2(
														function (_v5, _v6) {
															var n = _v6.n;
															var isIn = _v6.isIn;
															return _Utils_update(
																n,
																{selected: isIn});
														}),
													F2(
														function (_v7, _v8) {
															var e = _v8.e;
															var isIn = _v8.isIn;
															return _Utils_update(
																e,
																{selected: isIn});
														}),
													A3($author$project$Polygraph$connectedClosure, f, f, model.graph))
											}));
								case 'z':
									var k = msg.b;
									return k.ctrl ? $author$project$Model$noCmd(
										$author$project$Model$undo(model)) : $author$project$Model$noCmd(model);
								case '+':
									var k = msg.b;
									return increaseZBy(1);
								case '<':
									var k = msg.b;
									return increaseZBy(-1);
								default:
									break _v0$41;
							}
						}
					} else {
						break _v0$41;
					}
				default:
					break _v0$41;
			}
		}
		var _v9 = $author$project$GraphDefs$selectedEdgeId(model.graph);
		if (_v9.$ === 'Nothing') {
			return $author$project$Model$noCmd(model);
		} else {
			var id = _v9.a;
			return $author$project$Model$noCmd(
				A2(
					$elm$core$Maybe$withDefault,
					model,
					A2(
						$elm$core$Maybe$map,
						function (style) {
							return A2(
								$author$project$Model$setSaveGraph,
								model,
								A3(
									$author$project$GraphDefs$updateNormalEdge,
									id,
									function (e) {
										return _Utils_update(
											e,
											{style: style});
									},
									model.graph));
						},
						A2(
							$elm$core$Maybe$andThen,
							function (e) {
								return A2($author$project$Msg$mayUpdateArrowStyle, msg, e.label.details.style);
							},
							A2(
								$elm$core$Maybe$andThen,
								$author$project$GraphDefs$filterEdgeNormal,
								A2($author$project$Polygraph$getEdge, id, model.graph))))));
		}
	});
var $author$project$Main$enlargeGraph = F2(
	function (m, state) {
		var _v0 = function () {
			var _v1 = state.pos;
			if (_v1.$ === 'InputPosKeyboard') {
				var p = _v1.a;
				return A2($author$project$InputPosition$deltaKeyboardPos, m.sizeGrid, p);
			} else {
				return A2($author$project$Geometry$Point$subtract, m.mousePos, state.orig);
			}
		}();
		var ox = _v0.a;
		var oy = _v0.b;
		var diags = A2($author$project$GraphDefs$getSurroundingDiagrams, state.orig, m.graph);
		var edgesId = A2(
			$elm$core$List$concatMap,
			A2($elm$core$Basics$composeR, $author$project$GraphProof$edgesOfDiag, $elm_community$intdict$IntDict$keys),
			diags);
		var gcon = A3(
			$author$project$Polygraph$connectedClosure,
			$elm$core$Basics$identity,
			$elm$core$Basics$identity,
			A4(
				$author$project$Polygraph$updateList,
				edgesId,
				$elm$core$Basics$always(true),
				$elm$core$Basics$always(true),
				A3(
					$author$project$Polygraph$map,
					F2(
						function (_v4, _v5) {
							return false;
						}),
					F2(
						function (_v6, _v7) {
							return false;
						}),
					m.graph)));
		var noSurround = !A3(
			$author$project$Polygraph$any,
			function ($) {
				return $.isIn;
			},
			function ($) {
				return $.isIn;
			},
			gcon);
		var _v2 = state.orig;
		var xi = _v2.a;
		var yi = _v2.b;
		var mkp = F4(
			function (id, n, i, o) {
				return ((_Utils_cmp(n, i) > -1) && (noSurround || A2(
					$elm$core$Maybe$withDefault,
					false,
					A4(
						$author$project$Polygraph$get,
						id,
						function ($) {
							return $.isIn;
						},
						function ($) {
							return $.isIn;
						},
						gcon)))) ? (n + o) : n;
			});
		var mapNode = F2(
			function (id, n) {
				var _v3 = n.pos;
				var nx = _v3.a;
				var ny = _v3.b;
				return _Utils_update(
					n,
					{
						pos: _Utils_Tuple2(
							A4(mkp, id, nx, xi, ox),
							A4(mkp, id, ny, yi, oy))
					});
			});
		var g = A3(
			$author$project$Polygraph$map,
			mapNode,
			$elm$core$Basics$always($elm$core$Basics$identity),
			m.graph);
		return g;
	});
var $author$project$Main$update_Enlarge = F3(
	function (msg, state, model) {
		var fin = $author$project$Model$switch_Default(
			A2(
				$author$project$Model$setSaveGraph,
				model,
				A2($author$project$Main$enlargeGraph, model, state)));
		_v0$3:
		while (true) {
			switch (msg.$) {
				case 'MouseUp':
					return fin;
				case 'KeyChanged':
					if ((!msg.a) && (msg.c.$ === 'Control')) {
						switch (msg.c.a) {
							case 'Escape':
								return $author$project$Model$switch_Default(model);
							case 'Enter':
								return fin;
							default:
								break _v0$3;
						}
					} else {
						break _v0$3;
					}
				default:
					break _v0$3;
			}
		}
		return $author$project$Model$noCmd(
			_Utils_update(
				model,
				{
					mode: $author$project$Modes$EnlargeMode(
						_Utils_update(
							state,
							{
								pos: A2($author$project$InputPosition$update, state.pos, msg)
							}))
				}));
	});
var $author$project$Polygraph$updateNodes = F2(
	function (l, g) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, g2) {
					var id = _v0.id;
					var label = _v0.label;
					return A3(
						$author$project$Polygraph$updateNode,
						id,
						$elm$core$Basics$always(label),
						g2);
				}),
			g,
			l);
	});
var $author$project$Main$info_MoveNode = F2(
	function (model, _v0) {
		var orig = _v0.orig;
		var pos = _v0.pos;
		var merge = model.specialKeys.ctrl;
		var nodes = $author$project$Polygraph$nodes(
			$author$project$GraphDefs$selectedGraph(model.graph));
		var updNode = F2(
			function (delta, _v7) {
				var id = _v7.id;
				var label = _v7.label;
				return {
					id: id,
					label: _Utils_update(
						label,
						{
							pos: A2($author$project$Geometry$Point$add, label.pos, delta)
						})
				};
			});
		var moveNodes = function (delta) {
			return A2(
				$elm$core$List$map,
				updNode(delta),
				nodes);
		};
		var mkRet = function (movedNodes) {
			var _v5 = A2($elm$core$Debug$log, 'selected', model.graph);
			var g = A2($author$project$Polygraph$updateNodes, movedNodes, model.graph);
			var _v6 = A2($elm$core$Debug$log, 'mkRet', g);
			return {graph: g, valid: !merge};
		};
		var retMerge = function (movedNodes) {
			if (movedNodes.b && (!movedNodes.b.b)) {
				var n = movedNodes.a;
				var _v4 = A2($author$project$GraphDefs$mergeWithSameLoc, n, model.graph);
				var g = _v4.a;
				var valid = _v4.b;
				return valid ? {graph: g, valid: true} : mkRet(movedNodes);
			} else {
				return mkRet(movedNodes);
			}
		};
		var retDelta = function (delta) {
			var movedNodes = moveNodes(delta);
			return merge ? retMerge(movedNodes) : mkRet(movedNodes);
		};
		var mouseDelta = A2(
			$author$project$Geometry$Point$subtract,
			model.mousePos,
			$author$project$GraphDefs$centerOfNodes(nodes));
		switch (pos.$) {
			case 'InputPosKeyboard':
				var p = pos.a;
				return retDelta(
					A2($author$project$InputPosition$deltaKeyboardPos, model.sizeGrid, p));
			case 'InputPosGraph':
				var id = pos.a;
				if (!merge) {
					return retDelta(mouseDelta);
				} else {
					if (nodes.b && (!nodes.b.b)) {
						var n = nodes.a;
						return {
							graph: A3($author$project$Polygraph$merge, id, n.id, model.graph),
							valid: true
						};
					} else {
						return retDelta(mouseDelta);
					}
				}
			default:
				return retDelta(mouseDelta);
		}
	});
var $author$project$Main$update_MoveNode = F3(
	function (msg, state, model) {
		var movedRet = function () {
			var info = A2($author$project$Main$info_MoveNode, model, state);
			return info.valid ? $author$project$Model$switch_Default(
				state.save ? A2($author$project$Model$setSaveGraph, model, info.graph) : _Utils_update(
					model,
					{graph: info.graph})) : $author$project$Model$noCmd(model);
		}();
		var updateState = function (st) {
			return _Utils_update(
				model,
				{
					mode: $author$project$Modes$Move(st)
				});
		};
		_v0$3:
		while (true) {
			switch (msg.$) {
				case 'MouseClick':
					return movedRet;
				case 'KeyChanged':
					if ((!msg.a) && (msg.c.$ === 'Control')) {
						switch (msg.c.a) {
							case 'Escape':
								return $author$project$Model$switch_Default(model);
							case 'Enter':
								return movedRet;
							default:
								break _v0$3;
						}
					} else {
						break _v0$3;
					}
				default:
					break _v0$3;
			}
		}
		return $author$project$Model$noCmd(
			updateState(
				_Utils_update(
					state,
					{
						pos: A2($author$project$InputPosition$update, state.pos, msg)
					})));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $elm$browser$Browser$Dom$blur = _Browser_call('blur');
var $elm$parser$Parser$deadEndsToString = function (deadEnds) {
	return 'TODO deadEndsToString';
};
var $author$project$QuickInput$HandSide = F2(
	function (start, edges) {
		return {edges: edges, start: start};
	});
var $author$project$QuickInput$Edge = F2(
	function (edge, to) {
		return {edge: edge, to: to};
	});
var $author$project$QuickInput$endSymbol = '->';
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var p = _v2.a;
					var x = _v2.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v2.a;
					var a = _v2.b;
					var s1 = _v2.c;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3(
							$elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var $elm$parser$Parser$Advanced$spaces = $elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return _Utils_eq(
			c,
			_Utils_chr(' ')) || (_Utils_eq(
			c,
			_Utils_chr('\n')) || _Utils_eq(
			c,
			_Utils_chr('\r')));
	});
var $elm$parser$Parser$spaces = $elm$parser$Parser$Advanced$spaces;
var $author$project$QuickInput$startSymbol = '--';
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $author$project$QuickInput$correctLabelChar = F2(
	function (fb, c) {
		return !A2($elm$core$List$member, c, fb);
	});
var $elm$core$String$trim = _String_trim;
var $elm$parser$Parser$ExpectingVariable = {$: 'ExpectingVariable'};
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$parser$Parser$Advanced$varHelp = F7(
	function (isGood, offset, row, col, src, indent, context) {
		varHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, src);
			if (_Utils_eq(newOffset, -1)) {
				return {col: col, context: context, indent: indent, offset: offset, row: row, src: src};
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$variable = function (i) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var firstOffset = A3($elm$parser$Parser$Advanced$isSubChar, i.start, s.offset, s.src);
			if (_Utils_eq(firstOffset, -1)) {
				return A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, i.expecting));
			} else {
				var s1 = _Utils_eq(firstOffset, -2) ? A7($elm$parser$Parser$Advanced$varHelp, i.inner, s.offset + 1, s.row + 1, 1, s.src, s.indent, s.context) : A7($elm$parser$Parser$Advanced$varHelp, i.inner, firstOffset, s.row, s.col + 1, s.src, s.indent, s.context);
				var name = A3($elm$core$String$slice, s.offset, s1.offset, s.src);
				return A2($elm$core$Set$member, name, i.reserved) ? A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, i.expecting)) : A3($elm$parser$Parser$Advanced$Good, true, name, s1);
			}
		});
};
var $elm$parser$Parser$variable = function (i) {
	return $elm$parser$Parser$Advanced$variable(
		{expecting: $elm$parser$Parser$ExpectingVariable, inner: i.inner, reserved: i.reserved, start: i.start});
};
var $author$project$QuickInput$labelParser = function (fb) {
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($elm$core$String$trim),
		$elm$parser$Parser$variable(
			{
				inner: $author$project$QuickInput$correctLabelChar(fb),
				reserved: $elm$core$Set$empty,
				start: $author$project$QuickInput$correctLabelChar(fb)
			}));
};
var $author$project$QuickInput$vertexParser = $author$project$QuickInput$labelParser(
	_List_fromArray(
		[
			_Utils_chr('-'),
			_Utils_chr('=')
		]));
var $author$project$QuickInput$edgeParser = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($author$project$QuickInput$Edge),
						$elm$parser$Parser$symbol($author$project$QuickInput$startSymbol)),
					$elm$parser$Parser$spaces),
				A2(
					$elm$parser$Parser$ignorer,
					A2($elm$parser$Parser$ignorer, $author$project$QuickInput$vertexParser, $elm$parser$Parser$spaces),
					$elm$parser$Parser$symbol($author$project$QuickInput$endSymbol))),
			$author$project$QuickInput$vertexParser)
		]));
var $elm$parser$Parser$Advanced$lazy = function (thunk) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v0 = thunk(_Utils_Tuple0);
			var parse = _v0.a;
			return parse(s);
		});
};
var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
var $author$project$ParserExtra$repeat = function (p) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed($elm$core$List$cons),
					p),
				$elm$parser$Parser$lazy(
					function (_v0) {
						return $author$project$ParserExtra$repeat(p);
					})),
				$elm$parser$Parser$succeed(_List_Nil)
			]));
};
var $author$project$QuickInput$handSideParser = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$QuickInput$HandSide),
		$author$project$QuickInput$vertexParser),
	$author$project$ParserExtra$repeat($author$project$QuickInput$edgeParser));
var $author$project$QuickInput$equalityParser = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($elm$core$Tuple$pair),
			$elm$parser$Parser$spaces),
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2($elm$parser$Parser$ignorer, $author$project$QuickInput$handSideParser, $elm$parser$Parser$spaces),
				$elm$parser$Parser$symbol('=')),
			$elm$parser$Parser$spaces)),
	$author$project$QuickInput$handSideParser);
var $author$project$GraphDefs$createNodeLabel = F3(
	function (g, s, p) {
		var label = A3($author$project$GraphDefs$newNodeLabel, p, s, true);
		var _v0 = A2($author$project$Polygraph$newNode, g, label);
		var g2 = _v0.a;
		var id = _v0.b;
		return _Utils_Tuple3(g2, id, p);
	});
var $author$project$QuickInput$buildGraphEdges = F7(
	function (g, offset, alignment, pos, from, to, ch) {
		buildGraphEdges:
		while (true) {
			var style = function () {
				var st = $author$project$ArrowStyle$empty;
				return _Utils_update(
					st,
					{labelAlignment: alignment});
			}();
			if (!ch.b) {
				return g;
			} else {
				if (!ch.b.b) {
					var e = ch.a;
					return A4(
						$author$project$Polygraph$newEdge,
						g,
						from,
						to,
						A2($author$project$GraphDefs$newEdgeLabel, e.edge, style)).a;
				} else {
					var e = ch.a;
					var tail = ch.b;
					var posf = A2($author$project$Geometry$Point$add, offset, pos);
					var _v1 = A3($author$project$GraphDefs$createNodeLabel, g, e.to, posf);
					var g2 = _v1.a;
					var idto = _v1.b;
					var _v2 = A4(
						$author$project$Polygraph$newEdge,
						g2,
						from,
						idto,
						A2($author$project$GraphDefs$newEdgeLabel, e.edge, style));
					var g3 = _v2.a;
					var $temp$g = g3,
						$temp$offset = offset,
						$temp$alignment = alignment,
						$temp$pos = posf,
						$temp$from = idto,
						$temp$to = to,
						$temp$ch = tail;
					g = $temp$g;
					offset = $temp$offset;
					alignment = $temp$alignment;
					pos = $temp$pos;
					from = $temp$from;
					to = $temp$to;
					ch = $temp$ch;
					continue buildGraphEdges;
				}
			}
		}
	});
var $author$project$QuickInput$buildGraphSegment = F2(
	function (s, g) {
		var offset = A2(
			$author$project$Geometry$Point$resize,
			1 / $elm$core$List$length(s.edges),
			A2($author$project$Geometry$Point$subtract, s.to, s.from));
		return A7(
			$author$project$QuickInput$buildGraphEdges,
			g,
			offset,
			s.alignLeft ? $author$project$ArrowStyle$Left : $author$project$ArrowStyle$Right,
			s.from,
			s.fromId,
			s.toId,
			s.edges);
	});
var $author$project$QuickInput$orientEquation = F4(
	function (iniP, _v0, offset, g) {
		var source = _v0.a;
		var but = _v0.b;
		var nsource = $elm$core$List$length(source.edges);
		var nbut = $elm$core$List$length(but.edges);
		var ni1 = ((nsource + 1) / 2) | 0;
		var nf1 = ((nbut + 1) / 2) | 0;
		var _v1 = A2($elm_community$list_extra$List$Extra$splitAt, ni1, source.edges);
		var source1 = _v1.a;
		var source2 = _v1.b;
		var _v2 = A2($elm_community$list_extra$List$Extra$splitAt, nf1, but.edges);
		var but1 = _v2.a;
		var but2 = _v2.b;
		var ni2 = nsource - ni1;
		var nf2 = nbut - nf1;
		var maxud = A2($elm$core$Basics$max, ni1, nf2);
		var maxlr = A2($elm$core$Basics$max, ni2, nf1);
		var _v3 = iniP;
		var leftX = _v3.a;
		var topY = _v3.b;
		var rightX = leftX + (maxud * offset);
		var bottomY = topY + (maxlr * offset);
		var topRightPos = _Utils_Tuple2(rightX, topY);
		var topLeftPos = _Utils_Tuple2(leftX, topY);
		var bottomRightPos = _Utils_Tuple2(rightX, bottomY);
		var bottomLeftPos = _Utils_Tuple2(leftX, bottomY);
		var startLabel = source.start;
		var lastLabel = A2(
			$elm$core$Basics$composeR,
			$elm_community$list_extra$List$Extra$last,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$map(
					function ($) {
						return $.to;
					}),
				$elm$core$Maybe$withDefault('')));
		var topRightLabel = lastLabel(source1);
		var endLabel = lastLabel(source.edges);
		var bottomLeftLabel = lastLabel(but1);
		var _v4 = A3($author$project$GraphDefs$createNodeLabel, g, startLabel, topLeftPos);
		var g2 = _v4.a;
		var topLeftId = _v4.b;
		var _v5 = A3($author$project$GraphDefs$createNodeLabel, g2, endLabel, bottomRightPos);
		var g3 = _v5.a;
		var bottomRightId = _v5.b;
		var _v6 = _Utils_eq(source2, _List_Nil) ? _Utils_Tuple3(g3, bottomRightId, iniP) : A3($author$project$GraphDefs$createNodeLabel, g3, topRightLabel, topRightPos);
		var g4 = _v6.a;
		var topRightId = _v6.b;
		var _v7 = _Utils_eq(but2, _List_Nil) ? _Utils_Tuple3(g4, bottomRightId, iniP) : A3($author$project$GraphDefs$createNodeLabel, g4, bottomLeftLabel, bottomLeftPos);
		var g5 = _v7.a;
		var bottomLeftId = _v7.b;
		return _Utils_Tuple2(
			g5,
			_List_fromArray(
				[
					{alignLeft: true, edges: source1, from: topLeftPos, fromId: topLeftId, to: topRightPos, toId: topRightId},
					{alignLeft: false, edges: but1, from: topLeftPos, fromId: topLeftId, to: bottomLeftPos, toId: bottomLeftId},
					{alignLeft: true, edges: source2, from: topRightPos, fromId: topRightId, to: bottomRightPos, toId: bottomRightId},
					{alignLeft: false, edges: but2, from: bottomLeftPos, fromId: bottomLeftId, to: bottomRightPos, toId: bottomRightId}
				]));
	});
var $author$project$QuickInput$graphEquation = F4(
	function (pos, offset, eq, gi) {
		var _v0 = A4($author$project$QuickInput$orientEquation, pos, eq, offset, gi);
		var gf = _v0.a;
		var l = _v0.b;
		return A3($elm$core$List$foldl, $author$project$QuickInput$buildGraphSegment, gf, l);
	});
var $author$project$Main$graphDrawingChain = F3(
	function (offset, g, eq) {
		var mid = offset / 2;
		var iniP = _Utils_Tuple2(mid, mid);
		return A4($author$project$QuickInput$graphEquation, iniP, offset, eq, g);
	});
var $author$project$QuickInput$splitWithChain = F3(
	function (g, ch, id) {
		return A2(
			$elm$core$Maybe$withDefault,
			g,
			A2(
				$elm$core$Maybe$map,
				function (edge) {
					var on2 = A2($author$project$Polygraph$getNode, edge.to, g);
					var on1 = A2($author$project$Polygraph$getNode, edge.from, g);
					var _v0 = _Utils_Tuple2(on1, on2);
					if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
						var n1 = _v0.a.a;
						var n2 = _v0.b.a;
						return A2(
							$author$project$QuickInput$buildGraphSegment,
							{alignLeft: true, edges: ch.edges, from: n1.pos, fromId: edge.from, to: n2.pos, toId: edge.to},
							A2($author$project$Polygraph$removeEdge, id, g));
					} else {
						return g;
					}
				},
				A2($author$project$Polygraph$getEdge, id, g)));
	});
var $author$project$Main$graphQuickInput = F2(
	function (model, ch) {
		if (ch.$ === 'Nothing') {
			return model.graph;
		} else {
			var _v1 = ch.a;
			var eq1 = _v1.a;
			var eq2 = _v1.b;
			var od = A2(
				$elm$core$Debug$log,
				'selected subdiag',
				$author$project$GraphDefs$selectedIncompleteDiagram(model.graph));
			var _default = A3(
				$author$project$Main$graphDrawingChain,
				model.sizeGrid,
				model.graph,
				_Utils_Tuple2(eq1, eq2));
			var split = F2(
				function (l, edges) {
					return A2(
						$elm$core$Maybe$map,
						A2($author$project$QuickInput$splitWithChain, model.graph, edges),
						$author$project$GraphProof$isEmptyBranch(l));
				});
			if (od.$ === 'Nothing') {
				return _default;
			} else {
				var d = od.a;
				return A2(
					$elm$core$Maybe$withDefault,
					_default,
					A2(
						$elm_community$maybe_extra$Maybe$Extra$or,
						A2(split, d.lhs, eq2),
						A2(split, d.rhs, eq2)));
			}
		}
	});
var $author$project$HtmlDefs$quickInputId = 'quickinput';
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (_v0.$ === 'Ok') {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$Msg$unfocusId = function (s) {
	return A2(
		$elm$core$Task$attempt,
		function (_v0) {
			return $author$project$Msg$noOp;
		},
		$elm$browser$Browser$Dom$blur(s));
};
var $author$project$Main$update_QuickInput = F3(
	function (ch, msg, model) {
		var finalRet = function (chain) {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						graph: A2($author$project$Main$graphQuickInput, model, chain),
						mode: $author$project$Modes$DefaultMode,
						quickInput: ''
					}),
				$elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							$author$project$HtmlDefs$computeLayout(_Utils_Tuple0),
							$author$project$Msg$unfocusId($author$project$HtmlDefs$quickInputId)
						])));
		};
		_v0$3:
		while (true) {
			switch (msg.$) {
				case 'KeyChanged':
					if ((!msg.a) && (msg.c.$ === 'Control')) {
						switch (msg.c.a) {
							case 'Escape':
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{mode: $author$project$Modes$DefaultMode}),
									A2(
										$elm$core$Task$attempt,
										function (_v1) {
											return $author$project$Msg$noOp;
										},
										$elm$browser$Browser$Dom$blur($author$project$HtmlDefs$quickInputId)));
							case 'Enter':
								return finalRet(ch);
							default:
								break _v0$3;
						}
					} else {
						break _v0$3;
					}
				case 'QuickInput':
					var _final = msg.a;
					var s = msg.b;
					var _v2 = function () {
						var _v3 = A2($elm$parser$Parser$run, $author$project$QuickInput$equalityParser, s);
						if (_v3.$ === 'Ok') {
							var l = _v3.a;
							return _Utils_Tuple2(
								$elm$core$Debug$toString(l),
								$elm$core$Maybe$Just(l));
						} else {
							var e = _v3.a;
							return _Utils_Tuple2(
								$elm$parser$Parser$deadEndsToString(e),
								$elm$core$Maybe$Nothing);
						}
					}();
					var statusMsg = _v2.a;
					var chain = _v2.b;
					return _final ? finalRet(chain) : $author$project$Model$noCmd(
						_Utils_update(
							model,
							{
								mode: $author$project$Modes$QuickInputMode(chain),
								statusMsg: statusMsg
							}));
				default:
					break _v0$3;
			}
		}
		return $author$project$Model$noCmd(model);
	});
var $author$project$Geometry$makeRect = F2(
	function (p1, p2) {
		return $author$project$Geometry$rectEnveloppe(
			_List_fromArray(
				[p1, p2]));
	});
var $author$project$Main$selectGraph = F3(
	function (m, orig, keep) {
		var selRect = A2($author$project$Geometry$makeRect, orig, m.mousePos);
		var g = keep ? m.graph : $author$project$GraphDefs$clearSelection(m.graph);
		var isSel = function (n) {
			return A2($author$project$Geometry$isInRect, selRect, n.pos);
		};
		return A2($author$project$GraphDefs$addNodesSelection, g, isSel);
	});
var $author$project$Main$update_RectSelect = F4(
	function (msg, orig, keep, model) {
		_v0$2:
		while (true) {
			switch (msg.$) {
				case 'KeyChanged':
					if (((!msg.a) && (msg.c.$ === 'Control')) && (msg.c.a === 'Escape')) {
						return $author$project$Model$switch_Default(model);
					} else {
						break _v0$2;
					}
				case 'MouseClick':
					return _Utils_eq(model.mousePos, orig) ? $author$project$Model$switch_Default(
						$author$project$Main$selectByClick(model)) : $author$project$Model$switch_Default(
						_Utils_update(
							model,
							{
								graph: A3($author$project$Main$selectGraph, model, orig, keep)
							}));
				default:
					break _v0$2;
			}
		}
		return $author$project$Model$noCmd(model);
	});
var $author$project$Main$graph_RenameMode = F2(
	function (l, m) {
		if (!l.b) {
			return m.graph;
		} else {
			var _v1 = l.a;
			var id = _v1.a;
			var s = _v1.b;
			return A4(
				$author$project$Polygraph$update,
				id,
				function (n) {
					return _Utils_update(
						n,
						{label: s});
				},
				$author$project$GraphDefs$mapNormalEdge(
					function (e) {
						return _Utils_update(
							e,
							{label: s});
					}),
				m.graph);
		}
	});
var $author$project$Main$next_RenameMode = F4(
	function (finish, save, labels, model) {
		var g = A2($author$project$Main$graph_RenameMode, labels, model);
		var m2 = save ? A2($author$project$Model$setSaveGraph, model, g) : _Utils_update(
			model,
			{graph: g});
		if (finish) {
			return _Utils_update(
				m2,
				{mode: $author$project$Modes$DefaultMode});
		} else {
			if (!labels.b) {
				return _Utils_update(
					m2,
					{mode: $author$project$Modes$DefaultMode});
			} else {
				if (!labels.b.b) {
					return _Utils_update(
						m2,
						{mode: $author$project$Modes$DefaultMode});
				} else {
					var q = labels.b;
					return _Utils_update(
						m2,
						{
							mode: A2($author$project$Modes$RenameMode, save, q)
						});
				}
			}
		}
	});
var $author$project$Main$update_RenameMode = F4(
	function (save, labels, msg, model) {
		var edit_label = function (s) {
			return $author$project$Model$noCmd(
				_Utils_update(
					model,
					{
						mode: A2(
							$author$project$Modes$RenameMode,
							save,
							function () {
								if (labels.b) {
									var _v2 = labels.a;
									var id = _v2.a;
									var q = labels.b;
									return A2(
										$elm$core$List$cons,
										_Utils_Tuple2(id, s),
										q);
								} else {
									return labels;
								}
							}())
					}));
		};
		_v0$5:
		while (true) {
			switch (msg.$) {
				case 'KeyChanged':
					if ((!msg.a) && (msg.c.$ === 'Control')) {
						switch (msg.c.a) {
							case 'Escape':
								return $author$project$Model$switch_Default(model);
							case 'Enter':
								return $author$project$Model$noCmd(
									A4($author$project$Main$next_RenameMode, true, save, labels, model));
							case 'Tab':
								return $author$project$Model$noCmd(
									A4($author$project$Main$next_RenameMode, false, save, labels, model));
							default:
								break _v0$5;
						}
					} else {
						break _v0$5;
					}
				case 'NodeLabelEdit':
					var s = msg.b;
					return edit_label(s);
				case 'EdgeLabelEdit':
					var s = msg.b;
					return edit_label(s);
				default:
					break _v0$5;
			}
		}
		return $author$project$Model$noCmd(model);
	});
var $author$project$Main$graphResize = F2(
	function (st, m) {
		if (st.onlyGrid) {
			return m.graph;
		} else {
			var ratio = st.sizeGrid / m.sizeGrid;
			return A3(
				$author$project$Polygraph$map,
				F2(
					function (_v0, n) {
						return _Utils_update(
							n,
							{
								pos: A2($author$project$Geometry$Point$resize, ratio, n.pos)
							});
					}),
				function (_v1) {
					return $elm$core$Basics$identity;
				},
				m.graph);
		}
	});
var $author$project$Model$maxSizeGrid = 500;
var $author$project$Model$minSizeGrid = 2;
var $author$project$Main$update_Resize = F3(
	function (st, msg, m) {
		var finalise = function (_v1) {
			var m2 = $author$project$Model$pushHistory(m);
			return _Utils_Tuple2(
				_Utils_update(
					m2,
					{
						graph: A2($author$project$Main$graphResize, st, m),
						mode: $author$project$Modes$DefaultMode,
						sizeGrid: st.sizeGrid
					}),
				$elm$core$Platform$Cmd$none);
		};
		var increment = 10;
		var newState = function (st2) {
			return $author$project$Model$noCmd(
				_Utils_update(
					m,
					{
						mode: $author$project$Modes$ResizeMode(st2)
					}));
		};
		var newSize = function (si) {
			var s = A2(
				$elm$core$Basics$max,
				$author$project$Model$minSizeGrid,
				A2($elm$core$Basics$min, $author$project$Model$maxSizeGrid, si));
			return newState(
				_Utils_update(
					st,
					{sizeGrid: s}));
		};
		_v0$6:
		while (true) {
			switch (msg.$) {
				case 'KeyChanged':
					if (!msg.a) {
						if (msg.c.$ === 'Control') {
							switch (msg.c.a) {
								case 'Escape':
									return _Utils_Tuple2(
										_Utils_update(
											m,
											{mode: $author$project$Modes$DefaultMode}),
										$elm$core$Platform$Cmd$none);
								case 'Enter':
									return finalise(_Utils_Tuple0);
								default:
									break _v0$6;
							}
						} else {
							switch (msg.c.a.valueOf()) {
								case 'k':
									return newSize(st.sizeGrid + increment);
								case 'j':
									return newSize(st.sizeGrid - increment);
								case 'g':
									return newState(
										_Utils_update(
											st,
											{onlyGrid: !st.onlyGrid}));
								default:
									break _v0$6;
							}
						}
					} else {
						break _v0$6;
					}
				case 'SizeGrid':
					var s = msg.a;
					return newSize(s);
				default:
					break _v0$6;
			}
		}
		return $author$project$Model$noCmd(m);
	});
var $author$project$Main$update = F2(
	function (msg, modeli) {
		var model = function () {
			switch (msg.$) {
				case 'FileName':
					var s = msg.a;
					return _Utils_update(
						modeli,
						{fileName: s});
				case 'KeyChanged':
					var r = msg.b;
					return _Utils_update(
						modeli,
						{specialKeys: r});
				case 'MouseMoveRaw':
					var keys = msg.b;
					return _Utils_update(
						modeli,
						{mouseOnCanvas: true, specialKeys: keys});
				case 'MouseMove':
					var p = msg.a;
					return _Utils_update(
						modeli,
						{mousePos: p});
				case 'MouseDown':
					var e = msg.a;
					return _Utils_update(
						modeli,
						{specialKeys: e.keys});
				case 'MouseLeaveCanvas':
					var _v7 = A2($elm$core$Debug$log, 'mouseleave', _Utils_Tuple0);
					return _Utils_update(
						modeli,
						{mouseOnCanvas: false});
				case 'QuickInput':
					var _final = msg.a;
					var s = msg.b;
					var _v8 = A2($elm$core$Debug$log, 'coucou1!', _Utils_Tuple0);
					return _Utils_update(
						modeli,
						{
							mode: $author$project$Modes$QuickInputMode($elm$core$Maybe$Nothing),
							quickInput: s
						});
				case 'LatexPreambleEdit':
					var s = msg.a;
					return _Utils_update(
						modeli,
						{latexPreamble: s});
				default:
					return modeli;
			}
		}();
		switch (msg.$) {
			case 'Save':
				return _Utils_Tuple2(
					model,
					$author$project$Main$saveGraph(
						$author$project$Main$toJsGraphInfo(model)));
			case 'MinuteTick':
				return model.autoSave ? _Utils_Tuple2(
					model,
					$author$project$Main$quicksaveGraph(
						{
							feedback: false,
							info: $author$project$Main$toJsGraphInfo(model)
						})) : $author$project$Model$noCmd(model);
			case 'Clear':
				return $author$project$Model$noCmd($author$project$Model$iniModel);
			case 'ToggleHideGrid':
				return $author$project$Model$noCmd(
					_Utils_update(
						model,
						{hideGrid: !model.hideGrid}));
			case 'ToggleAutosave':
				return $author$project$Model$noCmd(
					_Utils_update(
						model,
						{autoSave: !model.autoSave}));
			case 'ExportQuiver':
				return _Utils_Tuple2(
					model,
					$author$project$Main$exportQuiver(
						A2(
							$author$project$GraphDefs$exportQuiver,
							model.sizeGrid,
							$author$project$GraphDefs$selectedGraph(model.graph))));
			case 'MouseMoveRaw':
				var v = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Main$onMouseMove(v));
			case 'NodeRendered':
				var n = msg.a;
				var _v1 = msg.b;
				var x = _v1.a;
				var y = _v1.b;
				var dims = _Utils_Tuple2(
					x,
					(!y) ? 12 : y);
				return $author$project$Model$noCmd(
					_Utils_update(
						model,
						{
							graph: A3(
								$author$project$Polygraph$updateNode,
								n,
								function (l) {
									return _Utils_update(
										l,
										{
											dims: $elm$core$Maybe$Just(dims)
										});
								},
								model.graph)
						}));
			case 'EdgeRendered':
				var e = msg.a;
				var _v2 = msg.b;
				var x = _v2.a;
				var y = _v2.b;
				var dims = _Utils_Tuple2(
					x,
					(!y) ? 12 : y);
				return $author$project$Model$noCmd(
					_Utils_update(
						model,
						{
							graph: A3(
								$author$project$GraphDefs$updateNormalEdge,
								e,
								function (l) {
									return _Utils_update(
										l,
										{
											dims: $elm$core$Maybe$Just(dims)
										});
								},
								model.graph)
						}));
			case 'Do':
				var cmd = msg.a;
				return _Utils_Tuple2(model, cmd);
			case 'Loaded':
				var g = msg.a;
				var _v3 = A2($elm$core$Debug$log, 'scenario', g.scenario);
				var scenario = $author$project$Msg$scenarioOfString(g.scenario);
				var m = $author$project$Model$clearHistory(
					A2(
						$author$project$Model$updateWithGraphInfo,
						_Utils_update(
							model,
							{fileName: g.fileName, mode: $author$project$Modes$DefaultMode, scenario: scenario}),
						g.graph));
				var graph = _Utils_eq(scenario, $author$project$Msg$Exercise1) ? A3(
					$author$project$Polygraph$map,
					F2(
						function (_v4, n) {
							return _Utils_update(
								n,
								{
									selected: A2($elm$core$String$contains, '\\bullet', n.label)
								});
						}),
					$elm$core$Basics$always($elm$core$Basics$identity),
					m.graph) : m.graph;
				return _Utils_Tuple2(
					_Utils_update(
						m,
						{graph: graph}),
					$author$project$HtmlDefs$computeLayout(_Utils_Tuple0));
			case 'FindReplace':
				var req = msg.a;
				return $author$project$Model$noCmd(
					A2(
						$author$project$Model$setSaveGraph,
						model,
						A2($author$project$GraphDefs$findReplaceInSelected, model.graph, req)));
			default:
				var _v5 = model.mode;
				switch (_v5.$) {
					case 'QuickInputMode':
						var c = _v5.a;
						return A3($author$project$Main$update_QuickInput, c, msg, model);
					case 'DefaultMode':
						return A2($author$project$Main$update_DefaultMode, msg, model);
					case 'RectSelect':
						var orig = _v5.a;
						return A4($author$project$Main$update_RectSelect, msg, orig, model.specialKeys.shift, model);
					case 'EnlargeMode':
						var state = _v5.a;
						return A3($author$project$Main$update_Enlarge, msg, state, model);
					case 'NewArrow':
						var astate = _v5.a;
						return A3($author$project$Modes$NewArrow$update, astate, msg, model);
					case 'PullshoutMode':
						var astate = _v5.a;
						return A3($author$project$Modes$Pullshout$update, astate, msg, model);
					case 'RenameMode':
						var b = _v5.a;
						var l = _v5.b;
						return A4($author$project$Main$update_RenameMode, b, l, msg, model);
					case 'Move':
						var s = _v5.a;
						return A3($author$project$Main$update_MoveNode, msg, s, model);
					case 'DebugMode':
						return A2($author$project$Main$update_DebugMode, msg, model);
					case 'SquareMode':
						var state = _v5.a;
						return A3($author$project$Modes$Square$update, state, msg, model);
					case 'SplitArrow':
						var state = _v5.a;
						return A3($author$project$Modes$SplitArrow$update, state, msg, model);
					case 'CutHead':
						var state = _v5.a;
						return A3($author$project$Main$update_CutHead, state, msg, model);
					case 'CloneMode':
						return A2($author$project$Main$update_Clone, msg, model);
					default:
						var s = _v5.a;
						return A3($author$project$Main$update_Resize, s, msg, model);
				}
		}
	});
var $author$project$Main$updateIntercept = F2(
	function (msg, modeli) {
		var _v0 = modeli.scenario;
		if (_v0.$ === 'Exercise1') {
			var nothing = $author$project$Model$noCmd(modeli);
			switch (msg.$) {
				case 'MouseMove':
					return nothing;
				case 'MouseDown':
					return nothing;
				case 'NodeClick':
					return nothing;
				case 'EdgeClick':
					return nothing;
				case 'EltDoubleClick':
					return nothing;
				case 'MouseOn':
					return nothing;
				case 'MouseClick':
					return nothing;
				default:
					return A2($author$project$Main$update, msg, modeli);
			}
		} else {
			return A2($author$project$Main$update, msg, modeli);
		}
	});
var $author$project$Msg$Clear = {$: 'Clear'};
var $author$project$Msg$ExportQuiver = {$: 'ExportQuiver'};
var $author$project$Msg$LatexPreambleEdit = function (a) {
	return {$: 'LatexPreambleEdit', a: a};
};
var $author$project$Msg$MouseDown = function (a) {
	return {$: 'MouseDown', a: a};
};
var $author$project$Msg$MouseLeaveCanvas = {$: 'MouseLeaveCanvas'};
var $author$project$Msg$MouseMoveRaw = F2(
	function (a, b) {
		return {$: 'MouseMoveRaw', a: a, b: b};
	});
var $author$project$Msg$MouseUp = {$: 'MouseUp'};
var $author$project$Msg$Save = {$: 'Save'};
var $author$project$Msg$SizeGrid = function (a) {
	return {$: 'SizeGrid', a: a};
};
var $author$project$Msg$ToggleAutosave = {$: 'ToggleAutosave'};
var $author$project$Msg$ToggleHideGrid = {$: 'ToggleHideGrid'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $author$project$Drawing$Drawing = function (a) {
	return {$: 'Drawing', a: a};
};
var $author$project$Drawing$empty = $author$project$Drawing$Drawing(_List_Nil);
var $author$project$GraphDrawing$foregroundZ = 10000;
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $author$project$Drawing$ofSvgs = F2(
	function (z, l) {
		return $author$project$Drawing$Drawing(
			A2(
				$elm$core$List$map,
				function (s) {
					return {svg: s, zindex: z};
				},
				l));
	});
var $author$project$Drawing$ofSvg = F2(
	function (z, s) {
		return A2(
			$author$project$Drawing$ofSvgs,
			z,
			_List_fromArray(
				[s]));
	});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Drawing$rect = F2(
	function (z, _v0) {
		var topLeft = _v0.topLeft;
		var bottomRight = _v0.bottomRight;
		var f = $elm$core$String$fromFloat;
		var _v1 = bottomRight;
		var tox = _v1.a;
		var toy = _v1.b;
		var _v2 = topLeft;
		var fromx = _v2.a;
		var fromy = _v2.b;
		return A2(
			$author$project$Drawing$ofSvg,
			z,
			A2(
				$elm$svg$Svg$rect,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x(
						f(fromx)),
						$elm$svg$Svg$Attributes$y(
						f(fromy)),
						$elm$svg$Svg$Attributes$width(
						f(tox - fromx)),
						$elm$svg$Svg$Attributes$height(
						f(toy - fromy)),
						$elm$svg$Svg$Attributes$class('rect-select')
					]),
				_List_Nil));
	});
var $author$project$Main$additionnalDrawing = function (m) {
	var drawSel = F2(
		function (pos, orig) {
			return A2(
				$author$project$Drawing$rect,
				$author$project$GraphDrawing$foregroundZ,
				A2(
					$author$project$Geometry$makeRect,
					orig,
					A2(
						$author$project$Geometry$Point$add,
						_Utils_Tuple2(1, 1),
						function () {
							if (pos.$ === 'InputPosKeyboard') {
								var p = pos.a;
								return A2(
									$author$project$Geometry$Point$add,
									orig,
									A2($author$project$InputPosition$deltaKeyboardPos, m.sizeGrid, p));
							} else {
								return m.mousePos;
							}
						}())));
		});
	var _v0 = m.mode;
	switch (_v0.$) {
		case 'RectSelect':
			var orig = _v0.a;
			return A2(drawSel, $author$project$InputPosition$InputPosMouse, orig);
		case 'EnlargeMode':
			var state = _v0.a;
			return A2(drawSel, state.pos, state.orig);
		default:
			return $author$project$Drawing$empty;
	}
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $author$project$HtmlDefs$canvasId = 'canvas';
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$HtmlDefs$checkbox = F4(
	function (msg, name, tooltip, checked) {
		return A2(
			$elm$html$Html$label,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$title(tooltip)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('checkbox'),
							$elm$html$Html$Events$onClick(msg),
							$elm$html$Html$Attributes$checked(checked),
							$elm$html$Html$Attributes$title(tooltip)
						]),
					_List_Nil),
					$elm$html$Html$text(name)
				]));
	});
var $elm$html$Html$Attributes$cols = function (n) {
	return A2(
		_VirtualDom_attribute,
		'cols',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$svg$Svg$foreignObject = $elm$svg$Svg$trustedNode('foreignObject');
var $author$project$Drawing$emptyForeign = A2(
	$author$project$Drawing$ofSvg,
	-10000,
	A2(
		$elm$svg$Svg$foreignObject,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$x('1'),
				$elm$svg$Svg$Attributes$y('1'),
				$elm$svg$Svg$Attributes$width('100%'),
				$elm$svg$Svg$Attributes$height('100%')
			]),
		_List_Nil));
var $author$project$Msg$EdgeClick = F2(
	function (a, b) {
		return {$: 'EdgeClick', a: a, b: b};
	});
var $author$project$Drawing$Black = {$: 'Black'};
var $author$project$Drawing$black = $author$project$Drawing$Black;
var $author$project$Drawing$Blue = {$: 'Blue'};
var $author$project$Drawing$blue = $author$project$Drawing$Blue;
var $author$project$Drawing$Red = {$: 'Red'};
var $author$project$Drawing$red = $author$project$Drawing$Red;
var $author$project$GraphDrawing$activityToColor = function (a) {
	switch (a.$) {
		case 'MainActive':
			return $author$project$Drawing$red;
		case 'WeakActive':
			return $author$project$Drawing$blue;
		default:
			return $author$project$Drawing$black;
	}
};
var $author$project$Drawing$Color = function (a) {
	return {$: 'Color', a: a};
};
var $author$project$Drawing$color = $author$project$Drawing$Color;
var $author$project$Drawing$drawingToZSvgs = function (_v0) {
	var c = _v0.a;
	return c;
};
var $author$project$Drawing$group = function (l) {
	return $author$project$Drawing$Drawing(
		$elm$core$List$concat(
			A2($elm$core$List$map, $author$project$Drawing$drawingToZSvgs, l)));
};
var $author$project$Drawing$attributeToZIndex = function (a) {
	if (a.$ === 'ZIndex') {
		var n = a.a;
		return $elm$core$Maybe$Just(n);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Drawing$defaultZ = 0;
var $author$project$Drawing$attributesToZIndex = A2(
	$elm$core$Basics$composeR,
	$elm_community$list_extra$List$Extra$findMap($author$project$Drawing$attributeToZIndex),
	$elm$core$Maybe$withDefault($author$project$Drawing$defaultZ));
var $author$project$Drawing$colorToString = function (c) {
	switch (c.$) {
		case 'Black':
			return 'black';
		case 'Red':
			return 'red';
		default:
			return 'blue';
	}
};
var $elm$svg$Svg$Events$on = $elm$html$Html$Events$on;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions = {preventDefault: true, stopPropagation: false};
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event = F6(
	function (keys, button, clientPos, offsetPos, pagePos, screenPos) {
		return {button: button, clientPos: clientPos, keys: keys, offsetPos: offsetPos, pagePos: pagePos, screenPos: screenPos};
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton = {$: 'BackButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton = {$: 'ErrorButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton = {$: 'ForwardButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton = {$: 'MainButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton = {$: 'MiddleButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton = {$: 'SecondButton'};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId = function (id) {
	switch (id) {
		case 0:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton;
		case 1:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton;
		case 2:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton;
		case 3:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton;
		case 4:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton;
		default:
			return $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton;
	}
};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder = A2(
	$elm$json$Json$Decode$map,
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId,
	A2($elm$json$Json$Decode$field, 'button', $elm$json$Json$Decode$int));
var $mpizenberg$elm_pointer_events$Internal$Decode$clientPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$Keys = F3(
	function (alt, ctrl, shift) {
		return {alt: alt, ctrl: ctrl, shift: shift};
	});
var $mpizenberg$elm_pointer_events$Internal$Decode$keys = A4(
	$elm$json$Json$Decode$map3,
	$mpizenberg$elm_pointer_events$Internal$Decode$Keys,
	A2($elm$json$Json$Decode$field, 'altKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'ctrlKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool));
var $elm$json$Json$Decode$map6 = _Json_map6;
var $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$pagePos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$screenPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'screenX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'screenY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder = A7($elm$json$Json$Decode$map6, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event, $mpizenberg$elm_pointer_events$Internal$Decode$keys, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder, $mpizenberg$elm_pointer_events$Internal$Decode$clientPos, $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos, $mpizenberg$elm_pointer_events$Internal$Decode$pagePos, $mpizenberg$elm_pointer_events$Internal$Decode$screenPos);
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions = F3(
	function (event, options, tag) {
		return A2(
			$elm$html$Html$Events$custom,
			event,
			A2(
				$elm$json$Json$Decode$map,
				function (ev) {
					return {
						message: tag(ev),
						preventDefault: options.preventDefault,
						stopPropagation: options.stopPropagation
					};
				},
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder));
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onClick = A2($mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'click', $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDoubleClick = A2($mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'dblclick', $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var $author$project$Drawing$attrToSvgAttr = F2(
	function (col, a) {
		switch (a.$) {
			case 'Color':
				var c = a.a;
				return $elm$core$Maybe$Just(
					col(
						$author$project$Drawing$colorToString(c)));
			case 'Class':
				var s = a.a;
				return $elm$core$Maybe$Just(
					$elm$svg$Svg$Attributes$class(s));
			case 'On':
				var e = a.a;
				var d = a.b;
				return $elm$core$Maybe$Just(
					A2($elm$svg$Svg$Events$on, e, d));
			case 'OnClick':
				var f = a.a;
				return $elm$core$Maybe$Just(
					$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onClick(f));
			case 'OnDoubleClick':
				var f = a.a;
				return $elm$core$Maybe$Just(
					$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDoubleClick(f));
			default:
				return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Drawing$attrsToSvgAttrs = function (f) {
	return $elm$core$List$filterMap(
		$author$project$Drawing$attrToSvgAttr(f));
};
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $author$project$Drawing$line = F3(
	function (l, _v0, _v1) {
		var fromx = _v0.a;
		var fromy = _v0.b;
		var tox = _v1.a;
		var toy = _v1.b;
		var z = $author$project$Drawing$attributesToZIndex(l);
		var f = $elm$core$String$fromFloat;
		return A2(
			$author$project$Drawing$ofSvg,
			z,
			A2(
				$elm$svg$Svg$line,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							f(fromx)),
							$elm$svg$Svg$Attributes$y1(
							f(fromy)),
							$elm$svg$Svg$Attributes$x2(
							f(tox)),
							$elm$svg$Svg$Attributes$y2(
							f(toy))
						]),
					A2($author$project$Drawing$attrsToSvgAttrs, $elm$svg$Svg$Attributes$stroke, l)),
				_List_Nil));
	});
var $author$project$Drawing$OnClick = function (a) {
	return {$: 'OnClick', a: a};
};
var $author$project$Drawing$onClick = $author$project$Drawing$OnClick;
var $author$project$Geometry$Point$normalise = F2(
	function (len, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		var r = $author$project$Geometry$Point$radius(
			_Utils_Tuple2(x, y));
		return _Utils_Tuple2((len * x) / r, (len * y) / r);
	});
var $author$project$Geometry$Point$towards = F3(
	function (source, to, shift) {
		return A2(
			$author$project$Geometry$Point$add,
			source,
			A2(
				$author$project$Geometry$Point$normalise,
				shift,
				A2($author$project$Geometry$Point$subtract, to, source)));
	});
var $author$project$GraphDrawing$drawPullshout = F4(
	function (edgeId, a, _v0, _v1) {
		var x1 = _v0.a;
		var x2 = _v0.b;
		var y1 = _v1.a;
		var y2 = _v1.b;
		var shift = 30;
		var _v2 = _Utils_eq(x1, y1) ? _Utils_Tuple2(y1, y2) : _Utils_Tuple2(y2, y1);
		var q1 = _v2.a;
		var q2 = _v2.b;
		var _v3 = _Utils_eq(x1, y1) ? _Utils_Tuple2(x1, x2) : _Utils_Tuple2(x2, x1);
		var p1 = _v3.a;
		var p2 = _v3.b;
		var smallshift = 5;
		var r2 = A3($author$project$Geometry$Point$towards, q1, q2, shift);
		var r1 = A3($author$project$Geometry$Point$towards, p1, p2, shift);
		var extrem = A3($author$project$Geometry$Point$diamondPave, r1, p1, r2);
		var s2 = A3($author$project$Geometry$Point$towards, r2, extrem, smallshift);
		var s1 = A3($author$project$Geometry$Point$towards, r1, extrem, smallshift);
		var blackline = $author$project$Drawing$line(
			_List_fromArray(
				[
					$author$project$Drawing$color(
					$author$project$GraphDrawing$activityToColor(a)),
					$author$project$Drawing$onClick(
					$author$project$Msg$EdgeClick(edgeId))
				]));
		return $author$project$Drawing$group(
			_List_fromArray(
				[
					A2(blackline, s1, extrem),
					A2(blackline, extrem, s2)
				]));
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $author$project$Geometry$QuadraticBezier$middle = function (_v0) {
	var from = _v0.from;
	var to = _v0.to;
	var controlPoint = _v0.controlPoint;
	return A2(
		$author$project$Geometry$Point$middle,
		controlPoint,
		A2($author$project$Geometry$Point$middle, from, to));
};
var $author$project$Msg$NodeClick = F2(
	function (a, b) {
		return {$: 'NodeClick', a: a, b: b};
	});
var $author$project$Msg$EltDoubleClick = F2(
	function (a, b) {
		return {$: 'EltDoubleClick', a: a, b: b};
	});
var $author$project$Msg$NodeLabelEdit = F2(
	function (a, b) {
		return {$: 'NodeLabelEdit', a: a, b: b};
	});
var $author$project$Msg$NodeRendered = F2(
	function (a, b) {
		return {$: 'NodeRendered', a: a, b: b};
	});
var $author$project$GraphDrawing$activityToClasses = function (a) {
	switch (a.$) {
		case 'MainActive':
			return _List_fromArray(
				['active-label']);
		case 'WeakActive':
			return _List_fromArray(
				['weak-active-label']);
		default:
			return _List_Nil;
	}
};
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $author$project$Drawing$circle = F3(
	function (attrs, _v0, n) {
		var cx = _v0.a;
		var cy = _v0.b;
		var z = $author$project$Drawing$attributesToZIndex(attrs);
		var f = $elm$core$String$fromFloat;
		return A2(
			$author$project$Drawing$ofSvg,
			z,
			A2(
				$elm$svg$Svg$circle,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$cx(
							f(cx)),
							$elm$svg$Svg$Attributes$cy(
							f(cy)),
							$elm$svg$Svg$Attributes$r(
							f(n))
						]),
					A2($author$project$Drawing$attrsToSvgAttrs, $elm$svg$Svg$Attributes$fill, attrs)),
				_List_Nil));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Drawing$htmlAnchor = F5(
	function (z, _v0, _v1, center, h) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var width = _v1.a;
		var height = _v1.b;
		var f = $elm$core$String$fromFloat;
		var _v2 = center ? _Utils_Tuple2(x1 - (width / 2), y1 - (height / 2)) : _Utils_Tuple2(x1, y1);
		var x = _v2.a;
		var y = _v2.b;
		return A2(
			$author$project$Drawing$ofSvg,
			z,
			A2(
				$elm$svg$Svg$foreignObject,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x(
						f(x)),
						$elm$svg$Svg$Attributes$y(
						f(y)),
						$elm$svg$Svg$Attributes$width('100%'),
						$elm$svg$Svg$Attributes$height(
						f(height))
					]),
				_List_fromArray(
					[h])));
	});
var $author$project$HtmlDefs$latexElement = 'math-latex';
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $author$project$HtmlDefs$makeLatex = F2(
	function (attrs, s) {
		return A3(
			$elm$html$Html$node,
			$author$project$HtmlDefs$latexElement,
			attrs,
			_List_fromArray(
				[
					$elm$html$Html$text(s)
				]));
	});
var $elm$html$Html$Attributes$autofocus = $elm$html$Html$Attributes$boolProperty('autofocus');
var $elm$browser$Browser$Dom$focus = _Browser_call('focus');
var $author$project$Msg$focusId = function (s) {
	return A2(
		$elm$core$Task$attempt,
		function (_v0) {
			return $author$project$Msg$noOp;
		},
		$elm$browser$Browser$Dom$focus(s));
};
var $author$project$Drawing$html = F4(
	function (z, p, d, h) {
		return A5($author$project$Drawing$htmlAnchor, z, p, d, true, h);
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$HtmlDefs$idInput = 'edited_label';
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $author$project$HtmlDefs$renderedClass = 'rendered-callback';
var $author$project$HtmlDefs$renderedDecoder = A2(
	$elm$json$Json$Decode$field,
	'detail',
	A3(
		$elm$json$Json$Decode$map2,
		$elm$core$Tuple$pair,
		A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
		A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float)));
var $author$project$HtmlDefs$renderedEvent = 'rendered';
var $author$project$HtmlDefs$onRendered = function (onRender) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$Events$on,
			$author$project$HtmlDefs$renderedEvent,
			A2($elm$json$Json$Decode$map, onRender, $author$project$HtmlDefs$renderedDecoder)),
			$elm$html$Html$Attributes$class($author$project$HtmlDefs$renderedClass)
		]);
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $author$project$HtmlDefs$preventsDefaultOnKeyDown = F2(
	function (noOp, filter) {
		return A2(
			$elm$html$Html$Events$preventDefaultOn,
			'keydown',
			A3(
				$elm$json$Json$Decode$map2,
				F2(
					function (ks, k) {
						return A2(filter, ks, k) ? _Utils_Tuple2(noOp, true) : _Utils_Tuple2(noOp, false);
					}),
				$author$project$HtmlDefs$keysDecoder,
				$author$project$HtmlDefs$keyDecoder));
	});
var $author$project$Msg$onTabPreventDefault = A2(
	$author$project$HtmlDefs$preventsDefaultOnKeyDown,
	$author$project$Msg$noOp,
	F2(
		function (_v0, k) {
			return _Utils_eq(
				k,
				$author$project$HtmlDefs$Control('Tab'));
		}));
var $author$project$HtmlDefs$select = _Platform_outgoingPort('select', $elm$json$Json$Encode$string);
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$GraphDrawing$make_input = F3(
	function (pos, label, onChange) {
		return A4(
			$author$project$Drawing$html,
			$author$project$GraphDrawing$foregroundZ,
			pos,
			_Utils_Tuple2(100, 16),
			A2(
				$elm$html$Html$input,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$value(label),
							$elm$html$Html$Events$onInput(onChange),
							$author$project$Msg$onTabPreventDefault,
							$elm$html$Html$Attributes$id($author$project$HtmlDefs$idInput),
							$elm$html$Html$Attributes$autofocus(true),
							A2(
							$elm$html$Html$Attributes$style,
							'width',
							$elm$core$String$fromInt(
								$elm$core$String$length(label) + 1) + 'ch')
						]),
					_Utils_ap(
						$author$project$HtmlDefs$onRendered(
							$elm$core$Basics$always(
								$author$project$Msg$Do(
									$author$project$Msg$focusId($author$project$HtmlDefs$idInput)))),
						$author$project$HtmlDefs$onRendered(
							$elm$core$Basics$always(
								$author$project$Msg$Do(
									$author$project$HtmlDefs$select($author$project$HtmlDefs$idInput)))))),
				_List_Nil));
	});
var $author$project$Drawing$ZIndex = function (a) {
	return {$: 'ZIndex', a: a};
};
var $author$project$Drawing$zindexAttr = $author$project$Drawing$ZIndex;
var $author$project$GraphDrawing$nodeLabelDrawing = F3(
	function (cfg, attrs, node) {
		var n = node.label;
		var id = node.id;
		var color = $author$project$GraphDrawing$activityToColor(node.label.isActive);
		if (n.editable) {
			return A3(
				$author$project$GraphDrawing$make_input,
				n.inputPos,
				n.label,
				$author$project$Msg$NodeLabelEdit(id));
		} else {
			if (n.label === '') {
				return A3(
					$author$project$Drawing$circle,
					A2(
						$elm$core$List$cons,
						$author$project$Drawing$zindexAttr($author$project$GraphDrawing$foregroundZ),
						A2(
							$elm$core$List$cons,
							$author$project$Drawing$color(color),
							attrs)),
					n.pos,
					5);
			} else {
				var label = cfg.latexPreamble + ('\n' + (n.isMath ? n.label : ('\\text{' + (n.label + '}'))));
				return A5(
					$author$project$Drawing$htmlAnchor,
					$author$project$GraphDrawing$foregroundZ,
					n.pos,
					n.dims,
					true,
					A2(
						$author$project$HtmlDefs$makeLatex,
						_Utils_ap(
							_List_fromArray(
								[
									$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onClick(
									$author$project$Msg$NodeClick(id)),
									$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDoubleClick(
									$author$project$Msg$EltDoubleClick(id))
								]),
							_Utils_ap(
								A2(
									$elm$core$List$map,
									$elm$html$Html$Attributes$class,
									$author$project$GraphDrawing$activityToClasses(n.isActive)),
								$author$project$HtmlDefs$onRendered(
									$author$project$Msg$NodeRendered(id)))),
						label));
			}
		}
	});
var $author$project$GraphDrawing$nodeDrawing = F2(
	function (cfg, n) {
		return A3(
			$author$project$GraphDrawing$nodeLabelDrawing,
			cfg,
			_List_fromArray(
				[
					$author$project$Drawing$onClick(
					$author$project$Msg$NodeClick(n.id))
				]),
			n);
	});
var $author$project$Msg$MouseOn = function (a) {
	return {$: 'MouseOn', a: a};
};
var $author$project$Drawing$Class = function (a) {
	return {$: 'Class', a: a};
};
var $author$project$Drawing$class = $author$project$Drawing$Class;
var $author$project$ArrowStyle$doubleSize = 2.5;
var $author$project$ArrowStyle$isDouble = function (_v0) {
	var _double = _v0._double;
	return _double;
};
var $author$project$ArrowStyle$prefixDouble = function (_v0) {
	var _double = _v0._double;
	return _double ? 'double-' : '';
};
var $author$project$ArrowStyle$headFileName = function (s) {
	return $author$project$ArrowStyle$prefixDouble(s) + ($author$project$ArrowStyle$headToString(s.head) + '.svg');
};
var $elm$svg$Svg$image = $elm$svg$Svg$trustedNode('image');
var $author$project$ArrowStyle$imgDir = 'img/arrow/';
var $author$project$ArrowStyle$imgHeight = 13;
var $author$project$ArrowStyle$imgWidth = 9.764;
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $author$project$ArrowStyle$svgRotate = F2(
	function (_v0, angle) {
		var x2 = _v0.a;
		var y2 = _v0.b;
		return $elm$svg$Svg$Attributes$transform(
			' rotate(' + ($elm$core$String$fromFloat(angle) + (' ' + ($elm$core$String$fromFloat(x2) + (' ' + ($elm$core$String$fromFloat(y2) + ')'))))));
	});
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $author$project$ArrowStyle$makeImg = F3(
	function (_v0, angle, file) {
		var x = _v0.a;
		var y = _v0.b;
		var _v1 = _Utils_Tuple2(x - ($author$project$ArrowStyle$imgHeight / 2), y - ($author$project$ArrowStyle$imgHeight / 2));
		var xh = _v1.a;
		var yh = _v1.b;
		var f = $elm$core$String$fromFloat;
		return A2(
			$elm$svg$Svg$image,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$xlinkHref(
					_Utils_ap($author$project$ArrowStyle$imgDir, file)),
					$elm$svg$Svg$Attributes$x(
					f(xh)),
					$elm$svg$Svg$Attributes$y(
					f(yh)),
					$elm$svg$Svg$Attributes$width(
					f($author$project$ArrowStyle$imgWidth)),
					$elm$svg$Svg$Attributes$height(
					f($author$project$ArrowStyle$imgHeight)),
					A2(
					$author$project$ArrowStyle$svgRotate,
					_Utils_Tuple2(x, y),
					angle)
				]),
			_List_Nil);
	});
var $author$project$ArrowStyle$tailFileName = function (s) {
	return $author$project$ArrowStyle$prefixDouble(s) + ($author$project$ArrowStyle$tailToString(s.tail) + '.svg');
};
var $author$project$ArrowStyle$makeHeadTailImgs = F2(
	function (_v0, style) {
		var from = _v0.from;
		var to = _v0.to;
		var controlPoint = _v0.controlPoint;
		var angle = function (delta) {
			return ($author$project$Geometry$Point$pointToAngle(delta) * 180) / $elm$core$Basics$pi;
		};
		return _List_fromArray(
			[
				A3(
				$author$project$ArrowStyle$makeImg,
				to,
				angle(
					A2($author$project$Geometry$Point$subtract, to, controlPoint)),
				$author$project$ArrowStyle$headFileName(style)),
				A3(
				$author$project$ArrowStyle$makeImg,
				from,
				angle(
					A2($author$project$Geometry$Point$subtract, controlPoint, from)),
				$author$project$ArrowStyle$tailFileName(style))
			]);
	});
var $author$project$ArrowStyle$dashedStr = '7, 3';
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $author$project$Drawing$dashedToAttrs = function (dashed) {
	return dashed ? _List_fromArray(
		[
			$elm$svg$Svg$Attributes$strokeDasharray($author$project$ArrowStyle$dashedStr)
		]) : _List_Nil;
};
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $author$project$Drawing$quadraticBezierToAttr = function (_v0) {
	var from = _v0.from;
	var to = _v0.to;
	var controlPoint = _v0.controlPoint;
	var f = $elm$core$String$fromFloat;
	var p = function (_v1) {
		var x1 = _v1.a;
		var x2 = _v1.b;
		return f(x1) + (' ' + f(x2));
	};
	return $elm$svg$Svg$Attributes$d(
		'M' + (p(from) + (' Q ' + (p(controlPoint) + (', ' + p(to))))));
};
var $author$project$Drawing$mkPath = F3(
	function (dashed, attrs, q) {
		return A2(
			$elm$svg$Svg$path,
			A2(
				$elm$core$List$cons,
				$author$project$Drawing$quadraticBezierToAttr(q),
				A2(
					$elm$core$List$cons,
					$elm$svg$Svg$Attributes$fill('none'),
					_Utils_ap(
						A2($author$project$Drawing$attrsToSvgAttrs, $elm$svg$Svg$Attributes$stroke, attrs),
						$author$project$Drawing$dashedToAttrs(dashed)))),
			_List_Nil);
	});
var $author$project$Geometry$Point$orthogonal = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return _Utils_Tuple2(0 - y, x);
};
var $author$project$Geometry$Point$orthoVectPx = F3(
	function (from, to, px) {
		return A2(
			$author$project$Geometry$Point$normalise,
			px,
			$author$project$Geometry$Point$orthogonal(
				A2($author$project$Geometry$Point$subtract, to, from)));
	});
var $author$project$Geometry$QuadraticBezier$orthoVectPx = F2(
	function (px, _v0) {
		var from = _v0.from;
		var to = _v0.to;
		var controlPoint = _v0.controlPoint;
		var deltaFrom = A3($author$project$Geometry$Point$orthoVectPx, from, controlPoint, px);
		var deltaTo = A3($author$project$Geometry$Point$orthoVectPx, controlPoint, to, px);
		var deltaCp = A2(
			$author$project$Geometry$Point$normalise,
			$elm$core$Basics$abs(px),
			A2($author$project$Geometry$Point$add, deltaFrom, deltaTo));
		return {
			controlPoint: A2($author$project$Geometry$Point$add, controlPoint, deltaCp),
			from: A2($author$project$Geometry$Point$add, deltaFrom, from),
			to: A2($author$project$Geometry$Point$add, deltaTo, to)
		};
	});
var $author$project$Drawing$arrow = F3(
	function (attrs, style, q) {
		var zindex = $author$project$Drawing$attributesToZIndex(attrs);
		var imgs = A2($author$project$ArrowStyle$makeHeadTailImgs, q, style);
		var mkgen = F2(
			function (d, l) {
				return A2(
					$author$project$Drawing$mkPath,
					d,
					_Utils_ap(l, attrs));
			});
		var mkl = A2(mkgen, style.dashed, _List_Nil);
		var mkshadow = A2(
			mkgen,
			false,
			_List_fromArray(
				[
					$author$project$Drawing$class('shadow-line')
				]));
		var mkall = function (l) {
			return _Utils_ap(
				A2($elm$core$List$map, mkshadow, l),
				A2($elm$core$List$map, mkl, l));
		};
		var lines = $author$project$ArrowStyle$isDouble(style) ? mkall(
			_List_fromArray(
				[
					A2($author$project$Geometry$QuadraticBezier$orthoVectPx, 0 - $author$project$ArrowStyle$doubleSize, q),
					A2($author$project$Geometry$QuadraticBezier$orthoVectPx, $author$project$ArrowStyle$doubleSize, q)
				])) : mkall(
			_List_fromArray(
				[q]));
		return A2(
			$author$project$Drawing$ofSvgs,
			zindex,
			_Utils_ap(lines, imgs));
	});
var $author$project$Drawing$OnDoubleClick = function (a) {
	return {$: 'OnDoubleClick', a: a};
};
var $author$project$Drawing$onDoubleClick = $author$project$Drawing$OnDoubleClick;
var $author$project$Msg$EdgeLabelEdit = F2(
	function (a, b) {
		return {$: 'EdgeLabelEdit', a: a, b: b};
	});
var $author$project$Msg$EdgeRendered = F2(
	function (a, b) {
		return {$: 'EdgeRendered', a: a, b: b};
	});
var $author$project$Geometry$RoundedRectangle$RoundedRectangle = F3(
	function (centre, size, radius) {
		return {centre: centre, radius: radius, size: size};
	});
var $author$project$Geometry$Point$NamedPoint = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $author$project$Geometry$Epsilon$inv_epsilon = 1 / $author$project$Geometry$Epsilon$epsilon;
var $author$project$Geometry$Point$inv_scale = F3(
	function (sx, sy, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(x / sx, y / sy);
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$isEmpty(dict);
};
var $author$project$Geometry$Point$name = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return {x: x, y: y};
};
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$sin = _Basics_sin;
var $author$project$Geometry$Point$lendir = F2(
	function (length, direction) {
		return _Utils_Tuple2(
			length * $elm$core$Basics$cos(direction),
			length * $elm$core$Basics$sin(direction));
	});
var $author$project$Geometry$Point$scale = F3(
	function (sx, sy, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(x * sx, y * sy);
	});
var $author$project$Geometry$RoundedRectangle$points = F2(
	function (_this, max_segment_length) {
		var n = (!_this.radius) ? 0 : ($elm$core$Basics$pi / $elm$core$Basics$atan(max_segment_length / (2 * _this.radius)));
		var sides = $elm$core$Basics$ceiling(n);
		var r = _this.radius / $elm$core$Basics$cos($elm$core$Basics$pi / sides);
		var add_corner_points = F3(
			function (sx, sy, _v5) {
				var angle_offset = _v5.a;
				var pts = _v5.b;
				var pts2 = A2(
					$elm$core$List$cons,
					A2(
						$author$project$Geometry$Point$add,
						_this.centre,
						A2(
							$author$project$Geometry$Point$add,
							A2($author$project$Geometry$Point$lendir, _this.radius, angle_offset),
							A3(
								$author$project$Geometry$Point$scale,
								sx,
								sy,
								A2(
									$author$project$Geometry$Point$subtract,
									A2($author$project$Geometry$Point$resize, 0.5, _this.size),
									_Utils_Tuple2(_this.radius, _this.radius))))),
					pts);
				var _for = F2(
					function (i, pts_for) {
						if (_Utils_cmp(i, sides / 4) > -1) {
							return pts_for;
						} else {
							var angle = ((((i + 0.5) / sides) * 2) * $elm$core$Basics$pi) + angle_offset;
							var pt = A2(
								$author$project$Geometry$Point$add,
								_this.centre,
								A2(
									$author$project$Geometry$Point$add,
									A2($author$project$Geometry$Point$lendir, r, angle),
									A3(
										$author$project$Geometry$Point$scale,
										sx,
										sy,
										A2(
											$author$project$Geometry$Point$subtract,
											A2($author$project$Geometry$Point$resize, 0.5, _this.size),
											_Utils_Tuple2(_this.radius, _this.radius)))));
							return A2(
								_for,
								i + 1,
								A2($elm$core$List$cons, pt, pts_for));
						}
					});
				var pts3 = A2(_for, 0, pts2);
				var angle_offset2 = angle_offset + ($elm$core$Basics$pi / 2);
				var pt = A2(
					$author$project$Geometry$Point$add,
					_this.centre,
					A2(
						$author$project$Geometry$Point$add,
						A2($author$project$Geometry$Point$lendir, _this.radius, angle_offset2),
						A3(
							$author$project$Geometry$Point$scale,
							sx,
							sy,
							A2(
								$author$project$Geometry$Point$subtract,
								A2($author$project$Geometry$Point$resize, 0.5, _this.size),
								_Utils_Tuple2(_this.radius, _this.radius)))));
				return _Utils_Tuple2(
					angle_offset2,
					A2($elm$core$List$cons, pt, pts3));
			});
		var _v0 = A3(
			add_corner_points,
			1,
			-1,
			A3(
				add_corner_points,
				-1,
				-1,
				A3(
					add_corner_points,
					-1,
					1,
					A3(
						add_corner_points,
						1,
						1,
						_Utils_Tuple2(0, _List_Nil)))));
		var pts = _v0.b;
		var elim0 = F2(
			function (l, acc) {
				if (l.b && l.b.b) {
					var _v2 = l.a;
					var x1 = _v2.a;
					var y1 = _v2.b;
					var _v3 = l.b;
					var _v4 = _v3.a;
					var x2 = _v4.a;
					var y2 = _v4.b;
					var t = _v3.b;
					return A2(
						elim0,
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(x2, y2),
							t),
						((_Utils_cmp(
							$elm$core$Basics$abs(x2 - x1),
							$author$project$Geometry$Epsilon$epsilon) < 1) && (_Utils_cmp(
							$elm$core$Basics$abs(y2 - y1),
							$author$project$Geometry$Epsilon$epsilon) < 1)) ? acc : A2(
							$elm$core$List$cons,
							_Utils_Tuple2(x1, y1),
							acc));
				} else {
					return $elm$core$List$reverse(acc);
				}
			});
		return A2(elim0, pts, _List_Nil);
	});
var $author$project$Geometry$RoundedRectangle$points5 = function (_this) {
	return A2($author$project$Geometry$RoundedRectangle$points, _this, 5);
};
var $author$project$Geometry$Point$rotate = F2(
	function (theta, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			(x * $elm$core$Basics$cos(theta)) - (y * $elm$core$Basics$sin(theta)),
			(y * $elm$core$Basics$cos(theta)) + (x * $elm$core$Basics$sin(theta)));
	});
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $elm$core$Set$singleton = function (key) {
	return $elm$core$Set$Set_elm_builtin(
		A2($elm$core$Dict$singleton, key, _Utils_Tuple0));
};
var $author$project$Geometry$Bezier$x_intersections_with_nonvertical_line = F2(
	function (m, c) {
		var determinant = (((m * m) - (4 * m)) + 4) - (8 * c);
		return (determinant > 0) ? _List_fromArray(
			[
				((2 - m) + $elm$core$Basics$sqrt(determinant)) / 4,
				((2 - m) - $elm$core$Basics$sqrt(determinant)) / 4
			]) : ((!determinant) ? _List_fromArray(
			[
				((2 - m) + $elm$core$Basics$sqrt(determinant)) / 4
			]) : _List_Nil);
	});
var $author$project$Geometry$Bezier$y_intersection_with_vertical_line = function (a) {
	return (2 * a) * (1 - a);
};
var $author$project$Geometry$Bezier$intersections_with_rounded_rectangle = F3(
	function (_this, rect, permit_containment) {
		var h = (!_this.h) ? 1 : _this.h;
		var points = A2(
			$elm$core$List$map,
			function (p) {
				return $author$project$Geometry$Point$name(
					A3(
						$author$project$Geometry$Point$inv_scale,
						_this.w,
						h,
						A2(
							$author$project$Geometry$Point$rotate,
							-_this.angle,
							A2($author$project$Geometry$Point$subtract, p, _this.origin))));
			},
			$author$project$Geometry$RoundedRectangle$points5(rect));
		var add_intersection = F2(
			function (p, intersections) {
				return A2(
					$elm$core$Set$insert,
					_Utils_Tuple2(
						$elm$core$Basics$round(p.x * $author$project$Geometry$Epsilon$inv_epsilon) / $author$project$Geometry$Epsilon$inv_epsilon,
						$elm$core$Basics$round(p.y * $author$project$Geometry$Epsilon$inv_epsilon) / $author$project$Geometry$Epsilon$inv_epsilon),
					intersections);
			});
		var m_c = function (_v4) {
			var endpoint0 = _v4.a;
			var endpoint1 = _v4.b;
			var m = (endpoint1.y - endpoint0.y) / (endpoint1.x - endpoint0.x);
			return _Utils_Tuple2(m, endpoint0.y - (m * endpoint0.x));
		};
		var intersections2 = function () {
			if (!_this.h) {
				var _for = F3(
					function (pt0, pts, intersections) {
						if (!pts.b) {
							return intersections;
						} else {
							var endpoint0 = pts.a;
							var q = pts.b;
							var endpoint1 = A2(
								$elm$core$Maybe$withDefault,
								pt0,
								$elm$core$List$head(q));
							var endpoints = _Utils_Tuple2(endpoint0, endpoint1);
							return A3(
								_for,
								pt0,
								q,
								function () {
									if (_Utils_cmp(
										$elm$core$Basics$abs(endpoint0.x - endpoint1.x),
										$author$project$Geometry$Epsilon$epsilon) < 1) {
										return ((endpoint0.x >= 0) && ((endpoint0.x <= 1) && ((A2($elm$core$Basics$min, endpoint0.y, endpoint1.y) <= 0) && (A2($elm$core$Basics$max, endpoint0.y, endpoint1.y) >= 0)))) ? A2(
											add_intersection,
											A2($author$project$Geometry$Point$NamedPoint, endpoint0.x, 0),
											intersections) : intersections;
									} else {
										var _v1 = m_c(endpoints);
										var m = _v1.a;
										var c = _v1.b;
										if (_Utils_cmp(
											$elm$core$Basics$abs(m),
											$author$project$Geometry$Epsilon$epsilon) > 0) {
											var x = (-c) / m;
											return ((x >= 0) && ((x <= 1) && ((_Utils_cmp(
												x,
												A2($elm$core$Basics$min, endpoint0.x, endpoint1.x)) > -1) && (_Utils_cmp(
												x,
												A2($elm$core$Basics$max, endpoint0.x, endpoint1.x)) < 1)))) ? A2(
												add_intersection,
												A2($author$project$Geometry$Point$NamedPoint, x, 0),
												intersections) : intersections;
										} else {
											if (_Utils_cmp(
												$elm$core$Basics$abs(endpoint0.y),
												$author$project$Geometry$Epsilon$epsilon) < 1) {
												var minx = A2($elm$core$Basics$min, endpoint0.x, endpoint1.x);
												var maxx = A2($elm$core$Basics$min, endpoint0.x, endpoint1.x);
												return ((minx <= 1) && (maxx >= 0)) ? A2(
													add_intersection,
													A2(
														$author$project$Geometry$Point$NamedPoint,
														A2($elm$core$Basics$max, maxx, 1),
														0),
													A2(
														add_intersection,
														A2(
															$author$project$Geometry$Point$NamedPoint,
															A2($elm$core$Basics$max, minx, 0),
															0),
														intersections)) : intersections;
											} else {
												return intersections;
											}
										}
									}
								}());
						}
					});
				return A3(
					_for,
					A2(
						$elm$core$Maybe$withDefault,
						A2($author$project$Geometry$Point$NamedPoint, 0, 0),
						$elm$core$List$head(points)),
					points,
					$elm$core$Set$empty);
			} else {
				var _for = F3(
					function (pt0, pts, intersections) {
						if (!pts.b) {
							return intersections;
						} else {
							var endpoint0 = pts.a;
							var q = pts.b;
							var endpoint1 = A2(
								$elm$core$Maybe$withDefault,
								pt0,
								$elm$core$List$head(q));
							var endpoints = _Utils_Tuple2(endpoint0, endpoint1);
							return A3(
								_for,
								pt0,
								q,
								function () {
									if (_Utils_cmp(
										$elm$core$Basics$abs(endpoint0.x - endpoint1.x),
										$author$project$Geometry$Epsilon$epsilon) < 1) {
										var y = $author$project$Geometry$Bezier$y_intersection_with_vertical_line(endpoint0.x);
										return ((y >= 0) && ((_Utils_cmp(
											y,
											A2($elm$core$Basics$min, endpoint0.y, endpoint1.y)) > -1) && (_Utils_cmp(
											y,
											A2($elm$core$Basics$max, endpoint0.y, endpoint1.y)) < 1))) ? A2(
											add_intersection,
											A2($author$project$Geometry$Point$NamedPoint, endpoint0.x, y),
											intersections) : intersections;
									} else {
										var _v3 = m_c(endpoints);
										var m = _v3.a;
										var c = _v3.b;
										var inters = A2(
											$elm$core$List$map,
											function (x) {
												return A2($author$project$Geometry$Point$NamedPoint, x, (m * x) + c);
											},
											A2(
												$elm$core$List$filter,
												function (x) {
													return (x >= 0) && ((x <= 1) && ((_Utils_cmp(
														x,
														A2($elm$core$Basics$min, endpoint0.x, endpoint1.x)) > -1) && (_Utils_cmp(
														x,
														A2($elm$core$Basics$max, endpoint0.x, endpoint1.x)) < 1)));
												},
												A2($author$project$Geometry$Bezier$x_intersections_with_nonvertical_line, m, c)));
										return A3($elm$core$List$foldl, add_intersection, intersections, inters);
									}
								}());
						}
					});
				return A3(
					_for,
					A2(
						$elm$core$Maybe$withDefault,
						A2($author$project$Geometry$Point$NamedPoint, 0, 0),
						$elm$core$List$head(points)),
					points,
					$elm$core$Set$empty);
			}
		}();
		var intersections3 = function () {
			if ($elm$core$Set$isEmpty(intersections2)) {
				var sharp_rect = A3($author$project$Geometry$RoundedRectangle$RoundedRectangle, rect.centre, rect.size, 0);
				return A2(
					$author$project$Geometry$Point$isInPoly,
					_this.origin,
					$author$project$Geometry$RoundedRectangle$points5(sharp_rect)) ? (permit_containment ? $elm$core$Set$singleton(
					_Utils_Tuple2(0, 0)) : A2($elm$core$Debug$log, 'Bézier curve was entirely contained by rounded rectangle.', intersections2)) : intersections2;
			} else {
				return intersections2;
			}
		}();
		return A2(
			$elm$core$List$map,
			A2($author$project$Geometry$Point$scale, _this.w, h),
			$elm$core$Set$toList(intersections3));
	});
var $author$project$Geometry$Bezier$new = F4(
	function (origin, w, h, angle) {
		return {
			angle: angle,
			control: A2(
				$author$project$Geometry$Point$add,
				origin,
				_Utils_Tuple2(w / 2, h)),
			end: A2(
				$author$project$Geometry$Point$add,
				origin,
				_Utils_Tuple2(w, 0)),
			h: h,
			origin: origin,
			w: w
		};
	});
var $author$project$Geometry$Point$lerp = F3(
	function (_this, other, t) {
		return A2(
			$author$project$Geometry$Point$add,
			_this,
			A2(
				$author$project$Geometry$Point$resize,
				t,
				A2($author$project$Geometry$Point$subtract, other, _this)));
	});
var $author$project$Geometry$Bezier$point = F2(
	function (_this, t) {
		return A3(
			$author$project$Geometry$Point$lerp,
			A3($author$project$Geometry$Point$lerp, _this.origin, _this.control, t),
			A3($author$project$Geometry$Point$lerp, _this.control, _this.end, t),
			t);
	});
var $author$project$Geometry$determine_label_position = F9(
	function (length, angle, edge_width, start, end, curve, label_position, label_alignement, label_size) {
		var bezier = A4(
			$author$project$Geometry$Bezier$new,
			_Utils_Tuple2(0, 0),
			length,
			curve,
			angle);
		var centre = A2($author$project$Geometry$Bezier$point, bezier, start + ((end - start) * label_position));
		var offset_angle = function () {
			switch (label_alignement.$) {
				case 'Centre':
					return 0;
				case 'Over':
					return 0;
				case 'Left':
					return 0 - ($elm$core$Basics$pi / 2);
				default:
					return $elm$core$Basics$pi / 2;
			}
		}();
		var offset_allowance = 4;
		var bail_out = 1024;
		var _while = F3(
			function (i, offset_min, offset_max) {
				_while:
				while (true) {
					var label_offset = (offset_min + offset_max) / 2;
					if (!i) {
						return A2($elm$core$Debug$log, 'Had to bail out from determining label offset.', label_offset);
					} else {
						var nexti = i - 1;
						var rect_centre = A2(
							$author$project$Geometry$Point$add,
							A2($author$project$Geometry$Point$lendir, label_offset, angle + offset_angle),
							A2($author$project$Geometry$Point$rotate, angle, centre));
						var intersections = A3(
							$author$project$Geometry$Bezier$intersections_with_rounded_rectangle,
							bezier,
							A3(
								$author$project$Geometry$RoundedRectangle$RoundedRectangle,
								rect_centre,
								A2(
									$author$project$Geometry$Point$add,
									_Utils_Tuple2(edge_width, edge_width),
									label_size),
								edge_width / 2),
							true);
						if (_Utils_eq(intersections, _List_Nil)) {
							if ((offset_max - offset_min) < 1) {
								return label_offset;
							} else {
								var $temp$i = nexti,
									$temp$offset_min = offset_min,
									$temp$offset_max = label_offset;
								i = $temp$i;
								offset_min = $temp$offset_min;
								offset_max = $temp$offset_max;
								continue _while;
							}
						} else {
							var $temp$i = nexti,
								$temp$offset_min = label_offset,
								$temp$offset_max = offset_max;
							i = $temp$i;
							offset_min = $temp$offset_min;
							offset_max = $temp$offset_max;
							continue _while;
						}
					}
				}
			});
		var offset_min = 0;
		var offset_max = (offset_allowance + ($elm$core$Basics$abs(curve) / 2)) + $author$project$Geometry$Point$radius(
			A2(
				$author$project$Geometry$Point$resize,
				0.5,
				A2(
					$author$project$Geometry$Point$add,
					label_size,
					_Utils_Tuple2(edge_width, edge_width))));
		var label_offset = A3(_while, bail_out, offset_min, offset_max);
		return A2(
			$author$project$Geometry$Point$add,
			centre,
			A2($author$project$Geometry$Point$lendir, label_offset, offset_angle));
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onMove = A2($mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mousemove', $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var $author$project$GraphDrawing$segmentLabel = F6(
	function (cfg, q, edgeId, activity, label, curve) {
		var offset = 10 + ($author$project$ArrowStyle$isDouble(label.style) ? $author$project$ArrowStyle$doubleSize : 0);
		var labelpos = function () {
			var diffP = A2($author$project$Geometry$Point$subtract, q.to, q.from);
			var angle = $author$project$Geometry$Point$pointToAngle(diffP);
			var length = $author$project$Geometry$Point$radius(diffP);
			return A2(
				$author$project$Geometry$Point$add,
				q.from,
				A2(
					$author$project$Geometry$Point$rotate,
					angle,
					A9(
						$author$project$Geometry$determine_label_position,
						length,
						angle,
						2,
						0,
						1,
						curve * length,
						label.style.labelPosition,
						label.style.labelAlignment,
						label.editable ? _Utils_Tuple2(2, 2) : label.dims)));
		}();
		return label.editable ? A3(
			$author$project$GraphDrawing$make_input,
			labelpos,
			label.label,
			$author$project$Msg$EdgeLabelEdit(edgeId)) : ((label.label === '') ? $author$project$Drawing$empty : A4(
			$author$project$Drawing$html,
			$author$project$GraphDrawing$foregroundZ,
			labelpos,
			label.dims,
			A2(
				$author$project$HtmlDefs$makeLatex,
				_Utils_ap(
					_List_fromArray(
						[
							$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onClick(
							$author$project$Msg$EdgeClick(edgeId)),
							$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDoubleClick(
							$author$project$Msg$EltDoubleClick(edgeId)),
							$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onMove(
							$elm$core$Basics$always(
								$author$project$Msg$MouseOn(edgeId)))
						]),
					_Utils_ap(
						A2(
							$elm$core$List$map,
							$elm$html$Html$Attributes$class,
							$author$project$GraphDrawing$activityToClasses(activity)),
						$author$project$HtmlDefs$onRendered(
							$author$project$Msg$EdgeRendered(edgeId)))),
				cfg.latexPreamble + ('\n' + label.label))));
	});
var $author$project$Drawing$On = F2(
	function (a, b) {
		return {$: 'On', a: a, b: b};
	});
var $author$project$Drawing$on = $author$project$Drawing$On;
var $author$project$Drawing$simpleOn = F2(
	function (s, m) {
		return A2(
			$author$project$Drawing$on,
			s,
			$elm$json$Json$Decode$succeed(m));
	});
var $author$project$GraphDrawing$normalEdgeDrawing = F7(
	function (cfg, edgeId, activity, z, label, q, curve) {
		var c = $author$project$GraphDrawing$activityToColor(activity);
		return $author$project$Drawing$group(
			_List_fromArray(
				[
					A3(
					$author$project$Drawing$arrow,
					_List_fromArray(
						[
							$author$project$Drawing$zindexAttr(z),
							$author$project$Drawing$color(c),
							$author$project$Drawing$onClick(
							$author$project$Msg$EdgeClick(edgeId)),
							$author$project$Drawing$onDoubleClick(
							$author$project$Msg$EltDoubleClick(edgeId)),
							A2(
							$author$project$Drawing$simpleOn,
							'mousemove',
							$author$project$Msg$MouseOn(edgeId))
						]),
					label.style,
					q),
					A6($author$project$GraphDrawing$segmentLabel, cfg, q, edgeId, activity, label, curve)
				]));
	});
var $author$project$Geometry$pad = F2(
	function (n, _v0) {
		var pos = _v0.pos;
		var dims = _v0.dims;
		var n2 = n * 2;
		return {
			dims: A2(
				$author$project$Geometry$Point$add,
				dims,
				_Utils_Tuple2(n2, n2)),
			pos: pos
		};
	});
var $author$project$Geometry$Point$diamondPx = F3(
	function (p1, p2, d) {
		var mid = A2($author$project$Geometry$Point$middle, p1, p2);
		return A2(
			$author$project$Geometry$Point$add,
			mid,
			A3($author$project$Geometry$Point$orthoVectPx, p1, p2, d));
	});
var $author$project$Geometry$pxFromRatio = F3(
	function (p1, p2, r) {
		return r * A2($author$project$Geometry$Point$distance, p2, p1);
	});
var $elm$core$List$map3 = _List_map3;
var $author$project$Geometry$distance = F3(
	function (ro, rd, _v0) {
		var aa = _v0.a;
		var bb = _v0.b;
		var f = F3(
			function (x, roi, rdi) {
				return (x - roi) / rdi;
			});
		var dimLo = A4($elm$core$List$map3, f, aa, ro, rd);
		var dimHi = A4($elm$core$List$map3, f, bb, ro, rd);
		var dimLo2 = A3($elm$core$List$map2, $elm$core$Basics$min, dimLo, dimHi);
		var dimHi2 = A3($elm$core$List$map2, $elm$core$Basics$max, dimLo, dimHi);
		var _v1 = _Utils_Tuple2(
			$elm$core$List$maximum(dimLo2),
			$elm$core$List$minimum(dimHi2));
		if ((_v1.a.$ === 'Just') && (_v1.b.$ === 'Just')) {
			var maxLo = _v1.a.a;
			var minHi = _v1.b.a;
			return (_Utils_cmp(minHi, maxLo) < 0) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(maxLo);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Geometry$intersection = F3(
	function (ro, rd, aabb) {
		return A2(
			$elm$core$Maybe$andThen,
			function (d) {
				return $elm$core$Maybe$Just(
					A3(
						$elm$core$List$map2,
						F2(
							function (roi, rdi) {
								return roi + (rdi * d);
							}),
						ro,
						rd));
			},
			A3($author$project$Geometry$distance, ro, rd, aabb));
	});
var $author$project$Geometry$Point$toList = function (_v0) {
	var px = _v0.a;
	var py = _v0.b;
	return _List_fromArray(
		[px, py]);
};
var $author$project$Geometry$raytraceRect = F3(
	function (p1, p2, _v0) {
		var topLeft = _v0.topLeft;
		var bottomRight = _v0.bottomRight;
		var v = A2(
			$author$project$Geometry$Point$normalise,
			1,
			A2($author$project$Geometry$Point$subtract, p2, p1));
		var l = $author$project$Geometry$Point$toList;
		var _v1 = A3(
			$author$project$Geometry$intersection,
			l(p1),
			l(v),
			_Utils_Tuple2(
				l(topLeft),
				l(bottomRight)));
		if ((((_v1.$ === 'Just') && _v1.a.b) && _v1.a.b.b) && (!_v1.a.b.b.b)) {
			var _v2 = _v1.a;
			var ix = _v2.a;
			var _v3 = _v2.b;
			var iy = _v3.a;
			return $elm$core$Maybe$Just(
				_Utils_Tuple2(ix, iy));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Geometry$segmentRectBent = F3(
	function (r1, r2, bent) {
		var controlPoint = A3(
			$author$project$Geometry$Point$diamondPx,
			r1.pos,
			r2.pos,
			A3($author$project$Geometry$pxFromRatio, r1.pos, r2.pos, bent));
		var p2 = A2(
			$elm$core$Maybe$withDefault,
			r2.pos,
			A3(
				$author$project$Geometry$raytraceRect,
				controlPoint,
				r2.pos,
				$author$project$Geometry$rectFromPosDims(r2)));
		var p1 = A2(
			$elm$core$Maybe$withDefault,
			r1.pos,
			A3(
				$author$project$Geometry$raytraceRect,
				controlPoint,
				r1.pos,
				$author$project$Geometry$rectFromPosDims(r1)));
		return {controlPoint: controlPoint, from: p1, to: p2};
	});
var $author$project$GraphDrawing$graphDrawing = F2(
	function (cfg, g0) {
		var padding = 5;
		var drawEdge = F4(
			function (id, n1, n2, e) {
				var _v0 = e.details;
				if (_v0.$ === 'PullshoutEdge') {
					return {
						drawing: A2(
							$elm$core$Maybe$withDefault,
							$author$project$Drawing$empty,
							A3(
								$elm$core$Maybe$map2,
								A2($author$project$GraphDrawing$drawPullshout, id, e.isActive),
								n1.extrems,
								n2.extrems)),
						extrems: $elm$core$Maybe$Just(
							_Utils_Tuple2(n1.posDims.pos, n2.posDims.pos)),
						posDims: {
							dims: _Utils_Tuple2(0, 0),
							pos: _Utils_Tuple2(0, 0)
						}
					};
				} else {
					var l = _v0.a;
					var q = A3($author$project$Geometry$segmentRectBent, n1.posDims, n2.posDims, l.style.bend);
					return {
						drawing: A7($author$project$GraphDrawing$normalEdgeDrawing, cfg, id, e.isActive, e.zindex, l, q, l.style.bend),
						extrems: $elm$core$Maybe$Just(
							_Utils_Tuple2(n1.posDims.pos, n2.posDims.pos)),
						posDims: {
							dims: A2(
								$author$project$Geometry$Point$resize,
								4,
								_Utils_Tuple2(padding, padding)),
							pos: $author$project$Geometry$QuadraticBezier$middle(q)
						}
					};
				}
			});
		var g = A5(
			$author$project$Polygraph$mapRecAll,
			$elm$core$Basics$identity,
			$elm$core$Basics$identity,
			F2(
				function (id, n) {
					return {
						drawing: A2(
							$author$project$GraphDrawing$nodeDrawing,
							cfg,
							A2($author$project$Polygraph$Node, id, n)),
						extrems: $elm$core$Maybe$Nothing,
						posDims: A2(
							$author$project$Geometry$pad,
							padding,
							{
								dims: n.editable ? _Utils_Tuple2(0, 0) : n.dims,
								pos: n.pos
							})
					};
				}),
			drawEdge,
			g0);
		var nodes = A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.label;
				},
				function ($) {
					return $.drawing;
				}),
			$author$project$Polygraph$nodes(g));
		var edges = A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.label;
				},
				function ($) {
					return $.drawing;
				}),
			$author$project$Polygraph$edges(g));
		var drawings = _Utils_ap(nodes, edges);
		return $author$project$Drawing$group(drawings);
	});
var $author$project$GraphDefs$makeSelection = function (g) {
	return A3(
		$author$project$Polygraph$any,
		function ($) {
			return $.selected;
		},
		function ($) {
			return $.selected;
		},
		g) ? g : $author$project$GraphDefs$addWeaklySelected(g);
};
var $author$project$GraphDrawing$MainActive = {$: 'MainActive'};
var $author$project$GraphDrawing$NoActive = {$: 'NoActive'};
var $author$project$GraphDrawing$WeakActive = {$: 'WeakActive'};
var $author$project$GraphDrawing$NormalEdge = function (a) {
	return {$: 'NormalEdge', a: a};
};
var $author$project$GraphDrawing$PullshoutEdge = {$: 'PullshoutEdge'};
var $author$project$GraphDefs$getEdgeDims = function (n) {
	var _v0 = n.dims;
	if (_v0.$ === 'Nothing') {
		return $author$project$GraphDefs$defaultDims(n.label);
	} else {
		var p = _v0.a;
		return p;
	}
};
var $author$project$GraphDrawing$make_edgeDrawingLabel = F2(
	function (_v0, e) {
		var editable = _v0.editable;
		var isActive = _v0.isActive;
		return {
			details: function () {
				var _v1 = e.details;
				if (_v1.$ === 'PullshoutEdge') {
					return $author$project$GraphDrawing$PullshoutEdge;
				} else {
					var l = _v1.a;
					var label = l.label;
					var style = l.style;
					return $author$project$GraphDrawing$NormalEdge(
						{
							dims: $author$project$GraphDefs$getEdgeDims(l),
							editable: editable,
							label: label,
							style: style
						});
				}
			}(),
			isActive: isActive,
			zindex: e.zindex
		};
	});
var $author$project$GraphDrawing$make_nodeDrawingLabel = F2(
	function (_v0, l) {
		var editable = _v0.editable;
		var isActive = _v0.isActive;
		var label = l.label;
		var pos = l.pos;
		var isMath = l.isMath;
		var nodePos = $author$project$GraphDefs$getNodePos(l);
		return {
			dims: $author$project$GraphDefs$getNodeDims(l),
			editable: editable,
			inputPos: pos,
			isActive: isActive,
			isMath: isMath,
			label: label,
			pos: nodePos
		};
	});
var $author$project$GraphDrawing$toDrawingGraph = function () {
	var makeActivity = function (r) {
		return r.selected ? $author$project$GraphDrawing$MainActive : (r.weaklySelected ? $author$project$GraphDrawing$WeakActive : $author$project$GraphDrawing$NoActive);
	};
	return A2(
		$author$project$Polygraph$map,
		F2(
			function (_v0, n) {
				return A2(
					$author$project$GraphDrawing$make_nodeDrawingLabel,
					{
						editable: false,
						isActive: makeActivity(n)
					},
					n);
			}),
		F2(
			function (_v1, e) {
				return A2(
					$author$project$GraphDrawing$make_edgeDrawingLabel,
					{
						editable: false,
						isActive: makeActivity(e)
					},
					e);
			}));
}();
var $author$project$Model$collageGraphFromGraph = F2(
	function (_v0, g) {
		return $author$project$GraphDrawing$toDrawingGraph(
			$author$project$GraphDefs$makeSelection(g));
	});
var $author$project$Modes$NewArrow$graphDrawing = F2(
	function (m, s) {
		return A2(
			$author$project$Model$collageGraphFromGraph,
			m,
			function () {
				var info = A2($author$project$Modes$NewArrow$moveNodeInfo, m, s);
				return info.graph;
			}());
	});
var $author$project$Modes$Pullshout$graphDrawing = F2(
	function (m, s) {
		return A2(
			$author$project$Model$collageGraphFromGraph,
			m,
			A2($author$project$Modes$Pullshout$graph, m, s));
	});
var $author$project$Modes$SplitArrow$graphDrawing = F2(
	function (m, state) {
		var info = A2($author$project$Modes$SplitArrow$stateInfo, m, state);
		return A2($author$project$Model$collageGraphFromGraph, m, info.graph);
	});
var $author$project$GraphDrawing$makeActive = function (l) {
	return A3(
		$author$project$Polygraph$updateList,
		l,
		function (n) {
			return _Utils_update(
				n,
				{isActive: $author$project$GraphDrawing$MainActive});
		},
		function (e) {
			return _Utils_update(
				e,
				{isActive: $author$project$GraphDrawing$MainActive});
		});
};
var $author$project$Modes$Square$squareMode_activeObj = function (info) {
	return _List_fromArray(
		[info.e1, info.e2, info.ne1, info.ne2]);
};
var $author$project$Modes$Square$graphDrawingFromInfo = function (info) {
	return $author$project$GraphDrawing$makeActive(
		$author$project$Modes$Square$squareMode_activeObj(info));
};
var $author$project$Modes$Square$stateInfo = F2(
	function (m, s) {
		var _v0 = A2($author$project$Modes$Square$moveNodeViewInfo, m, s);
		var info = _v0.a;
		return info;
	});
var $author$project$Modes$Square$graphDrawing = F2(
	function (m, state) {
		var info = A2($author$project$Modes$Square$stateInfo, m, state);
		return A2(
			$author$project$Modes$Square$graphDrawingFromInfo,
			info.edges,
			A2($author$project$Model$collageGraphFromGraph, m, info.graph));
	});
var $author$project$GraphDrawing$mapNormalEdge = F2(
	function (f, e) {
		return {
			details: function () {
				var _v0 = e.details;
				if (_v0.$ === 'PullshoutEdge') {
					return $author$project$GraphDrawing$PullshoutEdge;
				} else {
					var l = _v0.a;
					return $author$project$GraphDrawing$NormalEdge(
						f(l));
				}
			}(),
			isActive: e.isActive,
			zindex: e.zindex
		};
	});
var $author$project$Main$graphDrawingFromModel = function (m) {
	var _v0 = m.mode;
	switch (_v0.$) {
		case 'DefaultMode':
			return A2($author$project$Model$collageGraphFromGraph, m, m.graph);
		case 'RectSelect':
			var p = _v0.a;
			return $author$project$GraphDrawing$toDrawingGraph(
				A3($author$project$Main$selectGraph, m, p, m.specialKeys.shift));
		case 'EnlargeMode':
			var p = _v0.a;
			return A2(
				$author$project$Model$collageGraphFromGraph,
				m,
				A2($author$project$Main$enlargeGraph, m, p));
		case 'QuickInputMode':
			var ch = _v0.a;
			return A2(
				$author$project$Model$collageGraphFromGraph,
				m,
				A2($author$project$Main$graphQuickInput, m, ch));
		case 'Move':
			var s = _v0.a;
			return A2(
				$author$project$Model$collageGraphFromGraph,
				m,
				A2($author$project$Main$info_MoveNode, m, s).graph);
		case 'RenameMode':
			var l = _v0.b;
			var g = A2($author$project$Main$graph_RenameMode, l, m);
			var g2 = A2($author$project$Model$collageGraphFromGraph, m, g);
			if (l.b) {
				var _v2 = l.a;
				var id = _v2.a;
				return A4(
					$author$project$Polygraph$update,
					id,
					function (n) {
						return _Utils_update(
							n,
							{editable: true});
					},
					$author$project$GraphDrawing$mapNormalEdge(
						function (e) {
							return _Utils_update(
								e,
								{editable: true});
						}),
					g2);
			} else {
				return g2;
			}
		case 'DebugMode':
			return A3(
				$author$project$Polygraph$map,
				F2(
					function (id, n) {
						return _Utils_update(
							n,
							{
								label: $elm$core$String$fromInt(id)
							});
					}),
				function (_v3) {
					return $elm$core$Basics$identity;
				},
				A2($author$project$Model$collageGraphFromGraph, m, m.graph));
		case 'NewArrow':
			var astate = _v0.a;
			return A2($author$project$Modes$NewArrow$graphDrawing, m, astate);
		case 'SquareMode':
			var state = _v0.a;
			return A2($author$project$Modes$Square$graphDrawing, m, state);
		case 'SplitArrow':
			var state = _v0.a;
			return A2($author$project$Modes$SplitArrow$graphDrawing, m, state);
		case 'PullshoutMode':
			var state = _v0.a;
			return A2($author$project$Modes$Pullshout$graphDrawing, m, state);
		case 'CutHead':
			var state = _v0.a;
			return $author$project$GraphDrawing$toDrawingGraph(
				A2($author$project$Main$graphCutHead, state, m));
		case 'CloneMode':
			return $author$project$GraphDrawing$toDrawingGraph(
				$author$project$Main$graphClone(m));
		default:
			var sizeGrid = _v0.a;
			return $author$project$GraphDrawing$toDrawingGraph(
				A2($author$project$Main$graphResize, sizeGrid, m));
	}
};
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$pattern = $elm$svg$Svg$trustedNode('pattern');
var $elm$svg$Svg$Attributes$patternUnits = _VirtualDom_attribute('patternUnits');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $author$project$Drawing$grid = function (n) {
	var sn = $elm$core$String$fromInt(n);
	return A2(
		$author$project$Drawing$ofSvgs,
		$author$project$Drawing$defaultZ,
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$defs,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$pattern,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id('grid'),
								$elm$svg$Svg$Attributes$width(sn),
								$elm$svg$Svg$Attributes$height(sn),
								$elm$svg$Svg$Attributes$patternUnits('userSpaceOnUse')
							]),
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$path,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$d('M ' + (sn + (' 0 L 0 0 0 ' + sn))),
										$elm$svg$Svg$Attributes$fill('none'),
										$elm$svg$Svg$Attributes$stroke('gray'),
										$elm$svg$Svg$Attributes$strokeWidth('1px')
									]),
								_List_Nil)
							]))
					])),
				A2(
				$elm$svg$Svg$rect,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$width('100%'),
						$elm$svg$Svg$Attributes$height('100%'),
						$elm$svg$Svg$Attributes$fill('url(#grid)')
					]),
				_List_Nil)
			]));
};
var $author$project$Main$Plain = {$: 'Plain'};
var $author$project$ArrowStyle$controlChars = '>(=-bBA][';
var $author$project$Modes$NewArrow$help = '[ESC] cancel, [click, TAB] name the point (if new), ' + ('[hjkl] position the new point with the keyboard, ' + ('[RET] terminate the arrow creation, ' + ('[\"' + ($author$project$ArrowStyle$controlChars + ('\"] alternate between different arrow styles, ' + ('[i]nvert arrow, ' + '[p]ullback/[P]ushout mode.'))))));
var $author$project$Modes$Pullshout$help = '[ESC] cancel, ' + ('cycle between [p]ullback/[P]ushout possibilities, ' + '[RET] confirm');
var $author$project$Modes$SplitArrow$help = '[ESC] cancel, [click] name the point (if new), ' + ('[/] to move the existing label on the other edge, ' + '[RET] terminate the square creation');
var $author$project$Modes$Square$help = '[ESC] cancel, [click] name the point (if new), ' + ('[RET] terminate the square creation, ' + (' alternative possible [s]quares, ' + ' [a]lternative possible labels.'));
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $elm$parser$Parser$Advanced$end = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				$elm$core$String$length(s.src),
				s.offset) ? A3($elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $author$project$Main$Bold = {$: 'Bold'};
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 'Expecting', a: a};
};
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$token = function (str) {
	return $elm$parser$Parser$Advanced$token(
		$elm$parser$Parser$toToken(str));
};
var $author$project$Main$helpMsgParser_aux = function () {
	var varParser = function (cend) {
		var correctChar = function (c) {
			return !_Utils_eq(c, cend);
		};
		return $elm$parser$Parser$variable(
			{inner: correctChar, reserved: $elm$core$Set$empty, start: correctChar});
	};
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						function (s) {
							return _Utils_Tuple2(s, $author$project$Main$Bold);
						}),
					$elm$parser$Parser$token('[')),
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									$elm$parser$Parser$succeed($elm$core$Basics$identity),
									$elm$parser$Parser$token('\"')),
								A2(
									$elm$parser$Parser$ignorer,
									varParser(
										_Utils_chr('\"')),
									$elm$parser$Parser$token('\"'))),
								varParser(
								_Utils_chr(']'))
							])),
					$elm$parser$Parser$symbol(']'))),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					function (s) {
						return _Utils_Tuple2(s, $author$project$Main$Plain);
					}),
				varParser(
					_Utils_chr('[')))
			]));
}();
function $author$project$Main$cyclic$helpMsgParser() {
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(_List_Nil),
					$elm$parser$Parser$end),
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						$elm$parser$Parser$succeed($elm$core$List$cons),
						$author$project$Main$helpMsgParser_aux),
					$elm$parser$Parser$lazy(
						function (_v0) {
							return $author$project$Main$cyclic$helpMsgParser();
						}))
				])));
}
try {
	var $author$project$Main$helpMsgParser = $author$project$Main$cyclic$helpMsgParser();
	$author$project$Main$cyclic$helpMsgParser = function () {
		return $author$project$Main$helpMsgParser;
	};
} catch ($) {
	throw 'Some top-level definitions from `Main` are causing infinite recursion:\n\n  ┌─────┐\n  │    helpMsgParser\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $elm$html$Html$b = _VirtualDom_node('b');
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$core$String$lines = _String_lines;
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Main$helpStr_collage = function (_v0) {
	var s = _v0.a;
	var h = _v0.b;
	if (h.$ === 'Bold') {
		return A2(
			$elm$html$Html$span,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('['),
					A2(
					$elm$html$Html$b,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(s)
						])),
					$elm$html$Html$text(']')
				]));
	} else {
		return A2(
			$elm$html$Html$span,
			_List_Nil,
			A2(
				$elm$core$List$intersperse,
				A2($elm$html$Html$br, _List_Nil, _List_Nil),
				A2(
					$elm$core$List$map,
					$elm$html$Html$text,
					$elm$core$String$lines(s))));
	}
};
var $author$project$Main$helpMsg = function (model) {
	var cl = $elm$html$Html$Attributes$class('help-div');
	var makeHelpDiv = function (l) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[cl]),
			l);
	};
	var msg = function (s) {
		return makeHelpDiv(
			A2(
				$elm$core$List$map,
				$author$project$Main$helpStr_collage,
				A2(
					$elm$core$Result$withDefault,
					_List_fromArray(
						[
							_Utils_Tuple2('Parsing help msg error', $author$project$Main$Plain)
						]),
					A2($elm$parser$Parser$run, $author$project$Main$helpMsgParser, s))));
	};
	var _v0 = model.mode;
	switch (_v0.$) {
		case 'DefaultMode':
			return msg('Default mode (the basic tutorial can be completed before reading this). ' + ('Sumary of commands:\n' + ('Selection:' + ('  [click] for point/edge selection (hold for selection rectangle)' + (', [shift] to keep previous selection' + (', [C-a] select all' + (', [S]elect pointer surrounding subdiagram' + (', [u] expand selection to connected component' + (', [ESC] or [w] clear selection' + (', [H] and [L]: select subdiagram adjacent to selected edge' + (', [hjkl] move the selection from a point to another' + ('\nHistory: ' + ('[C-z] undo' + (', [Q]uicksave' + ('\nCopy/Paste: ' + ('[C-c] copy selection' + (', [C-v] paste' + (', [M-c] clone selection (same as C-c C-v)' + ('\n Basic editing: ' + ('new [p]oint' + (', new [t]ext' + (', [del]ete selected object (also [x])' + (', [q] find and replace in selection' + (', [r]ename selected object (or double click)' + (', new (commutative) [s]quare on selected point (with two already connected edges)' + ('\nArrows: ' + ('new [a]rrow from selected point' + (', [/] split arrow' + (', [c]ut head of selected arrow' + (', if an arrow is selected: [\"' + ($author$project$ArrowStyle$controlChars + ('\"] alternate between different arrow styles, [i]nvert arrow, ' + ('[+<] move to the foreground/background.' + ('\nMoving objects:' + ('[g] move selected objects (also merge, if wanted)' + (', [f]ix (snap) selected objects on the grid' + (', [e]nlarge diagram (create row/column spaces)' + ('\n\nMiscelleanous: ' + ('[R]esize canvas and grid size' + (', [d]ebug mode' + (', [G]enerate Coq script ([T]: generate test Coq script)' + (', [C] generate Coq script to address selected incomplete subdiagram ' + ('(i.e., a subdiagram with an empty branch)' + ', [E] enter an equation (prompt)')))))))))))))))))))))))))))))))))))))))))));
		case 'DebugMode':
			return makeHelpDiv(
				$elm$core$List$singleton(
					$elm$html$Html$text(
						'Debug Mode. [ESC] to cancel and come back to the default mode. ' + $elm$core$Debug$toString(model))));
		case 'NewArrow':
			return msg('Mode NewArrow. ' + $author$project$Modes$NewArrow$help);
		case 'PullshoutMode':
			return msg('Mode Pullback/Pullshout. ' + $author$project$Modes$Pullshout$help);
		case 'SquareMode':
			return msg('Mode Commutative square. ' + $author$project$Modes$Square$help);
		case 'SplitArrow':
			return msg('Mode Split Arrow. ' + $author$project$Modes$SplitArrow$help);
		case 'Move':
			return msg('Mode Move.' + ('Use mouse or h,j,k,l. [RET] or [click] to confirm.' + ' Hold [ctrl] to merge the selected point onto another node.'));
		case 'CutHead':
			return msg('Mode cut arrow.' + (' [RET] or [click] to confirm, [ctrl] to merge the endpoint with existing node. [ESC] to cancel. ' + ('[c] to switch between head/tail' + ', [d] to duplicate (or not) the arrow.')));
		case 'RenameMode':
			return msg('Rename mode: [RET] to confirm, [TAB] to next label, [ESC] to cancel');
		case 'EnlargeMode':
			var s = _v0.a;
			return msg('Enlarge mode: draw a rectangle to create space. ' + 'Use mouse or h,j,k,l. [RET] or click to confirm.');
		case 'QuickInputMode':
			return msg('Equation mode: enter equation in the textfield ' + ('(e.g., a -- f -> b -- g -> c =  a -- h -> d -- k -> c)' + (',  [RET] to confirm, [ESC] to cancel.' + (' If an incomplete subdiagram (i.e. a subdiagram ' + ('where one branch is a single arrow with empty label)' + (' is selected, it will replace the empty branch with' + ' the lhs or the rhs (depending on the orientation).'))))));
		case 'ResizeMode':
			var onlyGrid = _v0.a.onlyGrid;
			return msg(
				'Resize mode. [k]/[j] to increase/decrease, ' + ('or use the slider above. ' + (onlyGrid ? '[g] to resize the objects as well as the grid. ' : ('[g] to resize the grid only. ' + ('[ESC] to cancel, ' + '[RET] to confirm')))));
		default:
			var txt = 'Mode: ' + ($elm$core$Debug$toString(model.mode) + ('. [ESC] to cancel and come back to the default' + ' mode.'));
			return makeHelpDiv(
				_List_fromArray(
					[
						$elm$html$Html$text(txt)
					]));
	}
};
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $author$project$Modes$isResizeMode = function (m) {
	if (m.$ === 'ResizeMode') {
		return true;
	} else {
		return false;
	}
};
var $author$project$HtmlDefs$latexPreambleId = 'latex-preamble';
var $author$project$HtmlDefs$pasteEvent = 'pasteData';
var $author$project$HtmlDefs$onPaste = function (handler) {
	return A2(
		$elm$html$Html$Events$on,
		$author$project$HtmlDefs$pasteEvent,
		A2($elm$json$Json$Decode$map, handler, $elm$json$Json$Decode$value));
};
var $author$project$HtmlDefs$pasteElement = 'paste-capture';
var $author$project$HtmlDefs$makePasteCapture = F3(
	function (handler, attrs, s) {
		return A3(
			$elm$html$Html$node,
			$author$project$HtmlDefs$pasteElement,
			A2(
				$elm$core$List$cons,
				$author$project$HtmlDefs$onPaste(handler),
				attrs),
			s);
	});
var $author$project$Model$modedSizeGrid = function (m) {
	var _v0 = m.mode;
	if (_v0.$ === 'ResizeMode') {
		var s = _v0.a;
		return s.sizeGrid;
	} else {
		return m.sizeGrid;
	}
};
var $elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$Main$pasteGraph = _Platform_outgoingPort('pasteGraph', $elm$core$Basics$identity);
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $author$project$Main$quickInputView = function (m) {
	return A2(
		$elm$html$Html$p,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text('Enter equation: '),
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('text'),
						$elm$html$Html$Attributes$id($author$project$HtmlDefs$quickInputId),
						$elm$html$Html$Events$onInput(
						$author$project$Msg$QuickInput(false)),
						$elm$html$Html$Attributes$value(m.quickInput)
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty('min');
var $author$project$HtmlDefs$slider = F5(
	function (msg, name, min, max, value) {
		var f = $elm$core$String$fromInt;
		return A2(
			$elm$html$Html$label,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'padding', '20px')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('range'),
							$elm$html$Html$Attributes$value(
							f(value)),
							$elm$html$Html$Attributes$min(
							f(min)),
							$elm$html$Html$Attributes$max(
							f(max)),
							$elm$html$Html$Events$onInput(
							A2(
								$elm$core$Basics$composeR,
								$elm$core$String$toInt,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Maybe$withDefault(value),
									msg)))
						]),
					_List_Nil),
					$elm$html$Html$text(name)
				]));
	});
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $author$project$Drawing$svg = F2(
	function (l, d) {
		return A2(
			$elm$svg$Svg$svg,
			l,
			A2(
				$elm$core$List$map,
				function ($) {
					return $.svg;
				},
				A2(
					$elm$core$List$sortBy,
					function ($) {
						return $.zindex;
					},
					$author$project$Drawing$drawingToZSvgs(d))));
	});
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $author$project$Main$view = function (model) {
	var missings = $author$project$Polygraph$invalidEdges(model.graph);
	var cfg = {
		latexPreamble: function () {
			var _v0 = model.scenario;
			if (_v0.$ === 'Exercise1') {
				return '\\newcommand{\\depthHistory}{' + ($elm$core$String$fromInt(
					$elm$core$List$length(model.history)) + '}');
			} else {
				return model.latexPreamble;
			}
		}()
	};
	var drawings = A2(
		$author$project$GraphDrawing$graphDrawing,
		cfg,
		$author$project$Main$graphDrawingFromModel(model));
	var grid = model.hideGrid ? $author$project$Drawing$empty : $author$project$Drawing$grid(
		$author$project$Model$modedSizeGrid(model));
	var nmissings = $elm$core$List$length(missings);
	var svg = A2(
		$author$project$Drawing$svg,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$id($author$project$HtmlDefs$canvasId),
				A2($elm$html$Html$Attributes$style, 'border-style', 'solid'),
				A2(
				$elm$html$Html$Events$on,
				'mousemove',
				A3($elm$json$Json$Decode$map2, $author$project$Msg$MouseMoveRaw, $elm$json$Json$Decode$value, $author$project$HtmlDefs$keysDecoder)),
				$elm$html$Html$Events$onMouseLeave($author$project$Msg$MouseLeaveCanvas),
				A3(
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions,
				'mousedown',
				{preventDefault: false, stopPropagation: false},
				$author$project$Msg$MouseDown),
				$elm$html$Html$Events$onMouseUp($author$project$Msg$MouseUp)
			]),
		$author$project$Drawing$group(
			_List_fromArray(
				[
					grid,
					drawings,
					$author$project$Main$additionnalDrawing(model),
					$author$project$Drawing$emptyForeign
				])));
	var contents = _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$text(model.statusMsg),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Main$helpMsg(model),
						$author$project$Main$quickInputView(model)
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Msg$Save),
						$elm$html$Html$Attributes$id('save-button')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Save')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Msg$Clear)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Clear')
					])),
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$href('#' + $author$project$HtmlDefs$latexPreambleId)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Latex preamble')
					])),
				A4($author$project$HtmlDefs$checkbox, $author$project$Msg$ToggleHideGrid, 'Show grid', '', !model.hideGrid),
				A4($author$project$HtmlDefs$checkbox, $author$project$Msg$ToggleAutosave, 'Autosave', 'Quicksave every minute', model.autoSave),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Msg$ExportQuiver)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Export selection to quiver')
					]))
			]),
		_Utils_ap(
			$author$project$Modes$isResizeMode(model.mode) ? _List_fromArray(
				[
					A5(
					$author$project$HtmlDefs$slider,
					$author$project$Msg$SizeGrid,
					'Grid size',
					$author$project$Model$minSizeGrid,
					$author$project$Model$maxSizeGrid,
					$author$project$Model$modedSizeGrid(model))
				]) : _List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							(nmissings > 0) ? ($elm$core$String$fromInt(nmissings) + ' nodes or edges could not be rendered.') : '')
						])),
					A3(
					$author$project$HtmlDefs$makePasteCapture,
					A2($elm$core$Basics$composeL, $author$project$Msg$Do, $author$project$Main$pasteGraph),
					_List_Nil,
					_List_fromArray(
						[svg])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$textarea,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$cols(100),
									$elm$html$Html$Attributes$rows(100),
									$elm$html$Html$Attributes$value(model.bottomText),
									$elm$html$Html$Attributes$id($author$project$HtmlDefs$bottomTextId)
								]),
							_List_Nil),
							$elm$html$Html$text('latex preamble'),
							A2(
							$elm$html$Html$textarea,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$cols(100),
									$elm$html$Html$Attributes$rows(100),
									$elm$html$Html$Attributes$placeholder('latex Preamble'),
									$elm$html$Html$Attributes$value(model.latexPreamble),
									$elm$html$Html$Attributes$id($author$project$HtmlDefs$latexPreambleId),
									$elm$html$Html$Events$onInput($author$project$Msg$LatexPreambleEdit)
								]),
							_List_Nil)
						]))
				])));
	return A2($elm$html$Html$div, _List_Nil, contents);
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		init: function (_v0) {
			return _Utils_Tuple2($author$project$Model$iniModel, $elm$core$Platform$Cmd$none);
		},
		subscriptions: $author$project$Main$subscriptions,
		update: $author$project$Main$updateIntercept,
		view: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));