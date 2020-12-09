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



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
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
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
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
var $author$project$Model$Authoring = {$: 'Authoring'};
var $author$project$Main$canvasHeight = 200;
var $author$project$Main$canvasWidth = 600;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Data$data = $elm$core$Dict$empty;
var $author$project$Timeline$resolution = 1000;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$Timeline$scaleInvert = F2(
	function (model, pct) {
		return $elm$time$Time$millisToPosix(
			$elm$core$Basics$round(
				((pct / 100) * ($elm$time$Time$posixToMillis(model.maxTime) - $elm$time$Time$posixToMillis(model.minTime))) + $elm$time$Time$posixToMillis(model.minTime)));
	});
var $author$project$Timeline$evalTimes = function (model) {
	return A2(
		$elm$core$List$map,
		$author$project$Timeline$scaleInvert(model),
		A2(
			$elm$core$List$map,
			function (i) {
				return (i / $author$project$Timeline$resolution) * 100;
			},
			A2($elm$core$List$range, 0, $author$project$Timeline$resolution)));
};
var $author$project$Formula$And = F2(
	function (a, b) {
		return {$: 'And', a: a, b: b};
	});
var $author$project$Formula$BoundedEventually = F3(
	function (a, b, c) {
		return {$: 'BoundedEventually', a: a, b: b, c: c};
	});
var $author$project$Formula$EvaluatedAnd = F3(
	function (a, b, c) {
		return {$: 'EvaluatedAnd', a: a, b: b, c: c};
	});
var $author$project$Formula$EvaluatedBoundedEventually = F4(
	function (a, b, c, d) {
		return {$: 'EvaluatedBoundedEventually', a: a, b: b, c: c, d: d};
	});
var $author$project$Formula$EvaluatedBoundedGlobally = F4(
	function (a, b, c, d) {
		return {$: 'EvaluatedBoundedGlobally', a: a, b: b, c: c, d: d};
	});
var $author$project$Formula$EvaluatedBoundedUntil = F5(
	function (a, b, c, d, e) {
		return {$: 'EvaluatedBoundedUntil', a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$Formula$EvaluatedEventually = F2(
	function (a, b) {
		return {$: 'EvaluatedEventually', a: a, b: b};
	});
var $author$project$Formula$EvaluatedFalsity = function (a) {
	return {$: 'EvaluatedFalsity', a: a};
};
var $author$project$Formula$EvaluatedGlobally = F2(
	function (a, b) {
		return {$: 'EvaluatedGlobally', a: a, b: b};
	});
var $author$project$Formula$EvaluatedImplies = F3(
	function (a, b, c) {
		return {$: 'EvaluatedImplies', a: a, b: b, c: c};
	});
var $author$project$Formula$EvaluatedNot = F2(
	function (a, b) {
		return {$: 'EvaluatedNot', a: a, b: b};
	});
var $author$project$Formula$EvaluatedOr = F3(
	function (a, b, c) {
		return {$: 'EvaluatedOr', a: a, b: b, c: c};
	});
var $author$project$Formula$EvaluatedState = F2(
	function (a, b) {
		return {$: 'EvaluatedState', a: a, b: b};
	});
var $author$project$Formula$EvaluatedTruth = function (a) {
	return {$: 'EvaluatedTruth', a: a};
};
var $author$project$Formula$EvaluatedUntil = F3(
	function (a, b, c) {
		return {$: 'EvaluatedUntil', a: a, b: b, c: c};
	});
var $author$project$Formula$Until = F2(
	function (a, b) {
		return {$: 'Until', a: a, b: b};
	});
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
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$Formula$boundToMillis = F2(
	function (bound, unit) {
		switch (unit.$) {
			case 'MilliSecond':
				return $elm$core$Basics$round(bound);
			case 'Second':
				return $elm$core$Basics$round(bound * 1000);
			case 'Minute':
				return $elm$core$Basics$round(bound * 60000);
			case 'Hour':
				return $elm$core$Basics$round(bound * 3.6e6);
			case 'Day':
				return $elm$core$Basics$round(bound * 8.64e7);
			default:
				return $elm$core$Basics$round(bound * 3.1556952e10);
		}
	});
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Data$estimate = F2(
	function (timeline, query_time) {
		estimate:
		while (true) {
			if (!timeline.b) {
				return {residualMilliSec: 0, value: false};
			} else {
				if (!timeline.b.b) {
					var sample = timeline.a;
					var residual = $elm$time$Time$posixToMillis(query_time) - $elm$time$Time$posixToMillis(sample.time);
					return (residual >= 0) ? {residualMilliSec: residual, value: sample.value} : {residualMilliSec: 0, value: false};
				} else {
					if (!timeline.b.b.b) {
						var sample1 = timeline.a;
						var _v1 = timeline.b;
						var sample2 = _v1.a;
						var residual2 = $elm$time$Time$posixToMillis(query_time) - $elm$time$Time$posixToMillis(sample2.time);
						var residual1 = $elm$time$Time$posixToMillis(query_time) - $elm$time$Time$posixToMillis(sample1.time);
						return (residual1 < 0) ? {residualMilliSec: 0, value: false} : ((residual2 < 0) ? {residualMilliSec: residual1, value: sample1.value} : {residualMilliSec: residual2, value: sample2.value});
					} else {
						var sample1 = timeline.a;
						var _v2 = timeline.b;
						var sample2 = _v2.a;
						var rest = _v2.b;
						var residual2 = $elm$time$Time$posixToMillis(query_time) - $elm$time$Time$posixToMillis(sample2.time);
						var residual1 = $elm$time$Time$posixToMillis(query_time) - $elm$time$Time$posixToMillis(sample1.time);
						if (residual1 < 0) {
							return {residualMilliSec: 0, value: false};
						} else {
							if (residual2 < 0) {
								return {residualMilliSec: residual1, value: sample1.value};
							} else {
								var $temp$timeline = A2($elm$core$List$cons, sample2, rest),
									$temp$query_time = query_time;
								timeline = $temp$timeline;
								query_time = $temp$query_time;
								continue estimate;
							}
						}
					}
				}
			}
		}
	});
var $Chadtech$elm_bool_extra$Bool$Extra$any = $elm$core$List$any($elm$core$Basics$identity);
var $author$project$Formula$evalEventually = function (input) {
	if (!input.b) {
		return _List_Nil;
	} else {
		if (!input.b.b) {
			var b = input.a;
			return _List_fromArray(
				[b]);
		} else {
			var b = input.a;
			var rest = input.b;
			if (b) {
				return A2(
					$elm$core$List$cons,
					true,
					$author$project$Formula$evalEventually(rest));
			} else {
				var eventually = $author$project$Formula$evalEventually(rest);
				return A2(
					$elm$core$List$cons,
					$Chadtech$elm_bool_extra$Bool$Extra$any(eventually),
					eventually);
			}
		}
	}
};
var $Chadtech$elm_bool_extra$Bool$Extra$all = $elm$core$List$all($elm$core$Basics$identity);
var $author$project$Formula$evalGlobally = function (input) {
	if (!input.b) {
		return _List_Nil;
	} else {
		if (!input.b.b) {
			var b = input.a;
			return _List_fromArray(
				[b]);
		} else {
			var b = input.a;
			var rest = input.b;
			if (!b) {
				return A2(
					$elm$core$List$cons,
					false,
					$author$project$Formula$evalGlobally(rest));
			} else {
				var global = $author$project$Formula$evalGlobally(rest);
				return A2(
					$elm$core$List$cons,
					$Chadtech$elm_bool_extra$Bool$Extra$all(global),
					global);
			}
		}
	}
};
var $author$project$Formula$evalUntil = F2(
	function (input1, input2) {
		var _v0 = _Utils_Tuple2(input1, input2);
		if (!_v0.a.b) {
			return _List_Nil;
		} else {
			if (!_v0.b.b) {
				return _List_Nil;
			} else {
				if ((!_v0.a.b.b) && (!_v0.b.b.b)) {
					var _v1 = _v0.a;
					var _v2 = _v0.b;
					var b2 = _v2.a;
					return _List_fromArray(
						[b2]);
				} else {
					var _v3 = _v0.a;
					var b1 = _v3.a;
					var rest1 = _v3.b;
					var _v4 = _v0.b;
					var b2 = _v4.a;
					var rest2 = _v4.b;
					var evalRest = A2($author$project$Formula$evalUntil, rest1, rest2);
					if (b2) {
						return A2($elm$core$List$cons, true, evalRest);
					} else {
						if (!b1) {
							return A2($elm$core$List$cons, false, evalRest);
						} else {
							if (!evalRest.b) {
								return A2($elm$core$List$cons, false, evalRest);
							} else {
								var b = evalRest.a;
								return A2($elm$core$List$cons, b, evalRest);
							}
						}
					}
				}
			}
		}
	});
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
var $author$project$Formula$getFormulaTimeline = function (ef) {
	switch (ef.$) {
		case 'EvaluatedFalsity':
			var timeline = ef.a;
			return timeline;
		case 'EvaluatedTruth':
			var timeline = ef.a;
			return timeline;
		case 'EvaluatedState':
			var timeline = ef.b;
			return timeline;
		case 'EvaluatedNot':
			var timeline = ef.b;
			return timeline;
		case 'EvaluatedAnd':
			var timeline = ef.c;
			return timeline;
		case 'EvaluatedOr':
			var timeline = ef.c;
			return timeline;
		case 'EvaluatedImplies':
			var timeline = ef.c;
			return timeline;
		case 'EvaluatedEventually':
			var timeline = ef.b;
			return timeline;
		case 'EvaluatedGlobally':
			var timeline = ef.b;
			return timeline;
		case 'EvaluatedUntil':
			var timeline = ef.c;
			return timeline;
		case 'EvaluatedBoundedEventually':
			var timeline = ef.d;
			return timeline;
		case 'EvaluatedBoundedGlobally':
			var timeline = ef.d;
			return timeline;
		default:
			var timeline = ef.e;
			return timeline;
	}
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Formula$evaluate = F3(
	function (sim, formula, times) {
		switch (formula.$) {
			case 'Falsity':
				return $author$project$Formula$EvaluatedFalsity(
					A2(
						$elm$core$List$map,
						function (t) {
							return {time: t, value: false};
						},
						times));
			case 'Truth':
				return $author$project$Formula$EvaluatedTruth(
					A2(
						$elm$core$List$map,
						function (t) {
							return {time: t, value: true};
						},
						times));
			case 'State':
				var name = formula.a;
				var stateTimeline = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					A2($elm$core$Dict$get, name, sim));
				return A2(
					$author$project$Formula$EvaluatedState,
					name,
					A2(
						$elm$core$List$map,
						function (t) {
							return {
								time: t,
								value: A2($author$project$Data$estimate, stateTimeline, t).value
							};
						},
						times));
			case 'Not':
				var f = formula.a;
				var eval_f = A3($author$project$Formula$evaluate, sim, f, times);
				return A2(
					$author$project$Formula$EvaluatedNot,
					eval_f,
					A3(
						$elm$core$List$map2,
						F2(
							function (t, v) {
								return {time: t, value: v};
							}),
						times,
						A2(
							$elm$core$List$map,
							$elm$core$Basics$not,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f)))));
			case 'And':
				var f1 = formula.a;
				var f2 = formula.b;
				var eval_f2 = A3($author$project$Formula$evaluate, sim, f2, times);
				var eval_f1 = A3($author$project$Formula$evaluate, sim, f1, times);
				return A3(
					$author$project$Formula$EvaluatedAnd,
					eval_f1,
					eval_f2,
					A3(
						$elm$core$List$map2,
						F2(
							function (t, v) {
								return {time: t, value: v};
							}),
						times,
						A3(
							$elm$core$Basics$apR,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f2)),
							$elm$core$List$map2($elm$core$Basics$and),
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f1)))));
			case 'Or':
				var f1 = formula.a;
				var f2 = formula.b;
				var eval_f2 = A3($author$project$Formula$evaluate, sim, f2, times);
				var eval_f1 = A3($author$project$Formula$evaluate, sim, f1, times);
				return A3(
					$author$project$Formula$EvaluatedOr,
					eval_f1,
					eval_f2,
					A3(
						$elm$core$List$map2,
						F2(
							function (t, v) {
								return {time: t, value: v};
							}),
						times,
						A3(
							$elm$core$Basics$apR,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f2)),
							$elm$core$List$map2($elm$core$Basics$or),
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f1)))));
			case 'Implies':
				var f1 = formula.a;
				var f2 = formula.b;
				var eval_f2 = A3($author$project$Formula$evaluate, sim, f2, times);
				var eval_f1 = A3($author$project$Formula$evaluate, sim, f1, times);
				return A3(
					$author$project$Formula$EvaluatedImplies,
					eval_f1,
					eval_f2,
					A3(
						$elm$core$List$map2,
						F2(
							function (t, v) {
								return {time: t, value: v};
							}),
						times,
						A3(
							$elm$core$Basics$apR,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f1)),
							$elm$core$List$map2(
								F2(
									function (condition, result) {
										return condition ? result : true;
									})),
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f2)))));
			case 'Eventually':
				var f = formula.a;
				var eval_f = A3($author$project$Formula$evaluate, sim, f, times);
				return A2(
					$author$project$Formula$EvaluatedEventually,
					eval_f,
					A3(
						$elm$core$List$map2,
						F2(
							function (t, v) {
								return {time: t, value: v};
							}),
						times,
						$author$project$Formula$evalEventually(
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f)))));
			case 'Globally':
				var f = formula.a;
				var eval_f = A3($author$project$Formula$evaluate, sim, f, times);
				return A2(
					$author$project$Formula$EvaluatedGlobally,
					eval_f,
					A3(
						$elm$core$List$map2,
						F2(
							function (t, v) {
								return {time: t, value: v};
							}),
						times,
						$author$project$Formula$evalGlobally(
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f)))));
			case 'Until':
				var f1 = formula.a;
				var f2 = formula.b;
				var eval_f2 = A3($author$project$Formula$evaluate, sim, f2, times);
				var eval_f1 = A3($author$project$Formula$evaluate, sim, f1, times);
				return A3(
					$author$project$Formula$EvaluatedUntil,
					eval_f1,
					eval_f2,
					A3(
						$elm$core$List$map2,
						F2(
							function (t, v) {
								return {time: t, value: v};
							}),
						times,
						A3(
							$elm$core$Basics$apR,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f1)),
							$author$project$Formula$evalUntil,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.value;
								},
								$author$project$Formula$getFormulaTimeline(eval_f2)))));
			case 'BoundedEventually':
				var f = formula.a;
				var bound = formula.b;
				var units = formula.c;
				var eval_f = A3($author$project$Formula$evaluate, sim, f, times);
				var boundMs = A2($author$project$Formula$boundToMillis, bound, units);
				var boundingFilter = F2(
					function (evaluationPoint, lookAheadPoint) {
						var timeInterval = $elm$time$Time$posixToMillis(lookAheadPoint.time) - $elm$time$Time$posixToMillis(evaluationPoint.time);
						return (timeInterval >= 0) && (_Utils_cmp(timeInterval, boundMs) < 1);
					});
				return A4(
					$author$project$Formula$EvaluatedBoundedEventually,
					eval_f,
					bound,
					units,
					A2(
						$elm$core$List$map,
						function (evaluationPoint) {
							return {
								time: evaluationPoint.time,
								value: A2(
									$elm$core$List$any,
									function ($) {
										return $.value;
									},
									A2(
										$elm$core$List$filter,
										boundingFilter(evaluationPoint),
										$author$project$Formula$getFormulaTimeline(eval_f)))
							};
						},
						$author$project$Formula$getFormulaTimeline(eval_f)));
			case 'BoundedGlobally':
				var f = formula.a;
				var bound = formula.b;
				var units = formula.c;
				var eval_f = A3($author$project$Formula$evaluate, sim, f, times);
				var boundMs = A2($author$project$Formula$boundToMillis, bound, units);
				var boundingFilter = F2(
					function (evaluationPoint, lookAheadPoint) {
						var timeInterval = $elm$time$Time$posixToMillis(lookAheadPoint.time) - $elm$time$Time$posixToMillis(evaluationPoint.time);
						return (timeInterval >= 0) && (_Utils_cmp(timeInterval, boundMs) < 1);
					});
				return A4(
					$author$project$Formula$EvaluatedBoundedGlobally,
					eval_f,
					bound,
					units,
					A2(
						$elm$core$List$map,
						function (evaluationPoint) {
							return {
								time: evaluationPoint.time,
								value: A2(
									$elm$core$List$all,
									function ($) {
										return $.value;
									},
									A2(
										$elm$core$List$filter,
										boundingFilter(evaluationPoint),
										$author$project$Formula$getFormulaTimeline(eval_f)))
							};
						},
						$author$project$Formula$getFormulaTimeline(eval_f)));
			default:
				var f1 = formula.a;
				var f2 = formula.b;
				var bound = formula.c;
				var units = formula.d;
				var eval_f2 = A3($author$project$Formula$evaluate, sim, f2, times);
				var eval_f1 = A3($author$project$Formula$evaluate, sim, f1, times);
				return A5(
					$author$project$Formula$EvaluatedBoundedUntil,
					eval_f1,
					eval_f2,
					bound,
					units,
					$author$project$Formula$getFormulaTimeline(
						A3(
							$author$project$Formula$evaluate,
							sim,
							A2(
								$author$project$Formula$And,
								A2($author$project$Formula$Until, f1, f2),
								A3($author$project$Formula$BoundedEventually, f2, bound, units)),
							times)));
		}
	});
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
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Data$getMaxTime = function (sim) {
	return $elm$time$Time$millisToPosix(
		A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$maximum(
				A2(
					$elm$core$List$map,
					function (l) {
						return A2(
							$elm$core$Maybe$withDefault,
							0,
							$elm$core$List$maximum(
								A2(
									$elm$core$List$map,
									$elm$time$Time$posixToMillis,
									A2(
										$elm$core$List$map,
										function ($) {
											return $.time;
										},
										l))));
					},
					$elm$core$Dict$values(sim)))));
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
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
var $author$project$Data$getMinTime = function (sim) {
	return $elm$time$Time$millisToPosix(
		A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$minimum(
				A2(
					$elm$core$List$map,
					function (l) {
						return A2(
							$elm$core$Maybe$withDefault,
							0,
							$elm$core$List$minimum(
								A2(
									$elm$core$List$map,
									$elm$time$Time$posixToMillis,
									A2(
										$elm$core$List$map,
										function ($) {
											return $.time;
										},
										l))));
					},
					$elm$core$Dict$values(sim)))));
};
var $gampleman$elm_visualization$Zoom$NoTouches = {$: 'NoTouches'};
var $gampleman$elm_visualization$Zoom$Zoom = function (a) {
	return {$: 'Zoom', a: a};
};
var $gampleman$elm_visualization$Zoom$Transform$Transform = F3(
	function (k, x, y) {
		return {k: k, x: x, y: y};
	});
var $gampleman$elm_visualization$Zoom$Transform$identity = A3($gampleman$elm_visualization$Zoom$Transform$Transform, 1.0, 0.0, 0.0);
var $gampleman$elm_visualization$Zoom$infinity = 1 / 0;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $gampleman$elm_visualization$Zoom$init = function (_v0) {
	var width = _v0.width;
	var height = _v0.height;
	return $gampleman$elm_visualization$Zoom$Zoom(
		{
			drag: $elm$core$Maybe$Nothing,
			extent: _Utils_Tuple2(
				_Utils_Tuple2(0, 0),
				_Utils_Tuple2(width, height)),
			gestureLastScale: 0,
			scaleExtent: _Utils_Tuple2(0, $gampleman$elm_visualization$Zoom$infinity),
			touchStarted: false,
			touches: $gampleman$elm_visualization$Zoom$NoTouches,
			transform: $gampleman$elm_visualization$Zoom$Transform$identity,
			transition: $elm$core$Maybe$Nothing,
			translateExtent: _Utils_Tuple2(
				_Utils_Tuple2(-$gampleman$elm_visualization$Zoom$infinity, -$gampleman$elm_visualization$Zoom$infinity),
				_Utils_Tuple2($gampleman$elm_visualization$Zoom$infinity, $gampleman$elm_visualization$Zoom$infinity))
		});
};
var $author$project$Formula$Falsity = {$: 'Falsity'};
var $author$project$Main$initial_input = 'TRUE';
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
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
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
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
var $author$project$Formula$BoundedGlobally = F3(
	function (a, b, c) {
		return {$: 'BoundedGlobally', a: a, b: b, c: c};
	});
var $author$project$Formula$BoundedUntil = F4(
	function (a, b, c, d) {
		return {$: 'BoundedUntil', a: a, b: b, c: c, d: d};
	});
var $author$project$Formula$Eventually = function (a) {
	return {$: 'Eventually', a: a};
};
var $author$project$Formula$Globally = function (a) {
	return {$: 'Globally', a: a};
};
var $author$project$Formula$Implies = F2(
	function (a, b) {
		return {$: 'Implies', a: a, b: b};
	});
var $author$project$Formula$Not = function (a) {
	return {$: 'Not', a: a};
};
var $author$project$Formula$Or = F2(
	function (a, b) {
		return {$: 'Or', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					var _v2 = callback(a);
					var parseB = _v2.a;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0.a;
	return $elm$parser$Parser$Advanced$Parser(
		function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 'Bad') {
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, false, x);
			} else {
				var a = _v1.b;
				var s1 = _v1.c;
				return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
			}
		});
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $author$project$Formula$Truth = {$: 'Truth'};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
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
var $elm$parser$Parser$ExpectingKeyword = function (a) {
	return {$: 'ExpectingKeyword', a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$keyword = function (_v0) {
	var kwd = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(kwd);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, kwd, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return (_Utils_eq(newOffset, -1) || (0 <= A3(
				$elm$parser$Parser$Advanced$isSubChar,
				function (c) {
					return $elm$core$Char$isAlphaNum(c) || _Utils_eq(
						c,
						_Utils_chr('_'));
				},
				newOffset,
				s.src))) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$keyword = function (kwd) {
	return $elm$parser$Parser$Advanced$keyword(
		A2(
			$elm$parser$Parser$Advanced$Token,
			kwd,
			$elm$parser$Parser$ExpectingKeyword(kwd)));
};
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
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $author$project$Formula$booleanLiteral = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$Formula$Truth),
			$elm$parser$Parser$keyword('TRUE')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$Formula$Falsity),
			$elm$parser$Parser$keyword('FALSE'))
		]));
var $author$project$Formula$State = function (a) {
	return {$: 'State', a: a};
};
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
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
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
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
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
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
var $author$project$Formula$categoricalComparison = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed(
			F2(
				function (name, val) {
					return $author$project$Formula$State(
						name + ('__' + A3($elm$core$String$replace, ' ', '_', val)));
				})),
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$variable(
								{
									inner: function (c) {
										return $elm$core$Char$isAlphaNum(c) || _Utils_eq(
											c,
											_Utils_chr('_'));
									},
									reserved: $elm$core$Set$fromList(
										_List_fromArray(
											['and', 'or', 'f', 'g', 'not', 'implies', 'if', 'then'])),
									start: $elm$core$Char$isLower
								}),
							$elm$parser$Parser$spaces),
						$elm$parser$Parser$keyword('IS')),
					$elm$parser$Parser$spaces),
				$elm$parser$Parser$symbol('\"')),
			$elm$parser$Parser$spaces)),
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$variable(
			{
				inner: function (c) {
					return !_Utils_eq(
						c,
						_Utils_chr('\"'));
				},
				reserved: $elm$core$Set$fromList(_List_Nil),
				start: function (c) {
					return !_Utils_eq(
						c,
						_Utils_chr('\"'));
				}
			}),
		$elm$parser$Parser$symbol('\"')));
var $elm$parser$Parser$ExpectingFloat = {$: 'ExpectingFloat'};
var $elm$parser$Parser$Advanced$consumeBase = _Parser_consumeBase;
var $elm$parser$Parser$Advanced$consumeBase16 = _Parser_consumeBase16;
var $elm$parser$Parser$Advanced$bumpOffset = F2(
	function (newOffset, s) {
		return {col: s.col + (newOffset - s.offset), context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src};
	});
var $elm$parser$Parser$Advanced$chompBase10 = _Parser_chompBase10;
var $elm$parser$Parser$Advanced$isAsciiCode = _Parser_isAsciiCode;
var $elm$parser$Parser$Advanced$consumeExp = F2(
	function (offset, src) {
		if (A3($elm$parser$Parser$Advanced$isAsciiCode, 101, offset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 69, offset, src)) {
			var eOffset = offset + 1;
			var expOffset = (A3($elm$parser$Parser$Advanced$isAsciiCode, 43, eOffset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 45, eOffset, src)) ? (eOffset + 1) : eOffset;
			var newOffset = A2($elm$parser$Parser$Advanced$chompBase10, expOffset, src);
			return _Utils_eq(expOffset, newOffset) ? (-newOffset) : newOffset;
		} else {
			return offset;
		}
	});
var $elm$parser$Parser$Advanced$consumeDotAndExp = F2(
	function (offset, src) {
		return A3($elm$parser$Parser$Advanced$isAsciiCode, 46, offset, src) ? A2(
			$elm$parser$Parser$Advanced$consumeExp,
			A2($elm$parser$Parser$Advanced$chompBase10, offset + 1, src),
			src) : A2($elm$parser$Parser$Advanced$consumeExp, offset, src);
	});
var $elm$parser$Parser$Advanced$finalizeInt = F5(
	function (invalid, handler, startOffset, _v0, s) {
		var endOffset = _v0.a;
		var n = _v0.b;
		if (handler.$ === 'Err') {
			var x = handler.a;
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		} else {
			var toValue = handler.a;
			return _Utils_eq(startOffset, endOffset) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				_Utils_cmp(s.offset, startOffset) < 0,
				A2($elm$parser$Parser$Advanced$fromState, s, invalid)) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				toValue(n),
				A2($elm$parser$Parser$Advanced$bumpOffset, endOffset, s));
		}
	});
var $elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $elm$parser$Parser$Advanced$finalizeFloat = F6(
	function (invalid, expecting, intSettings, floatSettings, intPair, s) {
		var intOffset = intPair.a;
		var floatOffset = A2($elm$parser$Parser$Advanced$consumeDotAndExp, intOffset, s.src);
		if (floatOffset < 0) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A4($elm$parser$Parser$Advanced$fromInfo, s.row, s.col - (floatOffset + s.offset), invalid, s.context));
		} else {
			if (_Utils_eq(s.offset, floatOffset)) {
				return A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting));
			} else {
				if (_Utils_eq(intOffset, floatOffset)) {
					return A5($elm$parser$Parser$Advanced$finalizeInt, invalid, intSettings, s.offset, intPair, s);
				} else {
					if (floatSettings.$ === 'Err') {
						var x = floatSettings.a;
						return A2(
							$elm$parser$Parser$Advanced$Bad,
							true,
							A2($elm$parser$Parser$Advanced$fromState, s, invalid));
					} else {
						var toValue = floatSettings.a;
						var _v1 = $elm$core$String$toFloat(
							A3($elm$core$String$slice, s.offset, floatOffset, s.src));
						if (_v1.$ === 'Nothing') {
							return A2(
								$elm$parser$Parser$Advanced$Bad,
								true,
								A2($elm$parser$Parser$Advanced$fromState, s, invalid));
						} else {
							var n = _v1.a;
							return A3(
								$elm$parser$Parser$Advanced$Good,
								true,
								toValue(n),
								A2($elm$parser$Parser$Advanced$bumpOffset, floatOffset, s));
						}
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$number = function (c) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			if (A3($elm$parser$Parser$Advanced$isAsciiCode, 48, s.offset, s.src)) {
				var zeroOffset = s.offset + 1;
				var baseOffset = zeroOffset + 1;
				return A3($elm$parser$Parser$Advanced$isAsciiCode, 120, zeroOffset, s.src) ? A5(
					$elm$parser$Parser$Advanced$finalizeInt,
					c.invalid,
					c.hex,
					baseOffset,
					A2($elm$parser$Parser$Advanced$consumeBase16, baseOffset, s.src),
					s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 111, zeroOffset, s.src) ? A5(
					$elm$parser$Parser$Advanced$finalizeInt,
					c.invalid,
					c.octal,
					baseOffset,
					A3($elm$parser$Parser$Advanced$consumeBase, 8, baseOffset, s.src),
					s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 98, zeroOffset, s.src) ? A5(
					$elm$parser$Parser$Advanced$finalizeInt,
					c.invalid,
					c.binary,
					baseOffset,
					A3($elm$parser$Parser$Advanced$consumeBase, 2, baseOffset, s.src),
					s) : A6(
					$elm$parser$Parser$Advanced$finalizeFloat,
					c.invalid,
					c.expecting,
					c._int,
					c._float,
					_Utils_Tuple2(zeroOffset, 0),
					s)));
			} else {
				return A6(
					$elm$parser$Parser$Advanced$finalizeFloat,
					c.invalid,
					c.expecting,
					c._int,
					c._float,
					A3($elm$parser$Parser$Advanced$consumeBase, 10, s.offset, s.src),
					s);
			}
		});
};
var $elm$parser$Parser$Advanced$float = F2(
	function (expecting, invalid) {
		return $elm$parser$Parser$Advanced$number(
			{
				binary: $elm$core$Result$Err(invalid),
				expecting: expecting,
				_float: $elm$core$Result$Ok($elm$core$Basics$identity),
				hex: $elm$core$Result$Err(invalid),
				_int: $elm$core$Result$Ok($elm$core$Basics$toFloat),
				invalid: invalid,
				octal: $elm$core$Result$Err(invalid)
			});
	});
var $elm$parser$Parser$float = A2($elm$parser$Parser$Advanced$float, $elm$parser$Parser$ExpectingFloat, $elm$parser$Parser$ExpectingFloat);
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Formula$numericComparison = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F4(
						function (name, op, val, unit) {
							return $author$project$Formula$State(
								name + ('__' + (op + ('__' + ($elm$core$String$fromFloat(val) + ('__' + unit))))));
						})),
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$variable(
						{
							inner: function (c) {
								return $elm$core$Char$isAlphaNum(c) || _Utils_eq(
									c,
									_Utils_chr('_'));
							},
							reserved: $elm$core$Set$fromList(
								_List_fromArray(
									['and', 'or', 'f', 'g', 'not', 'implies', 'if', 'then'])),
							start: $elm$core$Char$isLower
						}),
					$elm$parser$Parser$spaces)),
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed('>'),
							$elm$parser$Parser$keyword('>')),
							A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed('>='),
							$elm$parser$Parser$keyword('>=')),
							A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed('<'),
							$elm$parser$Parser$keyword('<')),
							A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed('<='),
							$elm$parser$Parser$keyword('<=')),
							A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed('=='),
							$elm$parser$Parser$keyword('==')),
							A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed('!='),
							$elm$parser$Parser$keyword('!='))
						])),
				$elm$parser$Parser$spaces)),
		A2($elm$parser$Parser$ignorer, $elm$parser$Parser$float, $elm$parser$Parser$spaces)),
	$elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed('AU'),
				$elm$parser$Parser$keyword('AU')),
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed('DEG'),
				$elm$parser$Parser$keyword('DEG')),
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed('AMP'),
				$elm$parser$Parser$keyword('AMP'))
			])));
var $author$project$Formula$stateName = A2(
	$elm$parser$Parser$keeper,
	$elm$parser$Parser$succeed($author$project$Formula$State),
	$elm$parser$Parser$variable(
		{
			inner: function (c) {
				return $elm$core$Char$isAlphaNum(c) || _Utils_eq(
					c,
					_Utils_chr('_'));
			},
			reserved: $elm$core$Set$fromList(
				_List_fromArray(
					['and', 'or', 'f', 'g', 'not', 'implies', 'if', 'then'])),
			start: $elm$core$Char$isLower
		}));
var $author$project$Formula$atom = A2(
	$elm$parser$Parser$keeper,
	$elm$parser$Parser$succeed($elm$core$Basics$identity),
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$backtrackable($author$project$Formula$numericComparison),
					$elm$parser$Parser$backtrackable($author$project$Formula$categoricalComparison),
					$author$project$Formula$stateName,
					$author$project$Formula$booleanLiteral
				])),
		$elm$parser$Parser$spaces));
var $elm$parser$Parser$Advanced$lazy = function (thunk) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v0 = thunk(_Utils_Tuple0);
			var parse = _v0.a;
			return parse(s);
		});
};
var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
var $author$project$Formula$Day = {$: 'Day'};
var $author$project$Formula$Hour = {$: 'Hour'};
var $author$project$Formula$MilliSecond = {$: 'MilliSecond'};
var $author$project$Formula$Second = {$: 'Second'};
var $author$project$Formula$Year = {$: 'Year'};
var $author$project$Formula$timeUnit = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$Formula$MilliSecond),
			$elm$parser$Parser$keyword('MS')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$Formula$Second),
			$elm$parser$Parser$keyword('S')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$Formula$Hour),
			$elm$parser$Parser$keyword('H')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$Formula$Day),
			$elm$parser$Parser$keyword('D')),
			A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed($author$project$Formula$Year),
			$elm$parser$Parser$keyword('Y'))
		]));
function $author$project$Formula$cyclic$expression() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$elm$parser$Parser$backtrackable(
				$author$project$Formula$cyclic$infixOp()),
				$author$project$Formula$cyclic$term()
			]));
}
function $author$project$Formula$cyclic$term() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$author$project$Formula$atom,
				$author$project$Formula$cyclic$grouping(),
				$author$project$Formula$cyclic$prefixOp()
			]));
}
function $author$project$Formula$cyclic$grouping() {
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$symbol('(')),
			$elm$parser$Parser$spaces),
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$lazy(
						function (_v13) {
							return $author$project$Formula$cyclic$expression();
						}),
					$elm$parser$Parser$spaces),
				$elm$parser$Parser$symbol(')')),
			$elm$parser$Parser$spaces));
}
function $author$project$Formula$cyclic$infixOp() {
	return A2(
		$elm$parser$Parser$andThen,
		function (leftTerm) {
			return $elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									$author$project$Formula$Implies(leftTerm)),
								$elm$parser$Parser$oneOf(
									_List_fromArray(
										[
											$elm$parser$Parser$keyword('->'),
											$elm$parser$Parser$keyword('IMPLIES')
										]))),
							$elm$parser$Parser$spaces),
						$elm$parser$Parser$lazy(
							function (_v8) {
								return $author$project$Formula$cyclic$expression();
							})),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									$author$project$Formula$And(leftTerm)),
								$elm$parser$Parser$keyword('AND')),
							$elm$parser$Parser$spaces),
						$elm$parser$Parser$lazy(
							function (_v9) {
								return $author$project$Formula$cyclic$expression();
							})),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									$author$project$Formula$Or(leftTerm)),
								$elm$parser$Parser$keyword('OR')),
							$elm$parser$Parser$spaces),
						$elm$parser$Parser$lazy(
							function (_v10) {
								return $author$project$Formula$cyclic$expression();
							})),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									$author$project$Formula$Until(leftTerm)),
								$elm$parser$Parser$keyword('UNTIL')),
							$elm$parser$Parser$spaces),
						$elm$parser$Parser$lazy(
							function (_v11) {
								return $author$project$Formula$cyclic$expression();
							})),
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed(
											F3(
												function (t, u, f) {
													return A4($author$project$Formula$BoundedUntil, leftTerm, f, t, u);
												})),
										$elm$parser$Parser$keyword('HOLDS UNTIL WITHIN')),
									$elm$parser$Parser$spaces),
								A2($elm$parser$Parser$ignorer, $elm$parser$Parser$float, $elm$parser$Parser$spaces)),
							A2($elm$parser$Parser$ignorer, $author$project$Formula$timeUnit, $elm$parser$Parser$spaces)),
						$elm$parser$Parser$lazy(
							function (_v12) {
								return $author$project$Formula$cyclic$expression();
							}))
					]));
		},
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($elm$core$Basics$identity),
			$elm$parser$Parser$lazy(
				function (_v7) {
					return $author$project$Formula$cyclic$term();
				})));
}
function $author$project$Formula$cyclic$prefixOp() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($author$project$Formula$Not),
						$elm$parser$Parser$keyword('NOT')),
					$elm$parser$Parser$spaces),
				$elm$parser$Parser$lazy(
					function (_v0) {
						return $author$project$Formula$cyclic$expression();
					})),
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($author$project$Formula$Eventually),
						$elm$parser$Parser$keyword('EVENTUALLY')),
					$elm$parser$Parser$spaces),
				$elm$parser$Parser$lazy(
					function (_v1) {
						return $author$project$Formula$cyclic$expression();
					})),
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($author$project$Formula$Globally),
						$elm$parser$Parser$keyword('GLOBALLY')),
					$elm$parser$Parser$spaces),
				$elm$parser$Parser$lazy(
					function (_v2) {
						return $author$project$Formula$cyclic$expression();
					})),
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									F3(
										function (t, u, f) {
											return A3($author$project$Formula$BoundedEventually, f, t, u);
										})),
								$elm$parser$Parser$keyword('WITHIN')),
							$elm$parser$Parser$spaces),
						A2($elm$parser$Parser$ignorer, $elm$parser$Parser$float, $elm$parser$Parser$spaces)),
					A2($elm$parser$Parser$ignorer, $author$project$Formula$timeUnit, $elm$parser$Parser$spaces)),
				$elm$parser$Parser$lazy(
					function (_v3) {
						return $author$project$Formula$cyclic$expression();
					})),
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									F3(
										function (t, u, f) {
											return A3($author$project$Formula$BoundedGlobally, f, t, u);
										})),
								$elm$parser$Parser$keyword('FOR')),
							$elm$parser$Parser$spaces),
						A2($elm$parser$Parser$ignorer, $elm$parser$Parser$float, $elm$parser$Parser$spaces)),
					A2($elm$parser$Parser$ignorer, $author$project$Formula$timeUnit, $elm$parser$Parser$spaces)),
				$elm$parser$Parser$lazy(
					function (_v4) {
						return $author$project$Formula$cyclic$expression();
					})),
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed($author$project$Formula$Implies),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										$elm$parser$Parser$keyword('IF'),
										$elm$parser$Parser$keyword('WHEN')
									]))),
						$elm$parser$Parser$spaces),
					A2(
						$elm$parser$Parser$ignorer,
						A2(
							$elm$parser$Parser$ignorer,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$lazy(
									function (_v5) {
										return $author$project$Formula$cyclic$expression();
									}),
								$elm$parser$Parser$spaces),
							$elm$parser$Parser$keyword('THEN')),
						$elm$parser$Parser$spaces)),
				$elm$parser$Parser$lazy(
					function (_v6) {
						return $author$project$Formula$cyclic$expression();
					}))
			]));
}
try {
	var $author$project$Formula$expression = $author$project$Formula$cyclic$expression();
	$author$project$Formula$cyclic$expression = function () {
		return $author$project$Formula$expression;
	};
	var $author$project$Formula$term = $author$project$Formula$cyclic$term();
	$author$project$Formula$cyclic$term = function () {
		return $author$project$Formula$term;
	};
	var $author$project$Formula$grouping = $author$project$Formula$cyclic$grouping();
	$author$project$Formula$cyclic$grouping = function () {
		return $author$project$Formula$grouping;
	};
	var $author$project$Formula$infixOp = $author$project$Formula$cyclic$infixOp();
	$author$project$Formula$cyclic$infixOp = function () {
		return $author$project$Formula$infixOp;
	};
	var $author$project$Formula$prefixOp = $author$project$Formula$cyclic$prefixOp();
	$author$project$Formula$cyclic$prefixOp = function () {
		return $author$project$Formula$prefixOp;
	};
} catch ($) {
	throw 'Some top-level definitions from `Formula` are causing infinite recursion:\n\n  ┌─────┐\n  │    expression\n  │     ↓\n  │    term\n  │     ↓\n  │    grouping\n  │     ↓\n  │    infixOp\n  │     ↓\n  │    prefixOp\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $author$project$Formula$fullFormula = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$spaces),
	A2($elm$parser$Parser$ignorer, $author$project$Formula$expression, $elm$parser$Parser$end));
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
var $author$project$Formula$parse = function (string) {
	return A2($elm$parser$Parser$run, $author$project$Formula$fullFormula, string);
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Main$initial_formula = A2(
	$elm$core$Result$withDefault,
	$author$project$Formula$Falsity,
	$author$project$Formula$parse($author$project$Main$initial_input));
var $author$project$Main$initial_metadata = {cognizantEngineer: '', description: '', flightRuleID: '', flightRuleTitle: '', level: '', operationalCategory: '', rationale: '', violationImpact: ''};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $gampleman$elm_visualization$Zoom$scaleExtent = F3(
	function (mn, mx, _v0) {
		var zoom = _v0.a;
		return $gampleman$elm_visualization$Zoom$Zoom(
			_Utils_update(
				zoom,
				{
					scaleExtent: _Utils_Tuple2(mn, mx)
				}));
	});
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		{
			currentTime: $author$project$Data$getMinTime($author$project$Data$data),
			evalRule: A3(
				$author$project$Formula$evaluate,
				$author$project$Data$data,
				$author$project$Main$initial_formula,
				$author$project$Timeline$evalTimes(
					{
						maxTime: $author$project$Data$getMaxTime($author$project$Data$data),
						minTime: $author$project$Data$getMinTime($author$project$Data$data)
					})),
			helixData: $author$project$Main$initial_metadata,
			input: $author$project$Main$initial_input,
			maxTime: $author$project$Data$getMaxTime($author$project$Data$data),
			minTime: $author$project$Data$getMinTime($author$project$Data$data),
			output: $author$project$Main$initial_formula,
			parseError: $elm$core$Maybe$Nothing,
			showLegend: false,
			simulation: $author$project$Data$data,
			viewMode: $author$project$Model$Authoring,
			zoom: A3(
				$gampleman$elm_visualization$Zoom$scaleExtent,
				0.3,
				10,
				$gampleman$elm_visualization$Zoom$init(
					{height: $author$project$Main$canvasHeight, width: $author$project$Main$canvasWidth}))
		},
		$elm$core$Platform$Cmd$none);
};
var $author$project$Model$ZoomMsg = function (a) {
	return {$: 'ZoomMsg', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $gampleman$elm_visualization$Zoom$MouseMove = function (a) {
	return {$: 'MouseMove', a: a};
};
var $gampleman$elm_visualization$Zoom$MouseUp = {$: 'MouseUp'};
var $gampleman$elm_visualization$Zoom$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $gampleman$elm_visualization$Zoom$TouchStartedExpired = {$: 'TouchStartedExpired'};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
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
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
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
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrameDelta = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Delta(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrameDelta = $elm$browser$Browser$AnimationManager$onAnimationFrameDelta;
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
var $elm$browser$Browser$Events$onMouseMove = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousemove');
var $elm$browser$Browser$Events$onMouseUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mouseup');
var $gampleman$elm_visualization$Zoom$touchDelay = 500;
var $gampleman$elm_visualization$Zoom$subscriptions = F2(
	function (_v0, tagger) {
		var zoom = _v0.a;
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					function () {
					var _v1 = zoom.drag;
					if (_v1.$ === 'Just') {
						return $elm$core$Platform$Sub$batch(
							_List_fromArray(
								[
									$elm$browser$Browser$Events$onMouseMove(
									A3(
										$elm$json$Json$Decode$map2,
										F2(
											function (x, y) {
												return tagger(
													$gampleman$elm_visualization$Zoom$MouseMove(
														_Utils_Tuple2(x, y)));
											}),
										A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
										A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float))),
									$elm$browser$Browser$Events$onMouseUp(
									$elm$json$Json$Decode$succeed(
										tagger($gampleman$elm_visualization$Zoom$MouseUp)))
								]));
					} else {
						return $elm$core$Platform$Sub$none;
					}
				}(),
					zoom.touchStarted ? A2(
					$elm$time$Time$every,
					$gampleman$elm_visualization$Zoom$touchDelay,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Basics$always($gampleman$elm_visualization$Zoom$TouchStartedExpired),
						tagger)) : $elm$core$Platform$Sub$none,
					A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Platform$Sub$none,
					A2(
						$elm$core$Maybe$map,
						$elm$core$Basics$always(
							$elm$browser$Browser$Events$onAnimationFrameDelta(
								A2($elm$core$Basics$composeR, $gampleman$elm_visualization$Zoom$Tick, tagger))),
						zoom.transition))
				]));
	});
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2($gampleman$elm_visualization$Zoom$subscriptions, model.zoom, $author$project$Model$ZoomMsg)
			]));
};
var $author$project$Model$Parse = {$: 'Parse'};
var $author$project$Model$RuleFileLoaded = function (a) {
	return {$: 'RuleFileLoaded', a: a};
};
var $author$project$Model$RuleFileSelected = function (a) {
	return {$: 'RuleFileSelected', a: a};
};
var $author$project$Model$SimFileLoaded = function (a) {
	return {$: 'SimFileLoaded', a: a};
};
var $author$project$Model$SimFileSelected = function (a) {
	return {$: 'SimFileSelected', a: a};
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$json$Json$Decode$map8 = _Json_map8;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Data$decodeRule = function (json) {
	return A2(
		$elm$json$Json$Decode$decodeString,
		A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (s, h) {
					return {helixData: h, ruleText: s};
				}),
			A2($elm$json$Json$Decode$field, 'LogicSpec', $elm$json$Json$Decode$string),
			A9(
				$elm$json$Json$Decode$map8,
				F8(
					function (s1, s2, s3, s4, s5, s6, s7, s8) {
						return {cognizantEngineer: s3, description: s6, flightRuleID: s4, flightRuleTitle: s5, level: s2, operationalCategory: s1, rationale: s7, violationImpact: s8};
					}),
				A2($elm$json$Json$Decode$field, 'Operational category', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'Level', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'Cognizant engineer', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'Flight Rule ID', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'Flight rule title', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'Description', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'Rationale', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'Violation impact', $elm$json$Json$Decode$string))),
		json);
};
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Data$decodeSimulation = function (json) {
	return A2(
		$elm$json$Json$Decode$decodeString,
		$elm$json$Json$Decode$dict(
			$elm$json$Json$Decode$list(
				A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (t, v) {
							return {
								time: $elm$time$Time$millisToPosix(t),
								value: v
							};
						}),
					A2($elm$json$Json$Decode$field, 'Time', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'Value', $elm$json$Json$Decode$bool)))),
		json);
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
var $author$project$Data$encodeRule = F2(
	function (ruleText, helixData) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'Operational category',
					$elm$json$Json$Encode$string(helixData.operationalCategory)),
					_Utils_Tuple2(
					'Level',
					$elm$json$Json$Encode$string(helixData.level)),
					_Utils_Tuple2(
					'Cognizant engineer',
					$elm$json$Json$Encode$string(helixData.cognizantEngineer)),
					_Utils_Tuple2(
					'Flight Rule ID',
					$elm$json$Json$Encode$string(helixData.flightRuleID)),
					_Utils_Tuple2(
					'Flight rule title',
					$elm$json$Json$Encode$string(helixData.flightRuleTitle)),
					_Utils_Tuple2(
					'Description',
					$elm$json$Json$Encode$string(helixData.description)),
					_Utils_Tuple2(
					'Rationale',
					$elm$json$Json$Encode$string(helixData.rationale)),
					_Utils_Tuple2(
					'Violation impact',
					$elm$json$Json$Encode$string(helixData.violationImpact)),
					_Utils_Tuple2(
					'LogicSpec',
					$elm$json$Json$Encode$string(ruleText))
				]));
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(_Utils_Tuple0),
				dictionary));
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Data$encodeSimulation = function (simulation) {
	return A3(
		$elm$json$Json$Encode$dict,
		$elm$core$Basics$identity,
		function (timeline) {
			return A2(
				$elm$json$Json$Encode$list,
				function (p) {
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'Time',
								$elm$json$Json$Encode$int(
									$elm$time$Time$posixToMillis(p.time))),
								_Utils_Tuple2(
								'Value',
								$elm$json$Json$Encode$bool(p.value))
							]));
				},
				timeline);
		},
		simulation);
};
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var $elm$file$File$toString = _File_toString;
var $gampleman$elm_visualization$Zoom$OneFinger = function (a) {
	return {$: 'OneFinger', a: a};
};
var $gampleman$elm_visualization$Zoom$TwoFingers = F2(
	function (a, b) {
		return {$: 'TwoFingers', a: a, b: b};
	});
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $gampleman$elm_visualization$Zoom$Transform$invert = F2(
	function (_v0, _v1) {
		var locX = _v0.a;
		var locY = _v0.b;
		var k = _v1.k;
		var x = _v1.x;
		var y = _v1.y;
		return _Utils_Tuple2((locX - x) / k, (locY - y) / k);
	});
var $gampleman$elm_visualization$Zoom$Transform$translate = F2(
	function (_v0, _v1) {
		var locX = _v0.a;
		var locY = _v0.b;
		var k = _v1.k;
		var x = _v1.x;
		var y = _v1.y;
		return A3($gampleman$elm_visualization$Zoom$Transform$Transform, k, x + (k * locX), y + (k * locY));
	});
var $gampleman$elm_visualization$Zoom$constrain = F3(
	function (_v0, _v1, tsfm) {
		var extentT = _v0.a;
		var extentB = _v0.b;
		var translateExtentT = _v1.a;
		var translateExtentB = _v1.b;
		var minus = F2(
			function (_v5, _v6) {
				var a = _v5.a;
				var b = _v5.b;
				var c = _v6.a;
				var d = _v6.b;
				return _Utils_Tuple2(c - a, d - b);
			});
		var _v2 = A2(
			minus,
			translateExtentB,
			A2($gampleman$elm_visualization$Zoom$Transform$invert, extentB, tsfm));
		var dx1 = _v2.a;
		var dy1 = _v2.b;
		var _v3 = A2(
			minus,
			translateExtentT,
			A2($gampleman$elm_visualization$Zoom$Transform$invert, extentT, tsfm));
		var dx0 = _v3.a;
		var dy0 = _v3.b;
		var _v4 = _Utils_Tuple2(
			A2($elm$core$Basics$min, 0, dx0),
			A2($elm$core$Basics$min, 0, dy0));
		var minDX = _v4.a;
		var minDY = _v4.b;
		return A2(
			$gampleman$elm_visualization$Zoom$Transform$translate,
			_Utils_Tuple2(
				(_Utils_cmp(dx1, dx0) > 0) ? ((dx0 + dx1) / 2) : ((!minDX) ? A2($elm$core$Basics$max, 0, dx1) : minDX),
				(_Utils_cmp(dy1, dy0) > 0) ? ((dy0 + dy1) / 2) : ((!minDY) ? A2($elm$core$Basics$max, 0, dy1) : minDY)),
			tsfm);
	});
var $elm$core$Basics$pow = _Basics_pow;
var $gampleman$elm_visualization$Zoom$dist2 = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return A2($elm$core$Basics$pow, x2 - x1, 2) + A2($elm$core$Basics$pow, y2 - y1, 2);
	});
var $gampleman$elm_visualization$Zoom$easingInOutCubic = function (t) {
	return (t < 0.5) ? (A2($elm$core$Basics$pow, t * 2, 3) / 2) : (1 - (0.5 * A2($elm$core$Basics$pow, ((-2) * t) + 2, 3)));
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $gampleman$elm_visualization$Zoom$findAssociatedTouch = function (_v0) {
	var identifier = _v0.identifier;
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$filter(
			function (t) {
				return _Utils_eq(t.identifier, identifier);
			}),
		$elm$core$List$head);
};
var $gampleman$elm_visualization$Zoom$midpoint = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return _Utils_Tuple2((x1 + x2) / 2, (y1 + y2) / 2);
	});
var $gampleman$elm_visualization$Zoom$Matrix$transform = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v0.b;
		var _v2 = _v1.a;
		var a11 = _v2.a;
		var a12 = _v2.b;
		var a13 = _v2.c;
		var _v3 = _v1.b;
		var a21 = _v3.a;
		var a22 = _v3.b;
		var a23 = _v3.c;
		return _Utils_Tuple2(((a11 * x) + (a12 * y)) + (a13 * 1), ((a21 * x) + (a22 * y)) + (a23 * 1));
	});
var $gampleman$elm_visualization$Zoom$normalizePointerPosition = F2(
	function (position, maybeMatrix) {
		if (maybeMatrix.$ === 'Just') {
			var matrix = maybeMatrix.a;
			return A2($gampleman$elm_visualization$Zoom$Matrix$transform, position, matrix);
		} else {
			return position;
		}
	});
var $gampleman$elm_visualization$Zoom$scale = F3(
	function (_v0, k, trfm) {
		var mi = _v0.a;
		var mx = _v0.b;
		return _Utils_update(
			trfm,
			{
				k: A3($elm$core$Basics$clamp, mi, mx, k)
			});
	});
var $gampleman$elm_visualization$Zoom$Transform$scale = F2(
	function (k_, _v0) {
		var k = _v0.k;
		var x = _v0.x;
		var y = _v0.y;
		return A3($gampleman$elm_visualization$Zoom$Transform$Transform, k * k_, x, y);
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Basics$e = _Basics_e;
var $gampleman$elm_visualization$Zoom$Interpolation$exp = function (x) {
	return A2($elm$core$Basics$pow, $elm$core$Basics$e, x);
};
var $gampleman$elm_visualization$Zoom$Interpolation$cosh = function (x) {
	var x_ = $gampleman$elm_visualization$Zoom$Interpolation$exp(x);
	return (x_ + (1 / x_)) / 2;
};
var $gampleman$elm_visualization$Zoom$Interpolation$log = $elm$core$Basics$logBase($elm$core$Basics$e);
var $gampleman$elm_visualization$Zoom$Interpolation$sinh = function (x) {
	var x_ = $gampleman$elm_visualization$Zoom$Interpolation$exp(x);
	return (x_ - (1 / x_)) / 2;
};
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $gampleman$elm_visualization$Zoom$Interpolation$tanh = function (x) {
	var x_ = $gampleman$elm_visualization$Zoom$Interpolation$exp(2 * x);
	return (x_ - 1) / (x_ + 1);
};
var $gampleman$elm_visualization$Zoom$Interpolation$ε2 = 1.0e-12;
var $gampleman$elm_visualization$Zoom$Interpolation$ρ = $elm$core$Basics$sqrt(2);
var $gampleman$elm_visualization$Zoom$Interpolation$interpolate = F2(
	function (a, b) {
		var dy = b.cy - a.cy;
		var dx = b.cx - a.cx;
		var d2 = A2($elm$core$Basics$pow, dx, 2) + A2($elm$core$Basics$pow, dy, 2);
		if (_Utils_cmp(d2, $gampleman$elm_visualization$Zoom$Interpolation$ε2) < 0) {
			var s = $gampleman$elm_visualization$Zoom$Interpolation$log(b.size / a.size) / $gampleman$elm_visualization$Zoom$Interpolation$ρ;
			return _Utils_Tuple2(
				$elm$core$Basics$abs(s) * 1000,
				function (t) {
					return {
						cx: a.cx + (t * dx),
						cy: a.cy + (t * dy),
						size: a.size * $gampleman$elm_visualization$Zoom$Interpolation$exp(($gampleman$elm_visualization$Zoom$Interpolation$ρ * t) * s)
					};
				});
		} else {
			var d1 = $elm$core$Basics$sqrt(d2);
			var b1 = ((A2($elm$core$Basics$pow, b.size, 2) - A2($elm$core$Basics$pow, a.size, 2)) - (A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$ρ, 4) * d2)) / (((2 * b.size) * A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$ρ, 2)) * d1);
			var r1 = $gampleman$elm_visualization$Zoom$Interpolation$log(
				$elm$core$Basics$sqrt(
					A2($elm$core$Basics$pow, b1, 2) + 1) - b1);
			var b0 = ((A2($elm$core$Basics$pow, b.size, 2) - A2($elm$core$Basics$pow, a.size, 2)) + (A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$ρ, 4) * d2)) / (((2 * a.size) * A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$ρ, 2)) * d1);
			var r0 = $gampleman$elm_visualization$Zoom$Interpolation$log(
				$elm$core$Basics$sqrt(
					A2($elm$core$Basics$pow, b0, 2) + 1) - b0);
			var s_ = (r1 - r0) / $gampleman$elm_visualization$Zoom$Interpolation$ρ;
			return _Utils_Tuple2(
				s_ * 1000,
				function (t) {
					var s = t * s_;
					var coshr0 = $gampleman$elm_visualization$Zoom$Interpolation$cosh(r0);
					var u = (a.size / (A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$ρ, 2) * d1)) * ((coshr0 * $gampleman$elm_visualization$Zoom$Interpolation$tanh(($gampleman$elm_visualization$Zoom$Interpolation$ρ * s) + r0)) - $gampleman$elm_visualization$Zoom$Interpolation$sinh(r0));
					return {
						cx: a.cx + (u * dx),
						cy: a.cy + (u * dy),
						size: (a.size * coshr0) / $gampleman$elm_visualization$Zoom$Interpolation$cosh(($gampleman$elm_visualization$Zoom$Interpolation$ρ * s) + r0)
					};
				});
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $gampleman$elm_visualization$Zoom$schedule = F3(
	function (btransform, center, _v0) {
		var model = _v0.a;
		var atransform = model.transform;
		var _v1 = model.extent;
		var _v2 = _v1.a;
		var top = _v2.a;
		var left = _v2.b;
		var _v3 = _v1.b;
		var bottom = _v3.a;
		var right = _v3.b;
		var w = A2($elm$core$Basics$max, bottom - top, right - left);
		var _v4 = A2($gampleman$elm_visualization$Zoom$Transform$invert, center, btransform);
		var bx = _v4.a;
		var by = _v4.b;
		var _v5 = A2($gampleman$elm_visualization$Zoom$Transform$invert, center, atransform);
		var ax = _v5.a;
		var ay = _v5.b;
		var _v6 = A2(
			$gampleman$elm_visualization$Zoom$Interpolation$interpolate,
			{cx: ax, cy: ay, size: w / atransform.k},
			{cx: bx, cy: by, size: w / btransform.k});
		var dur = _v6.a;
		var interp = _v6.b;
		return $gampleman$elm_visualization$Zoom$Zoom(
			_Utils_update(
				model,
				{
					transition: $elm$core$Maybe$Just(
						{
							duration: dur,
							elapsed: 0,
							interpolation: function (t) {
								if (t === 1) {
									return btransform;
								} else {
									var l = interp(t);
									var k = w / l.size;
									return {k: k, x: center.a - (l.cx * k), y: center.b - (l.cy * k)};
								}
							}
						})
				}));
	});
var $gampleman$elm_visualization$Zoom$translate = F3(
	function (_v0, _v1, trfm) {
		var position0x = _v0.a;
		var position0y = _v0.b;
		var position1x = _v1.a;
		var position1y = _v1.b;
		return _Utils_update(
			trfm,
			{x: position0x - (position1x * trfm.k), y: position0y - (position1y * trfm.k)});
	});
var $gampleman$elm_visualization$Zoom$update = F2(
	function (msg, _v0) {
		var model = _v0.a;
		switch (msg.$) {
			case 'DoubleClicked':
				var shiftKey = msg.a;
				var position0 = msg.b;
				var position1 = A2($gampleman$elm_visualization$Zoom$Transform$invert, position0, model.transform);
				var k = model.transform.k * (shiftKey ? 0.5 : 2);
				var newTransform = A3(
					$gampleman$elm_visualization$Zoom$constrain,
					model.extent,
					model.translateExtent,
					A3(
						$gampleman$elm_visualization$Zoom$translate,
						position0,
						position1,
						A3($gampleman$elm_visualization$Zoom$scale, model.scaleExtent, k, model.transform)));
				return A3(
					$gampleman$elm_visualization$Zoom$schedule,
					newTransform,
					position0,
					$gampleman$elm_visualization$Zoom$Zoom(
						_Utils_update(
							model,
							{touchStarted: false})));
			case 'MouseDown':
				var position = msg.a;
				var matrix = msg.b;
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							drag: $elm$core$Maybe$Just(
								{
									current: A2(
										$gampleman$elm_visualization$Zoom$Transform$invert,
										A2($gampleman$elm_visualization$Zoom$normalizePointerPosition, position, matrix),
										model.transform),
									matrix: matrix
								}),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'MouseMove':
				var position_ = msg.a;
				var _v2 = model.drag;
				if (_v2.$ === 'Just') {
					var drag = _v2.a;
					var position = A2($gampleman$elm_visualization$Zoom$normalizePointerPosition, position_, drag.matrix);
					var trasform_ = A3($gampleman$elm_visualization$Zoom$translate, position, drag.current, model.transform);
					return $gampleman$elm_visualization$Zoom$Zoom(
						_Utils_update(
							model,
							{
								drag: $elm$core$Maybe$Just(
									_Utils_update(
										drag,
										{
											current: A2($gampleman$elm_visualization$Zoom$Transform$invert, position, trasform_)
										})),
								transform: A3($gampleman$elm_visualization$Zoom$constrain, model.extent, model.translateExtent, trasform_),
								transition: $elm$core$Maybe$Nothing
							}));
				} else {
					return $gampleman$elm_visualization$Zoom$Zoom(model);
				}
			case 'MouseUp':
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{drag: $elm$core$Maybe$Nothing}));
			case 'Wheeled':
				var delta = msg.a;
				var position0 = msg.b;
				var position1 = A2($gampleman$elm_visualization$Zoom$Transform$invert, position0, model.transform);
				var k = model.transform.k * A2($elm$core$Basics$pow, 2, delta);
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							transform: A3(
								$gampleman$elm_visualization$Zoom$constrain,
								model.extent,
								model.translateExtent,
								A3(
									$gampleman$elm_visualization$Zoom$translate,
									position0,
									position1,
									A3($gampleman$elm_visualization$Zoom$scale, model.scaleExtent, k, model.transform))),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'GestureStarted':
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{gestureLastScale: 1}));
			case 'Gestured':
				var scl = msg.a;
				var position0 = msg.b;
				var position1 = A2($gampleman$elm_visualization$Zoom$Transform$invert, position0, model.transform);
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							gestureLastScale: scl,
							transform: A3(
								$gampleman$elm_visualization$Zoom$constrain,
								model.extent,
								model.translateExtent,
								A3(
									$gampleman$elm_visualization$Zoom$translate,
									position0,
									position1,
									A2($gampleman$elm_visualization$Zoom$Transform$scale, scl / model.gestureLastScale, model.transform))),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'TouchStartedExpired':
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{touchStarted: false}));
			case 'TouchStarted':
				var touches = msg.a;
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							touchStarted: true,
							touches: function () {
								var _v3 = _Utils_Tuple2(model.touches, touches);
								_v3$3:
								while (true) {
									if (_v3.b.b) {
										switch (_v3.a.$) {
											case 'NoTouches':
												if (!_v3.b.b.b) {
													var _v4 = _v3.a;
													var _v5 = _v3.b;
													var single = _v5.a;
													return $gampleman$elm_visualization$Zoom$OneFinger(
														{
															identifier: single.identifier,
															position: single.position,
															previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, single.position, model.transform)
														});
												} else {
													var _v6 = _v3.a;
													var _v7 = _v3.b;
													var one = _v7.a;
													var _v8 = _v7.b;
													var two = _v8.a;
													return A2(
														$gampleman$elm_visualization$Zoom$TwoFingers,
														{
															identifier: one.identifier,
															position: one.position,
															previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, one.position, model.transform)
														},
														{
															identifier: two.identifier,
															position: two.position,
															previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, two.position, model.transform)
														});
												}
											case 'OneFinger':
												var id1 = _v3.a.a;
												var _v9 = _v3.b;
												var one = _v9.a;
												return A2(
													$gampleman$elm_visualization$Zoom$TwoFingers,
													id1,
													{
														identifier: one.identifier,
														position: one.position,
														previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, one.position, model.transform)
													});
											default:
												break _v3$3;
										}
									} else {
										break _v3$3;
									}
								}
								return model.touches;
							}(),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'TouchMoved':
				var touches = msg.a;
				return $gampleman$elm_visualization$Zoom$Zoom(
					function () {
						var _v10 = model.touches;
						switch (_v10.$) {
							case 'OneFinger':
								var before = _v10.a;
								var _v11 = A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, before, touches);
								if (_v11.$ === 'Just') {
									var after = _v11.a;
									return _Utils_update(
										model,
										{
											touchStarted: false,
											touches: $gampleman$elm_visualization$Zoom$OneFinger(
												_Utils_update(
													before,
													{position: after.position})),
											transform: A3(
												$gampleman$elm_visualization$Zoom$constrain,
												model.extent,
												model.translateExtent,
												A3($gampleman$elm_visualization$Zoom$translate, after.position, before.previous, model.transform)),
											transition: $elm$core$Maybe$Nothing
										});
								} else {
									return model;
								}
							case 'TwoFingers':
								var beforeTouch1 = _v10.a;
								var beforeTouch2 = _v10.b;
								var t = model.transform;
								var before2 = beforeTouch2.previous;
								var before1 = beforeTouch1.previous;
								var distBefore = A2($gampleman$elm_visualization$Zoom$dist2, before1, before2);
								var after2 = A2(
									$elm$core$Maybe$withDefault,
									beforeTouch2.position,
									A2(
										$elm$core$Maybe$map,
										function ($) {
											return $.position;
										},
										A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, beforeTouch2, touches)));
								var after1 = A2(
									$elm$core$Maybe$withDefault,
									beforeTouch1.position,
									A2(
										$elm$core$Maybe$map,
										function ($) {
											return $.position;
										},
										A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, beforeTouch1, touches)));
								var distAfter = A2($gampleman$elm_visualization$Zoom$dist2, after1, after2);
								return _Utils_update(
									model,
									{
										touchStarted: false,
										touches: A2(
											$gampleman$elm_visualization$Zoom$TwoFingers,
											_Utils_update(
												beforeTouch1,
												{position: after1}),
											_Utils_update(
												beforeTouch2,
												{position: after2})),
										transform: A3(
											$gampleman$elm_visualization$Zoom$constrain,
											model.extent,
											model.translateExtent,
											A3(
												$gampleman$elm_visualization$Zoom$translate,
												A2($gampleman$elm_visualization$Zoom$midpoint, after1, after2),
												A2($gampleman$elm_visualization$Zoom$midpoint, before1, before2),
												A3(
													$gampleman$elm_visualization$Zoom$scale,
													model.scaleExtent,
													$elm$core$Basics$sqrt(distAfter / distBefore),
													t))),
										transition: $elm$core$Maybe$Nothing
									});
							default:
								return model;
						}
					}());
			case 'TouchEnded':
				var touches = msg.a;
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							touches: function () {
								var _v12 = model.touches;
								switch (_v12.$) {
									case 'NoTouches':
										return $gampleman$elm_visualization$Zoom$NoTouches;
									case 'OneFinger':
										var touch1 = _v12.a;
										var _v13 = A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, touch1, touches);
										if (_v13.$ === 'Just') {
											return $gampleman$elm_visualization$Zoom$NoTouches;
										} else {
											return model.touches;
										}
									default:
										var before1 = _v12.a;
										var before2 = _v12.b;
										var _v14 = _Utils_Tuple2(
											A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, before1, touches),
											A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, before2, touches));
										if (_v14.a.$ === 'Just') {
											if (_v14.b.$ === 'Just') {
												return $gampleman$elm_visualization$Zoom$NoTouches;
											} else {
												var _v15 = _v14.b;
												return $gampleman$elm_visualization$Zoom$OneFinger(before2);
											}
										} else {
											if (_v14.b.$ === 'Just') {
												var _v16 = _v14.a;
												return $gampleman$elm_visualization$Zoom$OneFinger(before1);
											} else {
												var _v17 = _v14.a;
												var _v18 = _v14.b;
												return model.touches;
											}
										}
								}
							}()
						}));
			default:
				var delta = msg.a;
				var _v19 = model.transition;
				if (_v19.$ === 'Just') {
					var transition = _v19.a;
					var duration = transition.duration;
					var elapsed = transition.elapsed;
					var interpolation = transition.interpolation;
					return $gampleman$elm_visualization$Zoom$Zoom(
						_Utils_update(
							model,
							{
								transform: interpolation(
									$gampleman$elm_visualization$Zoom$easingInOutCubic(
										A3($elm$core$Basics$clamp, 0, 1, (elapsed + delta) / duration))),
								transition: (_Utils_cmp(elapsed + delta, duration) > -1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
									_Utils_update(
										transition,
										{elapsed: elapsed + delta}))
							}));
				} else {
					return $gampleman$elm_visualization$Zoom$Zoom(model);
				}
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 'CodeChange':
					var newInput = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{input: newInput}),
						$elm$core$Platform$Cmd$none);
				case 'Parse':
					var parseOut = $author$project$Formula$parse(model.input);
					var new_formula = A2($elm$core$Result$withDefault, $author$project$Formula$Falsity, parseOut);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								evalRule: A3(
									$author$project$Formula$evaluate,
									model.simulation,
									new_formula,
									$author$project$Timeline$evalTimes(model)),
								output: new_formula,
								parseError: function () {
									if (parseOut.$ === 'Ok') {
										return $elm$core$Maybe$Nothing;
									} else {
										var err = parseOut.a;
										return $elm$core$Maybe$Just(err);
									}
								}()
							}),
						$elm$core$Platform$Cmd$none);
				case 'ZoomMsg':
					var zoomMsg = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								zoom: A2($gampleman$elm_visualization$Zoom$update, zoomMsg, model.zoom)
							}),
						$elm$core$Platform$Cmd$none);
				case 'MoveScrubber':
					var t = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								currentTime: A2($author$project$Timeline$scaleInvert, model, t)
							}),
						$elm$core$Platform$Cmd$none);
				case 'ResetZoom':
					var initZoom = A3(
						$gampleman$elm_visualization$Zoom$scaleExtent,
						0.3,
						10,
						$gampleman$elm_visualization$Zoom$init(
							{height: $author$project$Main$canvasHeight, width: $author$project$Main$canvasWidth}));
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{zoom: initZoom}),
						$elm$core$Platform$Cmd$none);
				case 'ShowLegend':
					var b = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{showLegend: b}),
						$elm$core$Platform$Cmd$none);
				case 'ChangeMode':
					var mode = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{viewMode: mode}),
						$elm$core$Platform$Cmd$none);
				case 'DownloadSim':
					return _Utils_Tuple2(
						model,
						A3(
							$elm$file$File$Download$string,
							'simulation.json',
							'application/json',
							A2(
								$elm$json$Json$Encode$encode,
								4,
								$author$project$Data$encodeSimulation(model.simulation))));
				case 'SimFileRequested':
					return _Utils_Tuple2(
						model,
						A2(
							$elm$file$File$Select$file,
							_List_fromArray(
								['application/json']),
							$author$project$Model$SimFileSelected));
				case 'SimFileSelected':
					var file = msg.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$core$Task$perform,
							$author$project$Model$SimFileLoaded,
							$elm$file$File$toString(file)));
				case 'SimFileLoaded':
					var content = msg.a;
					var decodedFile = $author$project$Data$decodeSimulation(content);
					if (decodedFile.$ === 'Ok') {
						var sim = decodedFile.a;
						var $temp$msg = $author$project$Model$Parse,
							$temp$model = _Utils_update(
							model,
							{
								maxTime: $author$project$Data$getMaxTime(sim),
								minTime: $author$project$Data$getMinTime(sim),
								simulation: sim
							});
						msg = $temp$msg;
						model = $temp$model;
						continue update;
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				case 'DownloadRule':
					return _Utils_Tuple2(
						model,
						A3(
							$elm$file$File$Download$string,
							model.helixData.flightRuleID + '.json',
							'application/json',
							A2(
								$elm$json$Json$Encode$encode,
								4,
								A2($author$project$Data$encodeRule, model.input, model.helixData))));
				case 'RuleFileRequested':
					return _Utils_Tuple2(
						model,
						A2(
							$elm$file$File$Select$file,
							_List_fromArray(
								['application/json']),
							$author$project$Model$RuleFileSelected));
				case 'RuleFileSelected':
					var file = msg.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$core$Task$perform,
							$author$project$Model$RuleFileLoaded,
							$elm$file$File$toString(file)));
				case 'RuleFileLoaded':
					var content = msg.a;
					var decodedFile = $author$project$Data$decodeRule(content);
					if (decodedFile.$ === 'Ok') {
						var ruleText = decodedFile.a.ruleText;
						var helixData = decodedFile.a.helixData;
						var $temp$msg = $author$project$Model$Parse,
							$temp$model = _Utils_update(
							model,
							{helixData: helixData, input: ruleText});
						msg = $temp$msg;
						model = $temp$model;
						continue update;
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				case 'UpdateOperationalCategory':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{operationalCategory: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'UpdateLevel':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{level: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'UpdateCognizantEngineer':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{cognizantEngineer: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'UpdateFlightRuleID':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{flightRuleID: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'UpdateFlightRuleTitle':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{flightRuleTitle: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'UpdateDescription':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{description: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'UpdateRationale':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{rationale: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'UpdateViolationImpact':
					var value = msg.a;
					var oldhelix = model.helixData;
					var newhelix = _Utils_update(
						oldhelix,
						{violationImpact: value});
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{helixData: newhelix}),
						$elm$core$Platform$Cmd$none);
				case 'NewRule':
					var $temp$msg = $author$project$Model$Parse,
						$temp$model = _Utils_update(
						model,
						{
							helixData: {cognizantEngineer: '', description: '', flightRuleID: '', flightRuleTitle: '', level: '', operationalCategory: '', rationale: '', violationImpact: ''},
							input: '',
							viewMode: $author$project$Model$Authoring
						});
					msg = $temp$msg;
					model = $temp$model;
					continue update;
				default:
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
		}
	});
var $author$project$Model$DownloadRule = {$: 'DownloadRule'};
var $author$project$Model$Evaluation = {$: 'Evaluation'};
var $author$project$Model$NewRule = {$: 'NewRule'};
var $author$project$Model$RuleFileRequested = {$: 'RuleFileRequested'};
var $author$project$Model$ShowLegend = function (a) {
	return {$: 'ShowLegend', a: a};
};
var $author$project$Model$SimFileRequested = {$: 'SimFileRequested'};
var $author$project$Model$Validation = {$: 'Validation'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Model$ChangeMode = function (a) {
	return {$: 'ChangeMode', a: a};
};
var $author$project$Model$NoMsg = {$: 'NoMsg'};
var $author$project$Main$decodeViewModeSelector = function (dropdownString) {
	switch (dropdownString) {
		case 'Authoring':
			return $author$project$Model$ChangeMode($author$project$Model$Authoring);
		case 'Validation':
			return $author$project$Model$ChangeMode($author$project$Model$Validation);
		case 'Evaluation':
			return $author$project$Model$ChangeMode($author$project$Model$Evaluation);
		default:
			return $author$project$Model$NoMsg;
	}
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Formula$problemToString = function (problem) {
	switch (problem.$) {
		case 'Expecting':
			var string = problem.a;
			return 'Expecting ' + (string + ' ');
		case 'ExpectingVariable':
			return 'Expecting Variable';
		case 'ExpectingSymbol':
			var string = problem.a;
			return 'Expecting Symbol' + (string + ' ');
		case 'ExpectingKeyword':
			var string = problem.a;
			return 'Expecting Keyword' + (string + ' ');
		case 'ExpectingEnd':
			return 'Expecting End of Rule';
		case 'UnexpectedChar':
			return 'Unecpected Character';
		case 'Problem':
			var string = problem.a;
			return 'Problem Parsing ' + string;
		case 'BadRepeat':
			return 'Bad Repeat';
		default:
			return '';
	}
};
var $author$project$Formula$errorMessage = function (errs) {
	return A3(
		$elm$core$List$foldl,
		$elm$core$Basics$append,
		'',
		A2(
			$elm$core$List$map,
			function (err) {
				return 'Error at line: ' + ($elm$core$String$fromInt(err.row) + (' column: ' + ($elm$core$String$fromInt(err.col) + (' \n  ' + ($author$project$Formula$problemToString(err.problem) + '\n\n')))));
			},
			errs));
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $author$project$Formula$timeUnitString = function (unit) {
	switch (unit.$) {
		case 'MilliSecond':
			return 'ms';
		case 'Second':
			return 's';
		case 'Minute':
			return 'm';
		case 'Hour':
			return 'h';
		case 'Day':
			return 'd';
		default:
			return 'y';
	}
};
var $author$project$Formula$formulaToString = function (formula) {
	var formulaToStringAux = F2(
		function (formulaAux, tabLevel) {
			switch (formulaAux.$) {
				case 'Truth':
					return ' ⊤';
				case 'Falsity':
					return ' ⊥';
				case 'State':
					var name = formulaAux.a;
					return ' ' + name;
				case 'Not':
					var f = formulaAux.a;
					return ' ¬(' + (A2(formulaToStringAux, f, tabLevel) + ' )');
				case 'And':
					var f1 = formulaAux.a;
					var f2 = formulaAux.b;
					return A2(formulaToStringAux, f1, tabLevel) + (' /\\' + A2(formulaToStringAux, f2, tabLevel));
				case 'Or':
					var f1 = formulaAux.a;
					var f2 = formulaAux.b;
					return A2(formulaToStringAux, f1, tabLevel) + (' \\/' + A2(formulaToStringAux, f2, tabLevel));
				case 'Implies':
					var f1 = formulaAux.a;
					var f2 = formulaAux.b;
					return ' (' + (A2(formulaToStringAux, f1, tabLevel) + (' ) => (\n' + (A2($elm$core$String$repeat, tabLevel + 1, '  ') + (A2(formulaToStringAux, f2, tabLevel + 1) + ')\n'))));
				case 'Eventually':
					var f = formulaAux.a;
					return ' (F \n' + (A2($elm$core$String$repeat, tabLevel + 1, '  ') + (A2(formulaToStringAux, f, tabLevel + 1) + (' )\n' + A2($elm$core$String$repeat, tabLevel, '  '))));
				case 'Globally':
					var f = formulaAux.a;
					return ' (G \n' + (A2($elm$core$String$repeat, tabLevel + 1, '  ') + (A2(formulaToStringAux, f, tabLevel + 1) + (' )\n' + A2($elm$core$String$repeat, tabLevel, '  '))));
				case 'Until':
					var f1 = formulaAux.a;
					var f2 = formulaAux.b;
					return ' (' + (A2(formulaToStringAux, f1, 0) + (' U ' + (A2(formulaToStringAux, f2, 0) + (' )\n' + A2($elm$core$String$repeat, tabLevel, '  ')))));
				case 'BoundedEventually':
					var f = formulaAux.a;
					var bound = formulaAux.b;
					var unit = formulaAux.c;
					return ' (F [0, ' + ($elm$core$String$fromFloat(bound) + (' ' + ($author$project$Formula$timeUnitString(unit) + (']\n' + (A2($elm$core$String$repeat, tabLevel + 1, '  ') + (A2(formulaToStringAux, f, tabLevel + 1) + (' )\n' + A2($elm$core$String$repeat, tabLevel, '  '))))))));
				case 'BoundedGlobally':
					var f = formulaAux.a;
					var bound = formulaAux.b;
					var unit = formulaAux.c;
					return ' (G [0, ' + ($elm$core$String$fromFloat(bound) + (' ' + ($author$project$Formula$timeUnitString(unit) + (']\n' + (A2($elm$core$String$repeat, tabLevel + 1, '  ') + (A2(formulaToStringAux, f, tabLevel + 1) + (' )\n' + A2($elm$core$String$repeat, tabLevel, '  '))))))));
				default:
					var f1 = formulaAux.a;
					var f2 = formulaAux.b;
					var bound = formulaAux.c;
					var unit = formulaAux.d;
					return ' (' + (A2(formulaToStringAux, f1, 0) + (' U[0, ' + ($elm$core$String$fromFloat(bound) + (' ' + ($author$project$Formula$timeUnitString(unit) + ('] ' + (A2(formulaToStringAux, f2, 0) + (' )\n' + A2($elm$core$String$repeat, tabLevel, '  ')))))))));
			}
		});
	return A2(formulaToStringAux, formula, 0);
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
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
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Model$MoveScrubber = function (a) {
	return {$: 'MoveScrubber', a: a};
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty('min');
var $author$project$Timeline$scale = F2(
	function (model, time) {
		return (($elm$time$Time$posixToMillis(time) - $elm$time$Time$posixToMillis(model.minTime)) / ($elm$time$Time$posixToMillis(model.maxTime) - $elm$time$Time$posixToMillis(model.minTime))) * 100;
	});
var $elm$html$Html$Attributes$step = function (n) {
	return A2($elm$html$Html$Attributes$stringProperty, 'step', n);
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Timeline$timeScrubber = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('ScrubberContainer')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('ScrubberInput'),
						$elm$html$Html$Attributes$type_('range'),
						$elm$html$Html$Attributes$step('0.01'),
						$elm$html$Html$Attributes$value(
						$elm$core$String$fromFloat(
							A2($author$project$Timeline$scale, model, model.currentTime))),
						$elm$html$Html$Attributes$min(
						$elm$core$String$fromFloat(
							A2($author$project$Timeline$scale, model, model.minTime))),
						$elm$html$Html$Attributes$max(
						$elm$core$String$fromFloat(
							A2($author$project$Timeline$scale, model, model.maxTime))),
						$elm$html$Html$Events$onInput(
						function (s) {
							return $author$project$Model$MoveScrubber(
								A2(
									$elm$core$Maybe$withDefault,
									0,
									$elm$core$String$toFloat(s)));
						})
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Diagram$EvalFalse = {$: 'EvalFalse'};
var $author$project$Diagram$EvalTrue = {$: 'EvalTrue'};
var $author$project$Model$ResetZoom = function (a) {
	return {$: 'ResetZoom', a: a};
};
var $author$project$Diagram$TrueGood = {$: 'TrueGood'};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $gampleman$elm_visualization$Zoom$DoubleClicked = F2(
	function (a, b) {
		return {$: 'DoubleClicked', a: a, b: b};
	});
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
var $elm$svg$Svg$Events$custom = $elm$html$Html$Events$custom;
var $gampleman$elm_visualization$Zoom$Rect = F4(
	function (x, y, width, height) {
		return {height: height, width: width, x: x, y: y};
	});
var $elm$json$Json$Decode$map4 = _Json_map4;
var $gampleman$elm_visualization$Zoom$decodeRect = A5(
	$elm$json$Json$Decode$map4,
	$gampleman$elm_visualization$Zoom$Rect,
	A2($elm$json$Json$Decode$field, 'x', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'y', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float));
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $gampleman$elm_visualization$Zoom$decodeSVGTransformMatrix = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A4(
			$elm$json$Json$Decode$map3,
			F3(
				function (viewBox, width, height) {
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							_Utils_Tuple3(viewBox.width / width, 0, 0),
							_Utils_Tuple3(0, viewBox.height / height, 0)));
				}),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['currentTarget', 'viewBox', 'baseVal']),
				$gampleman$elm_visualization$Zoom$decodeRect),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['currentTarget', 'width', 'baseVal', 'value']),
				$elm$json$Json$Decode$float),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['currentTarget', 'height', 'baseVal', 'value']),
				$elm$json$Json$Decode$float)),
			$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
		]));
var $gampleman$elm_visualization$Zoom$decodeMousePosition = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (maybeMatrix, x, y) {
			return A2(
				$gampleman$elm_visualization$Zoom$normalizePointerPosition,
				_Utils_Tuple2(x, y),
				maybeMatrix);
		}),
	$gampleman$elm_visualization$Zoom$decodeSVGTransformMatrix,
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float)
			])),
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float)
			])));
var $gampleman$elm_visualization$Zoom$onDoubleClick = F2(
	function (_v0, tagger) {
		return A2(
			$elm$svg$Svg$Events$custom,
			'dblclick',
			A3(
				$elm$json$Json$Decode$map2,
				F2(
					function (shiftDown, position) {
						return {
							message: tagger(
								A2($gampleman$elm_visualization$Zoom$DoubleClicked, shiftDown, position)),
							preventDefault: true,
							stopPropagation: true
						};
					}),
				A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool),
				$gampleman$elm_visualization$Zoom$decodeMousePosition));
	});
var $gampleman$elm_visualization$Zoom$MouseDown = F2(
	function (a, b) {
		return {$: 'MouseDown', a: a, b: b};
	});
var $gampleman$elm_visualization$Zoom$onDrag = F2(
	function (_v0, tagger) {
		var drag = _v0.a.drag;
		if (drag.$ === 'Nothing') {
			return _List_fromArray(
				[
					A2(
					$elm$svg$Svg$Events$custom,
					'mousedown',
					A4(
						$elm$json$Json$Decode$map3,
						F3(
							function (x, y, matrix) {
								return {
									message: tagger(
										A2(
											$gampleman$elm_visualization$Zoom$MouseDown,
											_Utils_Tuple2(x, y),
											matrix)),
									preventDefault: false,
									stopPropagation: true
								};
							}),
						A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
						A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float),
						$gampleman$elm_visualization$Zoom$decodeSVGTransformMatrix))
				]);
		} else {
			return _List_Nil;
		}
	});
var $gampleman$elm_visualization$Zoom$GestureStarted = {$: 'GestureStarted'};
var $gampleman$elm_visualization$Zoom$Gestured = F2(
	function (a, b) {
		return {$: 'Gestured', a: a, b: b};
	});
var $gampleman$elm_visualization$Zoom$onGesture = F2(
	function (_v0, tagger) {
		return _List_fromArray(
			[
				A2(
				$elm$svg$Svg$Events$custom,
				'gesturestart',
				$elm$json$Json$Decode$succeed(
					{
						message: tagger($gampleman$elm_visualization$Zoom$GestureStarted),
						preventDefault: true,
						stopPropagation: true
					})),
				A2(
				$elm$svg$Svg$Events$custom,
				'gesturechange',
				A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (scl, position) {
							return {
								message: tagger(
									A2($gampleman$elm_visualization$Zoom$Gestured, scl, position)),
								preventDefault: true,
								stopPropagation: true
							};
						}),
					A2($elm$json$Json$Decode$field, 'scale', $elm$json$Json$Decode$float),
					$gampleman$elm_visualization$Zoom$decodeMousePosition))
			]);
	});
var $gampleman$elm_visualization$Zoom$TouchEnded = function (a) {
	return {$: 'TouchEnded', a: a};
};
var $gampleman$elm_visualization$Zoom$TouchMoved = function (a) {
	return {$: 'TouchMoved', a: a};
};
var $gampleman$elm_visualization$Zoom$TouchStarted = function (a) {
	return {$: 'TouchStarted', a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $gampleman$elm_visualization$Zoom$listLike = function (itemDecoder) {
	var decodeOne = function (n) {
		return A2(
			$elm$json$Json$Decode$field,
			$elm$core$String$fromInt(n),
			itemDecoder);
	};
	var decodeN = function (n) {
		return A3(
			$elm$core$List$foldr,
			$elm$json$Json$Decode$map2($elm$core$List$cons),
			$elm$json$Json$Decode$succeed(_List_Nil),
			A2(
				$elm$core$List$map,
				decodeOne,
				A2($elm$core$List$range, 0, n - 1)));
	};
	return A2(
		$elm$json$Json$Decode$andThen,
		decodeN,
		A2($elm$json$Json$Decode$field, 'length', $elm$json$Json$Decode$int));
};
var $gampleman$elm_visualization$Zoom$decodeTouches = A2(
	$elm$json$Json$Decode$andThen,
	function (maybeMatrix) {
		return A2(
			$elm$json$Json$Decode$field,
			'changedTouches',
			$gampleman$elm_visualization$Zoom$listLike(
				A4(
					$elm$json$Json$Decode$map3,
					F3(
						function (x, y, identifier) {
							return {
								identifier: identifier,
								position: A2(
									$gampleman$elm_visualization$Zoom$normalizePointerPosition,
									_Utils_Tuple2(x, y),
									maybeMatrix)
							};
						}),
					A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
					A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float),
					A2($elm$json$Json$Decode$field, 'identifier', $elm$json$Json$Decode$int))));
	},
	$gampleman$elm_visualization$Zoom$decodeSVGTransformMatrix);
var $gampleman$elm_visualization$Zoom$onTouch = F2(
	function (_v0, tagger) {
		var zoom = _v0.a;
		return _List_fromArray(
			[
				A2(
				$elm$svg$Svg$Events$custom,
				'touchstart',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						var _v1 = _Utils_Tuple3(zoom.touchStarted, touches, zoom.touches);
						if (((_v1.a && _v1.b.b) && (!_v1.b.b.b)) && (_v1.c.$ === 'NoTouches')) {
							var _v2 = _v1.b;
							var touch0 = _v2.a;
							var _v3 = _v1.c;
							return {
								message: tagger(
									A2($gampleman$elm_visualization$Zoom$DoubleClicked, false, touch0.position)),
								preventDefault: true,
								stopPropagation: true
							};
						} else {
							return {
								message: tagger(
									$gampleman$elm_visualization$Zoom$TouchStarted(touches)),
								preventDefault: false,
								stopPropagation: true
							};
						}
					},
					$gampleman$elm_visualization$Zoom$decodeTouches)),
				A2(
				$elm$svg$Svg$Events$custom,
				'touchmove',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						return {
							message: tagger(
								$gampleman$elm_visualization$Zoom$TouchMoved(touches)),
							preventDefault: true,
							stopPropagation: true
						};
					},
					$gampleman$elm_visualization$Zoom$decodeTouches)),
				A2(
				$elm$svg$Svg$Events$custom,
				'touchend',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						return {
							message: tagger(
								$gampleman$elm_visualization$Zoom$TouchEnded(touches)),
							preventDefault: false,
							stopPropagation: true
						};
					},
					$gampleman$elm_visualization$Zoom$decodeTouches)),
				A2(
				$elm$svg$Svg$Events$custom,
				'touchcancel',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						return {
							message: tagger(
								$gampleman$elm_visualization$Zoom$TouchEnded(touches)),
							preventDefault: false,
							stopPropagation: true
						};
					},
					$gampleman$elm_visualization$Zoom$decodeTouches)),
				A2($elm$html$Html$Attributes$style, 'touch-action', 'none'),
				A2($elm$html$Html$Attributes$style, '-webkit-tap-highlight-color', 'rgba(0,0,0,0)')
			]);
	});
var $gampleman$elm_visualization$Zoom$Wheeled = F2(
	function (a, b) {
		return {$: 'Wheeled', a: a, b: b};
	});
var $gampleman$elm_visualization$Zoom$onWheel = F2(
	function (_v0, tagger) {
		return A2(
			$elm$svg$Svg$Events$custom,
			'wheel',
			A4(
				$elm$json$Json$Decode$map3,
				F3(
					function (deltaY, deltaMode, position) {
						return {
							message: tagger(
								A2(
									$gampleman$elm_visualization$Zoom$Wheeled,
									(-deltaY) * ((!deltaMode) ? 0.002 : ((deltaMode === 1) ? 0.05 : 1)),
									position)),
							preventDefault: true,
							stopPropagation: true
						};
					}),
				A2($elm$json$Json$Decode$field, 'deltaY', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'deltaMode', $elm$json$Json$Decode$int),
				$gampleman$elm_visualization$Zoom$decodeMousePosition));
	});
var $elm$svg$Svg$Attributes$preserveAspectRatio = _VirtualDom_attribute('preserveAspectRatio');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $gampleman$elm_visualization$Zoom$Transform$toString = function (_v0) {
	var k = _v0.k;
	var x = _v0.x;
	var y = _v0.y;
	return 'translate(' + ($elm$core$String$fromFloat(x) + (',' + ($elm$core$String$fromFloat(y) + (') scale(' + ($elm$core$String$fromFloat(k) + ')')))));
};
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $gampleman$elm_visualization$Zoom$transform = function (_v0) {
	var zoom = _v0.a;
	return $elm$svg$Svg$Attributes$transform(
		$gampleman$elm_visualization$Zoom$Transform$toString(zoom.transform));
};
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $author$project$Diagram$FalseGood = {$: 'FalseGood'};
var $author$project$Diagram$Neutral = {$: 'Neutral'};
var $author$project$Diagram$Undetermined = {$: 'Undetermined'};
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var $author$project$Diagram$toColorString = F2(
	function (colorCtx, extraClass) {
		return extraClass + (' ' + (colorCtx.colored ? _Utils_ap(
			colorCtx.violation ? 'Violation ' : 'Validation ',
			_Utils_ap(
				function () {
					var _v0 = colorCtx._eval;
					switch (_v0.$) {
						case 'EvalTrue':
							return 'EvalTrue ';
						case 'EvalFalse':
							return 'EvalFalse ';
						default:
							return 'Undetermined ';
					}
				}(),
				function () {
					var _v1 = colorCtx.tgt;
					switch (_v1.$) {
						case 'TrueGood':
							return 'TrueGood';
						case 'FalseGood':
							return 'FalseGood';
						default:
							return 'NeutralTarget';
					}
				}())) : ''));
	});
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Diagram$andGlyph = F3(
	function (ctx, clr, nodeY) {
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(nodeY - (0.5 * ctx.glyphSize.height))),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(ctx.glyphSize.height)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$rx(
							$elm$core$String$fromFloat(0.5 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'shape'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(ctx.x + (0.1 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(nodeY - (0.45 * ctx.glyphSize.height))),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(0.9 * ctx.glyphSize.height)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(0.8 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$rx(
							$elm$core$String$fromFloat(0.4 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$fill('#CECECE'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'inner shape'))
						]),
					_List_Nil)
				]));
	});
var $author$project$Diagram$getMonoWidth = F2(
	function (ctx, text) {
		return (($elm$core$String$length(text) * 0.55) * ctx.stateHeight) + ctx.stateHeight;
	});
var $author$project$Diagram$boundingBox = F2(
	function (ctx, formula) {
		switch (formula.$) {
			case 'EvaluatedTruth':
				return {x: ctx.glyphSize.width, y: ctx.glyphSize.height};
			case 'EvaluatedFalsity':
				return {x: ctx.glyphSize.width, y: ctx.glyphSize.height};
			case 'EvaluatedState':
				var name = formula.a;
				return {
					x: A2($author$project$Diagram$getMonoWidth, ctx, name),
					y: ctx.stateHeight
				};
			case 'EvaluatedImplies':
				var f1 = formula.a;
				var f2 = formula.b;
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				return {
					x: A2($elm$core$Basics$max, f1_bb.x, f2_bb.x) + ctx.blockSize,
					y: (f1_bb.y + ctx.blockSize) + f2_bb.y
				};
			case 'EvaluatedAnd':
				var f1 = formula.a;
				var f2 = formula.b;
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				return {
					x: A2($elm$core$Basics$max, f1_bb.x, f2_bb.x) + ctx.blockSize,
					y: (f1_bb.y + ctx.blockSize) + f2_bb.y
				};
			case 'EvaluatedOr':
				var f1 = formula.a;
				var f2 = formula.b;
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				return {
					x: A2($elm$core$Basics$max, f1_bb.x, f2_bb.x) + ctx.blockSize,
					y: (f1_bb.y + ctx.blockSize) + f2_bb.y
				};
			case 'EvaluatedNot':
				var f = formula.a;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				return {
					x: (f_bb.x + (0.25 * ctx.blockSize)) + ctx.glyphSize.width,
					y: A2($elm$core$Basics$max, ctx.glyphSize.width, f_bb.y)
				};
			case 'EvaluatedEventually':
				var f = formula.a;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				return {
					x: f_bb.x + (3 * ctx.blockSize),
					y: ctx.blockSize + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize)
				};
			case 'EvaluatedGlobally':
				var f = formula.a;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				return {
					x: f_bb.x + (3 * ctx.blockSize),
					y: ctx.blockSize + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize)
				};
			case 'EvaluatedUntil':
				var f1 = formula.a;
				var f2 = formula.b;
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				return {
					x: (f1_bb.x + f2_bb.x) + (4 * ctx.blockSize),
					y: ctx.blockSize + A2(
						$elm$core$Basics$max,
						2 * ctx.blockSize,
						A2($elm$core$Basics$max, f2_bb.y, f1_bb.y))
				};
			case 'EvaluatedBoundedEventually':
				var f = formula.a;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				return {
					x: f_bb.x + (3 * ctx.blockSize),
					y: (ctx.stateHeight + ctx.blockSize) + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize)
				};
			case 'EvaluatedBoundedGlobally':
				var f = formula.a;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				return {
					x: f_bb.x + (3 * ctx.blockSize),
					y: (ctx.stateHeight + ctx.blockSize) + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize)
				};
			default:
				var f1 = formula.a;
				var f2 = formula.b;
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				return {
					x: (f1_bb.x + f2_bb.x) + (4 * ctx.blockSize),
					y: (ctx.stateHeight + ctx.blockSize) + A2(
						$elm$core$Basics$max,
						2 * ctx.blockSize,
						A2($elm$core$Basics$max, f2_bb.y, f1_bb.y))
				};
		}
	});
var $elm$svg$Svg$Attributes$fontFamily = _VirtualDom_attribute('font-family');
var $elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $author$project$Diagram$bracketBoundText = F3(
	function (ctx, clr, boundString) {
		return A2(
			$elm$svg$Svg$text_,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$fill('black'),
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromFloat(ctx.x)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromFloat(ctx.y)),
					$elm$svg$Svg$Attributes$fontSize(
					$elm$core$String$fromFloat(ctx.stateHeight)),
					$elm$svg$Svg$Attributes$textAnchor('end'),
					$elm$svg$Svg$Attributes$fontFamily('Monaco, monospace'),
					$elm$svg$Svg$Attributes$class(
					A2($author$project$Diagram$toColorString, clr, 'text shape boundText'))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(boundString)
				]));
	});
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $author$project$Diagram$closedBracket = F3(
	function (ctx, clr, height) {
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke('black'),
					$elm$svg$Svg$Attributes$strokeLinecap('round'),
					$elm$svg$Svg$Attributes$class(
					A2($author$project$Diagram$toColorString, clr, 'line'))
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.blockSize))),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.blockSize))),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.blockSize))),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y + height))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.blockSize))),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y + height)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y + height))
						]),
					_List_Nil)
				]));
	});
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $author$project$Diagram$curvedConnector = F6(
	function (ctx, clr1, clr2, sourceY, nodeY, targetY) {
		var _v0 = _Utils_Tuple3(ctx.x + ctx.blockSize, ctx.x + ctx.blockSize, ctx.x + (0.5 * ctx.glyphSize.width));
		var sourceX = _v0.a;
		var targetX = _v0.b;
		var nodeX = _v0.c;
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(sourceX) + (' ' + ($elm$core$String$fromFloat(sourceY) + (' ' + (' C ' + ($elm$core$String$fromFloat((1.0 * nodeX) + (0.0 * sourceX)) + (' ' + ($elm$core$String$fromFloat(sourceY) + (', ' + ($elm$core$String$fromFloat(nodeX) + (' ' + ($elm$core$String$fromFloat((0.0 * nodeY) + (1.0 * sourceY)) + (', ' + ($elm$core$String$fromFloat(nodeX) + (' ' + $elm$core$String$fromFloat(nodeY - (0.5 * ctx.glyphSize.height)))))))))))))))))),
							$elm$svg$Svg$Attributes$strokeWidth('1.0'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$fill('transparent'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr1, 'line'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(nodeX) + (' ' + ($elm$core$String$fromFloat(nodeY + (0.5 * ctx.glyphSize.height)) + (' ' + (' C ' + ($elm$core$String$fromFloat(nodeX) + (' ' + ($elm$core$String$fromFloat((0.0 * nodeY) + (1.0 * targetY)) + (', ' + ($elm$core$String$fromFloat((1.0 * nodeX) + (0.0 * targetX)) + (' ' + ($elm$core$String$fromFloat(targetY) + (', ' + ($elm$core$String$fromFloat(targetX) + (' ' + $elm$core$String$fromFloat(targetY))))))))))))))))),
							$elm$svg$Svg$Attributes$strokeWidth('1.0'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$fill('transparent'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr2, 'line'))
						]),
					_List_Nil)
				]));
	});
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $author$project$Diagram$eventuallyBacketLine = F5(
	function (ctx, clr, bracketHeight, bracketWidth, bracketStart) {
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$circle,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$cx(
							$elm$core$String$fromFloat(ctx.x + ctx.blockSize)),
							$elm$svg$Svg$Attributes$cy(
							$elm$core$String$fromFloat(ctx.y + bracketHeight)),
							$elm$svg$Svg$Attributes$r('2'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'shape'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(bracketStart)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y + bracketHeight)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(bracketStart + bracketWidth)),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y + bracketHeight)),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$strokeWidth('1'),
							$elm$svg$Svg$Attributes$strokeDasharray('1 3'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'line'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(ctx.x + (1.5 * ctx.blockSize)) + (' ' + ($elm$core$String$fromFloat(ctx.y + (0.5 * bracketHeight)) + (' ' + (' C ' + ($elm$core$String$fromFloat(ctx.x + (1.0 * ctx.blockSize)) + (' ' + ($elm$core$String$fromFloat(ctx.y + (0.5 * bracketHeight)) + (', ' + ($elm$core$String$fromFloat(ctx.x + (1.0 * ctx.blockSize)) + (' ' + ($elm$core$String$fromFloat(ctx.y + (0.5 * bracketHeight)) + (', ' + ($elm$core$String$fromFloat(ctx.x + (1.0 * ctx.blockSize)) + (' ' + $elm$core$String$fromFloat(ctx.y + (1.0 * bracketHeight)))))))))))))))))),
							$elm$svg$Svg$Attributes$strokeWidth('1.0'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$fill('transparent'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'line'))
						]),
					_List_Nil)
				]));
	});
var $author$project$Diagram$getEvalContext = F2(
	function (ctx, estimate) {
		return _Utils_eq(ctx, $author$project$Diagram$Undetermined) ? $author$project$Diagram$Undetermined : (estimate ? $author$project$Diagram$EvalTrue : $author$project$Diagram$EvalFalse);
	});
var $author$project$Diagram$globallyBracketLine = F4(
	function (ctx, clr, bracketHeight, bracketWidth) {
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x + bracketWidth)),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y)),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$strokeWidth('1'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'line'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(ctx.x + (1.5 * ctx.blockSize)) + (' ' + ($elm$core$String$fromFloat(ctx.y + (0.5 * bracketHeight)) + (' ' + (' C ' + ($elm$core$String$fromFloat(ctx.x + (1.0 * ctx.blockSize)) + (' ' + ($elm$core$String$fromFloat(ctx.y + (0.5 * bracketHeight)) + (', ' + ($elm$core$String$fromFloat(ctx.x + (1.0 * ctx.blockSize)) + (' ' + ($elm$core$String$fromFloat(ctx.y + (0.5 * bracketHeight)) + (', ' + ($elm$core$String$fromFloat(ctx.x + (1.0 * ctx.blockSize)) + (' ' + $elm$core$String$fromFloat(ctx.y))))))))))))))))),
							$elm$svg$Svg$Attributes$strokeWidth('1.0'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$fill('transparent'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'line'))
						]),
					_List_Nil)
				]));
	});
var $author$project$Diagram$impliesGlyph = F5(
	function (ctx, clr, clrPre, clrPost, nodeY) {
		var outputCapHeight = ctx.glyphSize.height * 0.25;
		var _v0 = _Utils_Tuple2(ctx.x + (0.5 * ctx.glyphSize.width), nodeY - (0.5 * ctx.glyphSize.height));
		var inputX = _v0.a;
		var inputY = _v0.b;
		var _v1 = _Utils_Tuple2(inputX, (inputY + ctx.glyphSize.height) - outputCapHeight);
		var outputX = _v1.a;
		var outputY = _v1.b;
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(nodeY - (0.5 * ctx.glyphSize.height))),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(ctx.glyphSize.height)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$rx(
							$elm$core$String$fromFloat(0.5 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$fill('#CECECE'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'inner shape'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(inputX)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(inputY)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(outputX)),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(
								(clr.colored && _Utils_eq(clrPre._eval, $author$project$Diagram$EvalFalse)) ? (outputY - (1.0 * ctx.glyphSize.width)) : (outputY - 0.5))),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clrPre, 'line'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clrPost, 'shape')),
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(ctx.x) + (' ' + ($elm$core$String$fromFloat(outputY) + (' h ' + ($elm$core$String$fromFloat(ctx.glyphSize.width) + (' V ' + ($elm$core$String$fromFloat((outputY + outputCapHeight) - (0.5 * ctx.glyphSize.width)) + (' A ' + ($elm$core$String$fromFloat(0.5 * ctx.glyphSize.width) + (' ' + ($elm$core$String$fromFloat(0.5 * ctx.glyphSize.width) + (' 0 0 1 ' + ($elm$core$String$fromFloat(outputX) + (' ' + ($elm$core$String$fromFloat(outputY + outputCapHeight) + (' A ' + ($elm$core$String$fromFloat(0.5 * ctx.glyphSize.width) + (' ' + ($elm$core$String$fromFloat(0.5 * ctx.glyphSize.width) + (' 0 0 1 ' + ($elm$core$String$fromFloat(ctx.x) + (' ' + ($elm$core$String$fromFloat(outputY) + ' Z'))))))))))))))))))))))))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(outputX)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(outputY - 0.5)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(outputX + (0.3 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(outputY - (0.45 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clrPre, 'line'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(outputX)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(outputY - 0.5)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(outputX - (0.3 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(outputY - (0.45 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clrPre, 'line'))
						]),
					_List_Nil)
				]));
	});
var $author$project$Diagram$mergeConnector = F7(
	function (ctx, clr1, clr2, sourceY1, sourceY2, targetY1, targetY2) {
		var _v0 = _Utils_Tuple2(ctx.x + ctx.blockSize, ctx.x + (0.75 * ctx.glyphSize.width));
		var sourceX = _v0.a;
		var targetX = _v0.b;
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(sourceX) + (' ' + ($elm$core$String$fromFloat(sourceY1) + (' ' + (' C ' + ($elm$core$String$fromFloat((0.25 * sourceX) + (0.75 * targetX)) + (' ' + ($elm$core$String$fromFloat(sourceY1) + (', ' + ($elm$core$String$fromFloat((0.75 * sourceX) + (0.25 * targetX)) + (' ' + ($elm$core$String$fromFloat(targetY1) + (', ' + ($elm$core$String$fromFloat(targetX) + (' ' + $elm$core$String$fromFloat(targetY1))))))))))))))))),
							$elm$svg$Svg$Attributes$strokeWidth('1.0'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$fill('transparent'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr1, 'line'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d(
							'M ' + ($elm$core$String$fromFloat(sourceX) + (' ' + ($elm$core$String$fromFloat(sourceY2) + (' ' + (' C ' + ($elm$core$String$fromFloat(0.5 * (sourceX + targetX)) + (' ' + ($elm$core$String$fromFloat(sourceY2) + (', ' + ($elm$core$String$fromFloat(0.5 * (sourceX + targetX)) + (' ' + ($elm$core$String$fromFloat(targetY2) + (', ' + ($elm$core$String$fromFloat(targetX) + (' ' + $elm$core$String$fromFloat(targetY2))))))))))))))))),
							$elm$svg$Svg$Attributes$strokeWidth('1.0'),
							$elm$svg$Svg$Attributes$strokeLinecap('round'),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$fill('transparent'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr2, 'line'))
						]),
					_List_Nil)
				]));
	});
var $elm$svg$Svg$Attributes$strokeDashoffset = _VirtualDom_attribute('stroke-dashoffset');
var $author$project$Diagram$notGlyph = F2(
	function (ctx, clr) {
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$circle,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$cx(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$cy(
							$elm$core$String$fromFloat(ctx.y + (0.5 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$r(
							$elm$core$String$fromFloat(0.5 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'shape'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$circle,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$cx(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$cy(
							$elm$core$String$fromFloat(ctx.y + (0.5 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$r(
							$elm$core$String$fromFloat(0.4 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$fill('#CECECE'),
							$elm$svg$Svg$Attributes$class(
							'inner ' + A2($author$project$Diagram$toColorString, clr, 'shape'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x + ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y + ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$strokeDasharray(
							$elm$core$String$fromFloat(0.9 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$strokeDashoffset(
							$elm$core$String$fromFloat((-0.25) * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'line'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x + ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y + (0.5 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat((ctx.x + ctx.glyphSize.width) + (0.25 * ctx.blockSize))),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y + (0.5 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$stroke('black'),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'line'))
						]),
					_List_Nil)
				]));
	});
var $author$project$Diagram$openBracket = F3(
	function (ctx, clr, height) {
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$stroke('black'),
					$elm$svg$Svg$Attributes$strokeLinecap('round'),
					$elm$svg$Svg$Attributes$class(
					A2($author$project$Diagram$toColorString, clr, 'line'))
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.blockSize))),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y + height))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$line,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x1(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y1(
							$elm$core$String$fromFloat(ctx.y + height)),
							$elm$svg$Svg$Attributes$x2(
							$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.blockSize))),
							$elm$svg$Svg$Attributes$y2(
							$elm$core$String$fromFloat(ctx.y + height))
						]),
					_List_Nil)
				]));
	});
var $author$project$Diagram$orGlyph = F5(
	function (ctx, clr, clr1, clr2, nodeY) {
		return A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(ctx.x)),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(nodeY - (0.5 * ctx.glyphSize.height))),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(ctx.glyphSize.height)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$rx(
							$elm$core$String$fromFloat(0.5 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$class(
							A2($author$project$Diagram$toColorString, clr, 'shape'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(ctx.x + (0.1 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(nodeY - (0.45 * ctx.glyphSize.height))),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(0.4 * ctx.glyphSize.height)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(0.8 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$rx(
							$elm$core$String$fromFloat(0.4 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$fill('#CECECE'),
							$elm$svg$Svg$Attributes$class(
							'inner ' + A2($author$project$Diagram$toColorString, clr1, 'shape'))
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(ctx.x + (0.1 * ctx.glyphSize.width))),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(nodeY + (0.05 * ctx.glyphSize.height))),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(0.4 * ctx.glyphSize.height)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(0.8 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$rx(
							$elm$core$String$fromFloat(0.4 * ctx.glyphSize.width)),
							$elm$svg$Svg$Attributes$fill('#CECECE'),
							$elm$svg$Svg$Attributes$class(
							'inner ' + A2($author$project$Diagram$toColorString, clr2, 'shape'))
						]),
					_List_Nil)
				]));
	});
var $elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var $elm$svg$Svg$Attributes$textLength = _VirtualDom_attribute('textLength');
var $author$project$Diagram$untilConnectorLine = F3(
	function (ctx, clr, bracketHeight) {
		return A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1(
					$elm$core$String$fromFloat(ctx.x)),
					$elm$svg$Svg$Attributes$y1(
					$elm$core$String$fromFloat(ctx.y)),
					$elm$svg$Svg$Attributes$x2(
					$elm$core$String$fromFloat(ctx.x)),
					$elm$svg$Svg$Attributes$y2(
					$elm$core$String$fromFloat(ctx.y + bracketHeight)),
					$elm$svg$Svg$Attributes$stroke('black'),
					$elm$svg$Svg$Attributes$strokeWidth('1'),
					$elm$svg$Svg$Attributes$strokeLinecap('round'),
					$elm$svg$Svg$Attributes$class(
					A2($author$project$Diagram$toColorString, clr, 'line'))
				]),
			_List_Nil);
	});
var $author$project$Diagram$viewDiagram = F3(
	function (formula, ctx, clr) {
		switch (formula.$) {
			case 'EvaluatedTruth':
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$circle,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$cx(
									$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.glyphSize.width))),
									$elm$svg$Svg$Attributes$cy(
									$elm$core$String$fromFloat(ctx.y + (0.5 * ctx.blockSize))),
									$elm$svg$Svg$Attributes$r(
									$elm$core$String$fromFloat(0.5 * ctx.glyphSize.width)),
									$elm$svg$Svg$Attributes$class(
									A2(
										$author$project$Diagram$toColorString,
										_Utils_update(
											clr,
											{_eval: $author$project$Diagram$EvalTrue}),
										'shape'))
								]),
							_List_Nil)
						]));
			case 'EvaluatedFalsity':
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$circle,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$cx(
									$elm$core$String$fromFloat(ctx.x + (0.5 * ctx.glyphSize.width))),
									$elm$svg$Svg$Attributes$cy(
									$elm$core$String$fromFloat(ctx.y + (0.5 * ctx.blockSize))),
									$elm$svg$Svg$Attributes$r(
									$elm$core$String$fromFloat(0.5 * ctx.glyphSize.width)),
									$elm$svg$Svg$Attributes$class(
									A2(
										$author$project$Diagram$toColorString,
										_Utils_update(
											clr,
											{_eval: $author$project$Diagram$EvalFalse}),
										'shape'))
								]),
							_List_Nil)
						]));
			case 'EvaluatedState':
				var name = formula.a;
				var timeline = formula.b;
				var val = A2($author$project$Data$estimate, timeline, ctx.time).value;
				return A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('stateNode')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$rect,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$x(
									$elm$core$String$fromFloat(ctx.x)),
									$elm$svg$Svg$Attributes$y(
									$elm$core$String$fromFloat(ctx.y)),
									$elm$svg$Svg$Attributes$width(
									$elm$core$String$fromFloat(
										A2($author$project$Diagram$getMonoWidth, ctx, name))),
									$elm$svg$Svg$Attributes$height(
									$elm$core$String$fromFloat(ctx.stateHeight)),
									$elm$svg$Svg$Attributes$rx('1'),
									$elm$svg$Svg$Attributes$ry('1'),
									$elm$svg$Svg$Attributes$class(
									A2(
										$author$project$Diagram$toColorString,
										_Utils_update(
											clr,
											{
												_eval: A2($author$project$Diagram$getEvalContext, clr._eval, val)
											}),
										'shape stateNodeRect'))
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$text_,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$fill('white'),
									$elm$svg$Svg$Attributes$x(
									$elm$core$String$fromFloat(ctx.x + (0.25 * ctx.stateHeight))),
									$elm$svg$Svg$Attributes$y(
									$elm$core$String$fromFloat(ctx.y + (0.85 * ctx.stateHeight))),
									$elm$svg$Svg$Attributes$fontSize(
									$elm$core$String$fromFloat(ctx.stateHeight)),
									$elm$svg$Svg$Attributes$textLength(
									$elm$core$String$fromFloat(
										A2($author$project$Diagram$getMonoWidth, ctx, name) - ctx.stateHeight)),
									$elm$svg$Svg$Attributes$fontFamily('Monaco, monospace'),
									$elm$svg$Svg$Attributes$class(
									A2(
										$author$project$Diagram$toColorString,
										_Utils_update(
											clr,
											{
												_eval: val ? $author$project$Diagram$EvalTrue : $author$project$Diagram$EvalFalse
											}),
										'text stateNodeText'))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(name)
								]))
						]));
			case 'EvaluatedAnd':
				var f1 = formula.a;
				var f2 = formula.b;
				var timeline = formula.c;
				var view_1 = A3(
					$author$project$Diagram$viewDiagram,
					f1,
					_Utils_update(
						ctx,
						{x: ctx.x + ctx.blockSize}),
					clr);
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				var view_2 = A3(
					$author$project$Diagram$viewDiagram,
					f2,
					_Utils_update(
						ctx,
						{x: ctx.x + ctx.blockSize, y: (ctx.y + f1_bb.y) + ctx.blockSize}),
					clr);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var estimated_f2 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f2),
					ctx.time).value;
				var estimated_f1 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f1),
					ctx.time).value;
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A7(
							$author$project$Diagram$mergeConnector,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f1)
								}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							ctx.y + (0.5 * f1_bb.y),
							((ctx.y + f1_bb.y) + ctx.blockSize) + (0.5 * f2_bb.y),
							ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y)),
							ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y))),
							A3(
							$author$project$Diagram$andGlyph,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y))),
							view_1,
							view_2
						]));
			case 'EvaluatedOr':
				var f1 = formula.a;
				var f2 = formula.b;
				var timeline = formula.c;
				var view_1 = A3(
					$author$project$Diagram$viewDiagram,
					f1,
					_Utils_update(
						ctx,
						{x: ctx.x + ctx.blockSize}),
					clr);
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				var view_2 = A3(
					$author$project$Diagram$viewDiagram,
					f2,
					_Utils_update(
						ctx,
						{x: ctx.x + ctx.blockSize, y: (ctx.y + f1_bb.y) + ctx.blockSize}),
					clr);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var estimated_f2 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f2),
					ctx.time).value;
				var estimated_f1 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f1),
					ctx.time).value;
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A7(
							$author$project$Diagram$mergeConnector,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f1)
								}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							ctx.y + (0.5 * f1_bb.y),
							((ctx.y + f1_bb.y) + ctx.blockSize) + (0.5 * f2_bb.y),
							(ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y))) - (0.2 * ctx.glyphSize.height),
							(ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y))) + (0.2 * ctx.glyphSize.height)),
							A5(
							$author$project$Diagram$orGlyph,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f1)
								}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y))),
							view_1,
							view_2
						]));
			case 'EvaluatedImplies':
				var f1 = formula.a;
				var f2 = formula.b;
				var timeline = formula.c;
				var view_1 = A3(
					$author$project$Diagram$viewDiagram,
					f1,
					_Utils_update(
						ctx,
						{x: ctx.x + ctx.blockSize}),
					_Utils_update(
						clr,
						{tgt: $author$project$Diagram$Neutral}));
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var estimated_f2 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f2),
					ctx.time).value;
				var estimated_f1 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f1),
					ctx.time).value;
				var view_2 = A3(
					$author$project$Diagram$viewDiagram,
					f2,
					_Utils_update(
						ctx,
						{x: ctx.x + ctx.blockSize, y: (ctx.y + f1_bb.y) + ctx.blockSize}),
					_Utils_update(
						clr,
						{
							_eval: (!estimated_f1) ? $author$project$Diagram$Undetermined : clr._eval
						}));
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A6(
							$author$project$Diagram$curvedConnector,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f1)
								}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							ctx.y + (0.5 * f1_bb.y),
							ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y)),
							((ctx.y + f1_bb.y) + ctx.blockSize) + (0.5 * f2_bb.y)),
							A5(
							$author$project$Diagram$impliesGlyph,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f1)
								}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							ctx.y + (0.5 * ((f1_bb.y + ctx.blockSize) + f2_bb.y))),
							view_1,
							view_2
						]));
			case 'EvaluatedNot':
				var f = formula.a;
				var timeline = formula.b;
				var view_f = A3(
					$author$project$Diagram$viewDiagram,
					f,
					_Utils_update(
						ctx,
						{x: (ctx.x + (0.25 * ctx.blockSize)) + ctx.glyphSize.width}),
					_Utils_update(
						clr,
						{
							tgt: _Utils_eq(clr.tgt, $author$project$Diagram$TrueGood) ? $author$project$Diagram$FalseGood : $author$project$Diagram$TrueGood
						}));
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$author$project$Diagram$notGlyph,
							_Utils_update(
								ctx,
								{y: (ctx.y + (0.5 * f_bb.y)) - (0.5 * ctx.glyphSize.width)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this),
									tgt: _Utils_eq(clr.tgt, $author$project$Diagram$TrueGood) ? $author$project$Diagram$FalseGood : $author$project$Diagram$TrueGood
								})),
							view_f
						]));
			case 'EvaluatedEventually':
				var f = formula.a;
				var timeline = formula.b;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var bracketHeight = ctx.blockSize + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize);
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A3($author$project$Diagram$openBracket, ctx, clr, bracketHeight),
							A3(
							$author$project$Diagram$closedBracket,
							_Utils_update(
								ctx,
								{x: (ctx.x + f_bb.x) + (2.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A5(
							$author$project$Diagram$eventuallyBacketLine,
							_Utils_update(
								ctx,
								{x: ctx.x + (0.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight,
							f_bb.x + (3 * ctx.blockSize),
							ctx.x),
							A3(
							$author$project$Diagram$viewDiagram,
							f,
							_Utils_update(
								ctx,
								{x: ctx.x + (2 * ctx.blockSize), y: ctx.y - (0.5 * (f_bb.y - bracketHeight))}),
							clr)
						]));
			case 'EvaluatedGlobally':
				var f = formula.a;
				var timeline = formula.b;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var bracketHeight = ctx.blockSize + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize);
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$author$project$Diagram$openBracket,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A3(
							$author$project$Diagram$closedBracket,
							_Utils_update(
								ctx,
								{x: (ctx.x + f_bb.x) + (2.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A4(
							$author$project$Diagram$globallyBracketLine,
							_Utils_update(
								ctx,
								{x: ctx.x + (0.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight,
							f_bb.x + (2.5 * ctx.blockSize)),
							A3(
							$author$project$Diagram$viewDiagram,
							f,
							_Utils_update(
								ctx,
								{x: ctx.x + (2 * ctx.blockSize), y: ctx.y - (0.5 * (f_bb.y - bracketHeight))}),
							clr)
						]));
			case 'EvaluatedUntil':
				var f1 = formula.a;
				var f2 = formula.b;
				var timeline = formula.c;
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var estimated_f2 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f2),
					ctx.time).value;
				var estimated_f1 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f1),
					ctx.time).value;
				var bracketHeight = ctx.blockSize + A2(
					$elm$core$Basics$max,
					A2($elm$core$Basics$max, f1_bb.y, f2_bb.y),
					2 * ctx.blockSize);
				var view_1 = A3(
					$author$project$Diagram$viewDiagram,
					f1,
					_Utils_update(
						ctx,
						{x: ctx.x + (2 * ctx.blockSize), y: ctx.y - (0.5 * (f1_bb.y - bracketHeight))}),
					clr);
				var view_2 = A3(
					$author$project$Diagram$viewDiagram,
					f2,
					_Utils_update(
						ctx,
						{x: (ctx.x + f1_bb.x) + (3 * ctx.blockSize), y: ctx.y - (0.5 * (f2_bb.y - bracketHeight))}),
					clr);
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$author$project$Diagram$openBracket,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A3(
							$author$project$Diagram$closedBracket,
							_Utils_update(
								ctx,
								{x: ((ctx.x + f1_bb.x) + f2_bb.x) + (3.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A4(
							$author$project$Diagram$globallyBracketLine,
							_Utils_update(
								ctx,
								{x: ctx.x + (0.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f1)
								}),
							bracketHeight,
							f1_bb.x + (2.0 * ctx.blockSize)),
							A5(
							$author$project$Diagram$eventuallyBacketLine,
							_Utils_update(
								ctx,
								{x: (ctx.x + f1_bb.x) + (1.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							bracketHeight,
							(f1_bb.x + f2_bb.x) + (4 * ctx.blockSize),
							ctx.x),
							A3(
							$author$project$Diagram$untilConnectorLine,
							_Utils_update(
								ctx,
								{x: (ctx.x + f1_bb.x) + (2.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							bracketHeight),
							view_1,
							view_2
						]));
			case 'EvaluatedBoundedEventually':
				var f = formula.a;
				var bound = formula.b;
				var unit = formula.c;
				var timeline = formula.d;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var bracketHeight = ctx.blockSize + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize);
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$author$project$Diagram$openBracket,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A3(
							$author$project$Diagram$closedBracket,
							_Utils_update(
								ctx,
								{x: (ctx.x + f_bb.x) + (2.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A5(
							$author$project$Diagram$eventuallyBacketLine,
							_Utils_update(
								ctx,
								{x: ctx.x + (0.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight,
							f_bb.x + (3 * ctx.blockSize),
							ctx.x),
							A3(
							$author$project$Diagram$viewDiagram,
							f,
							_Utils_update(
								ctx,
								{x: ctx.x + (2 * ctx.blockSize), y: ctx.y - (0.5 * (f_bb.y - bracketHeight))}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								})),
							A3(
							$author$project$Diagram$bracketBoundText,
							_Utils_update(
								ctx,
								{x: (ctx.x + f_bb.x) + (3 * ctx.blockSize), y: (ctx.y + bracketHeight) + ctx.stateHeight}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							$elm$core$String$fromFloat(bound) + (' ' + $author$project$Formula$timeUnitString(unit)))
						]));
			case 'EvaluatedBoundedGlobally':
				var f = formula.a;
				var bound = formula.b;
				var unit = formula.c;
				var timeline = formula.d;
				var f_bb = A2($author$project$Diagram$boundingBox, ctx, f);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var bracketHeight = ctx.blockSize + A2($elm$core$Basics$max, f_bb.y, 2 * ctx.blockSize);
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$author$project$Diagram$openBracket,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A3(
							$author$project$Diagram$closedBracket,
							_Utils_update(
								ctx,
								{x: (ctx.x + f_bb.x) + (2.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A4(
							$author$project$Diagram$globallyBracketLine,
							_Utils_update(
								ctx,
								{x: ctx.x + (0.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight,
							f_bb.x + (2.5 * ctx.blockSize)),
							A3(
							$author$project$Diagram$viewDiagram,
							f,
							_Utils_update(
								ctx,
								{x: ctx.x + (2 * ctx.blockSize), y: ctx.y - (0.5 * (f_bb.y - bracketHeight))}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								})),
							A3(
							$author$project$Diagram$bracketBoundText,
							_Utils_update(
								ctx,
								{x: (ctx.x + f_bb.x) + (3 * ctx.blockSize), y: (ctx.y + bracketHeight) + ctx.stateHeight}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							$elm$core$String$fromFloat(bound) + (' ' + $author$project$Formula$timeUnitString(unit)))
						]));
			default:
				var f1 = formula.a;
				var f2 = formula.b;
				var bound = formula.c;
				var unit = formula.d;
				var timeline = formula.e;
				var f2_bb = A2($author$project$Diagram$boundingBox, ctx, f2);
				var f1_bb = A2($author$project$Diagram$boundingBox, ctx, f1);
				var estimated_this = A2($author$project$Data$estimate, timeline, ctx.time).value;
				var estimated_f2 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f2),
					ctx.time).value;
				var estimated_f1 = A2(
					$author$project$Data$estimate,
					$author$project$Formula$getFormulaTimeline(f1),
					ctx.time).value;
				var bracketHeight = ctx.blockSize + A2(
					$elm$core$Basics$max,
					A2($elm$core$Basics$max, f1_bb.y, f2_bb.y),
					2 * ctx.blockSize);
				var view_1 = A3(
					$author$project$Diagram$viewDiagram,
					f1,
					_Utils_update(
						ctx,
						{x: ctx.x + (2 * ctx.blockSize), y: ctx.y - (0.5 * (f1_bb.y - bracketHeight))}),
					_Utils_update(
						clr,
						{
							_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
						}));
				var view_2 = A3(
					$author$project$Diagram$viewDiagram,
					f2,
					_Utils_update(
						ctx,
						{x: (ctx.x + f1_bb.x) + (3 * ctx.blockSize), y: ctx.y - (0.5 * (f2_bb.y - bracketHeight))}),
					_Utils_update(
						clr,
						{
							_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
						}));
				return A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$author$project$Diagram$openBracket,
							ctx,
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A3(
							$author$project$Diagram$closedBracket,
							_Utils_update(
								ctx,
								{x: ((ctx.x + f1_bb.x) + f2_bb.x) + (3.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							bracketHeight),
							A4(
							$author$project$Diagram$globallyBracketLine,
							_Utils_update(
								ctx,
								{x: ctx.x + (0.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f1)
								}),
							bracketHeight,
							f1_bb.x + (2.0 * ctx.blockSize)),
							A5(
							$author$project$Diagram$eventuallyBacketLine,
							_Utils_update(
								ctx,
								{x: (ctx.x + f1_bb.x) + (1.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							bracketHeight,
							(f1_bb.x + f2_bb.x) + (4 * ctx.blockSize),
							ctx.x),
							A3(
							$author$project$Diagram$untilConnectorLine,
							_Utils_update(
								ctx,
								{x: (ctx.x + f1_bb.x) + (2.5 * ctx.blockSize)}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_f2)
								}),
							bracketHeight),
							A3(
							$author$project$Diagram$bracketBoundText,
							_Utils_update(
								ctx,
								{x: ((ctx.x + f1_bb.x) + f2_bb.x) + (4 * ctx.blockSize), y: (ctx.y + bracketHeight) + ctx.stateHeight}),
							_Utils_update(
								clr,
								{
									_eval: A2($author$project$Diagram$getEvalContext, clr._eval, estimated_this)
								}),
							$elm$core$String$fromFloat(bound) + (' ' + $author$project$Formula$timeUnitString(unit))),
							view_1,
							view_2
						]));
		}
	});
var $author$project$Diagram$viewCanvas = F5(
	function (formula, model, width, height, zoom) {
		var ruleEvaluation = A2(
			$author$project$Data$estimate,
			$author$project$Formula$getFormulaTimeline(formula),
			model.currentTime).value;
		return A2(
			$elm$svg$Svg$svg,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$x('0'),
						$elm$svg$Svg$Attributes$y('0'),
						$elm$svg$Svg$Attributes$width('100%'),
						$elm$svg$Svg$Attributes$height('100%'),
						$elm$svg$Svg$Attributes$viewBox(
						'0 0 ' + ($elm$core$String$fromFloat(width) + (' ' + $elm$core$String$fromFloat(height)))),
						$elm$svg$Svg$Attributes$preserveAspectRatio('xMinYMin meet'),
						$elm$svg$Svg$Attributes$class(
						'diagramCanvas ' + (((!ruleEvaluation) && (!_Utils_eq(model.viewMode, $author$project$Model$Authoring))) ? 'OverallViolation' : ''))
					]),
				A2(
					$elm$core$List$cons,
					A2($gampleman$elm_visualization$Zoom$onDoubleClick, zoom, $author$project$Model$ResetZoom),
					A2(
						$elm$core$List$cons,
						A2($gampleman$elm_visualization$Zoom$onWheel, zoom, $author$project$Model$ZoomMsg),
						_Utils_ap(
							A2($gampleman$elm_visualization$Zoom$onDrag, zoom, $author$project$Model$ZoomMsg),
							_Utils_ap(
								A2($gampleman$elm_visualization$Zoom$onGesture, zoom, $author$project$Model$ZoomMsg),
								A2($gampleman$elm_visualization$Zoom$onTouch, zoom, $author$project$Model$ZoomMsg)))))),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$gampleman$elm_visualization$Zoom$transform(zoom)
						]),
					_List_fromArray(
						[
							A3(
							$author$project$Diagram$viewDiagram,
							formula,
							{
								blockSize: 20,
								glyphSize: {height: 20, width: 10},
								simulation: model.simulation,
								stateHeight: 10,
								time: model.currentTime,
								x: 10,
								y: 10
							},
							{
								colored: function () {
									var _v0 = model.viewMode;
									switch (_v0.$) {
										case 'Authoring':
											return false;
										case 'Validation':
											return true;
										default:
											return true;
									}
								}(),
								_eval: ruleEvaluation ? $author$project$Diagram$EvalTrue : $author$project$Diagram$EvalFalse,
								tgt: $author$project$Diagram$TrueGood,
								violation: !ruleEvaluation
							})
						]))
				]));
	});
var $author$project$Model$CodeChange = function (a) {
	return {$: 'CodeChange', a: a};
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$property = $elm$virtual_dom$VirtualDom$property;
var $author$project$Main$viewEditor = function (model) {
	return A3(
		$elm$html$Html$node,
		'code-editor',
		_List_fromArray(
			[
				A2(
				$elm$html$Html$Attributes$property,
				'editorValue',
				$elm$json$Json$Encode$string(model.input)),
				A2(
				$elm$html$Html$Events$on,
				'editorChanged',
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Model$CodeChange,
					A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['target', 'editorValue']),
						$elm$json$Json$Decode$string))),
				A2(
				$elm$html$Html$Events$on,
				'parse',
				$elm$json$Json$Decode$succeed($author$project$Model$Parse))
			]),
		_List_Nil);
};
var $author$project$Model$UpdateCognizantEngineer = function (a) {
	return {$: 'UpdateCognizantEngineer', a: a};
};
var $author$project$Model$UpdateDescription = function (a) {
	return {$: 'UpdateDescription', a: a};
};
var $author$project$Model$UpdateFlightRuleID = function (a) {
	return {$: 'UpdateFlightRuleID', a: a};
};
var $author$project$Model$UpdateFlightRuleTitle = function (a) {
	return {$: 'UpdateFlightRuleTitle', a: a};
};
var $author$project$Model$UpdateLevel = function (a) {
	return {$: 'UpdateLevel', a: a};
};
var $author$project$Model$UpdateOperationalCategory = function (a) {
	return {$: 'UpdateOperationalCategory', a: a};
};
var $author$project$Model$UpdateRationale = function (a) {
	return {$: 'UpdateRationale', a: a};
};
var $author$project$Model$UpdateViolationImpact = function (a) {
	return {$: 'UpdateViolationImpact', a: a};
};
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $author$project$Main$viewHelixData = function (model) {
	return _Utils_eq(model.viewMode, $author$project$Model$Authoring) ? A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('Helix')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Operational Category:  ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateOperationalCategory)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.operationalCategory)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Level:                 ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateLevel)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.level)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Cognizant Engineer:    ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateCognizantEngineer)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.cognizantEngineer)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Flight Rule ID:        ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateFlightRuleID)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.flightRuleID)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Flight Rule Title:     ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateFlightRuleTitle)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.flightRuleTitle)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Description:           ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateDescription)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.description)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Rationale:             ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateRationale)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.rationale)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Violation Impact:      ')
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left'),
								$elm$html$Html$Events$onInput($author$project$Model$UpdateViolationImpact)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.violationImpact)
							]))
					]))
			])) : A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('Helix')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Operational Category:  ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.operationalCategory)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Level:                 ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.level)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Cognizant Engineer:    ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.cognizantEngineer)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Flight Rule ID:        ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.flightRuleID)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Flight Rule Title:     ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.flightRuleTitle)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Description:           ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.description)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Rationale:             ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.rationale)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'table-row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'width', '50%'),
								A2($elm$html$Html$Attributes$style, 'vertical-align', 'top')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Violation Impact:      ')
							])),
						A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'table-cell'),
								A2($elm$html$Html$Attributes$style, 'text-align', 'left')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.helixData.violationImpact)
							]))
					]))
			]));
};
var $author$project$Timeline$viewTimeline = F3(
	function (model, label, timeline) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('TimelineContainer')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('TimelineLabel')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('\t' + label)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('TimelineView')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$svg,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$x('0'),
											$elm$svg$Svg$Attributes$y('0'),
											$elm$svg$Svg$Attributes$width('100%'),
											$elm$svg$Svg$Attributes$height('30'),
											$elm$svg$Svg$Attributes$viewBox('0 0 100 100'),
											$elm$svg$Svg$Attributes$preserveAspectRatio('none'),
											$elm$svg$Svg$Attributes$class('TimelineSVG')
										]),
									A2(
										$elm$core$List$cons,
										function () {
											var est = A2($author$project$Data$estimate, timeline, model.currentTime);
											var coloringString = est.value ? 'EstimateTrue' : 'EstimateFalse';
											return A2(
												$elm$svg$Svg$line,
												_List_fromArray(
													[
														$elm$svg$Svg$Attributes$class('ScrubberResidual ' + (label + (' ' + coloringString))),
														$elm$svg$Svg$Attributes$x1(
														$elm$core$String$fromFloat(
															A2($author$project$Timeline$scale, model, model.currentTime))),
														$elm$svg$Svg$Attributes$x2(
														$elm$core$String$fromFloat(
															A2(
																$author$project$Timeline$scale,
																model,
																$elm$time$Time$millisToPosix(
																	function (s) {
																		return $elm$time$Time$posixToMillis(model.currentTime) - s;
																	}(est.residualMilliSec))))),
														$elm$svg$Svg$Attributes$y1('50'),
														$elm$svg$Svg$Attributes$y2('50')
													]),
												_List_Nil);
										}(),
										_Utils_ap(
											A2(
												$elm$core$List$map,
												function (point) {
													return A2(
														$elm$svg$Svg$rect,
														_List_fromArray(
															[
																$elm$svg$Svg$Attributes$x(
																$elm$core$String$fromFloat(
																	A2($author$project$Timeline$scale, model, point.time))),
																$elm$svg$Svg$Attributes$width(
																$elm$core$String$fromFloat(100 / $author$project$Timeline$resolution)),
																$elm$svg$Svg$Attributes$y('25'),
																$elm$svg$Svg$Attributes$height('50'),
																$elm$svg$Svg$Attributes$class(
																label + (' ' + (point.value ? 'EstimateTrue' : 'EstimateFalse')))
															]),
														_List_Nil);
												},
												timeline),
											_List_fromArray(
												[
													A2(
													$elm$svg$Svg$line,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$class('ScrubberLine'),
															$elm$svg$Svg$Attributes$x1(
															$elm$core$String$fromFloat(
																A2($author$project$Timeline$scale, model, model.currentTime))),
															$elm$svg$Svg$Attributes$x2(
															$elm$core$String$fromFloat(
																A2($author$project$Timeline$scale, model, model.currentTime))),
															$elm$svg$Svg$Attributes$y1('0'),
															$elm$svg$Svg$Attributes$y2('100')
														]),
													_List_Nil)
												]))))
								]))
						]))
				]));
	});
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$ul,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$li,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'font-size', '16px'),
								A2($elm$html$Html$Attributes$style, 'float', 'left'),
								A2($elm$html$Html$Attributes$style, 'letter-spacing', '4px')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('MORGANA | FLIGHT RULES STUDIO')
							])),
						A2(
						$elm$html$Html$select,
						_List_fromArray(
							[
								$elm$html$Html$Events$onInput($author$project$Main$decodeViewModeSelector),
								A2($elm$html$Html$Attributes$style, 'float', 'left')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$option,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$selected(
										_Utils_eq(model.viewMode, $author$project$Model$Authoring))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Authoring')
									])),
								A2(
								$elm$html$Html$option,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$selected(
										_Utils_eq(model.viewMode, $author$project$Model$Validation))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Validation')
									])),
								A2(
								$elm$html$Html$option,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$selected(
										_Utils_eq(model.viewMode, $author$project$Model$Evaluation))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Evaluation')
									]))
							])),
						A2(
						$elm$html$Html$li,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Model$ShowLegend(true))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$a,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Legend')
									]))
							])),
						A2(
						$elm$html$Html$li,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Model$SimFileRequested)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$a,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Load Simulation')
									]))
							])),
						A2(
						$elm$html$Html$li,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Model$RuleFileRequested)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$a,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Load Rule')
									]))
							])),
						A2(
						$elm$html$Html$li,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Model$NewRule)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$a,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('New Rule')
									]))
							]))
					])),
				model.showLegend ? A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('legend-overlay'),
						$elm$html$Html$Events$onClick(
						$author$project$Model$ShowLegend(false))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$img,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$src('../flightrules/images/legend.png'),
								$elm$html$Html$Attributes$class('legend-img')
							]),
						_List_Nil)
					])) : $elm$html$Html$text(''),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('container')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('left')
							]),
						function () {
							var _v0 = model.viewMode;
							switch (_v0.$) {
								case 'Authoring':
									return _List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('canvas fullHeight'),
													A2($elm$html$Html$Attributes$style, 'height', '100%'),
													A2($elm$html$Html$Attributes$style, 'max-height', '100%')
												]),
											_List_fromArray(
												[
													A5($author$project$Diagram$viewCanvas, model.evalRule, model, $author$project$Main$canvasWidth, 2 * $author$project$Main$canvasHeight, model.zoom)
												]))
										]);
								case 'Validation':
									return _List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('canvas')
												]),
											_List_fromArray(
												[
													A5($author$project$Diagram$viewCanvas, model.evalRule, model, $author$project$Main$canvasWidth, $author$project$Main$canvasHeight, model.zoom)
												])),
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('timeline')
												]),
											_Utils_ap(
												_List_fromArray(
													[
														$author$project$Timeline$timeScrubber(model),
														A3(
														$author$project$Timeline$viewTimeline,
														model,
														'Rule',
														$author$project$Formula$getFormulaTimeline(model.evalRule))
													]),
												_Utils_ap(
													$elm$core$Dict$values(
														A2(
															$elm$core$Dict$map,
															F2(
																function (name, timeline) {
																	return A3($author$project$Timeline$viewTimeline, model, name, timeline);
																}),
															model.simulation)),
													_List_fromArray(
														[
															A2(
															$elm$html$Html$div,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$class('blank-buffer')
																]),
															_List_Nil)
														]))))
										]);
								default:
									return _List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('canvas')
												]),
											_List_fromArray(
												[
													A5($author$project$Diagram$viewCanvas, model.evalRule, model, $author$project$Main$canvasWidth, $author$project$Main$canvasHeight, model.zoom)
												])),
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('timeline')
												]),
											_List_fromArray(
												[
													$author$project$Timeline$timeScrubber(model),
													A3(
													$author$project$Timeline$viewTimeline,
													model,
													'Rule',
													$author$project$Formula$getFormulaTimeline(model.evalRule)),
													A2(
													$elm$html$Html$button,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text('Upload Simulation')
														])),
													A2(
													$elm$html$Html$div,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('blank-buffer')
														]),
													_List_Nil)
												]))
										]);
							}
						}()),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('right')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$author$project$Main$viewEditor(model)
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Model$Parse)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Parse')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Model$DownloadRule)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Download')
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('formulaText')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$pre,
										_List_Nil,
										_List_fromArray(
											[
												function () {
												var _v1 = model.parseError;
												if (_v1.$ === 'Nothing') {
													return $elm$html$Html$text(
														$author$project$Formula$formulaToString(model.output));
												} else {
													var err = _v1.a;
													return $elm$html$Html$text(
														$author$project$Formula$errorMessage(err));
												}
											}()
											]))
									])),
								$author$project$Main$viewHelixData(model),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('blank-buffer')
									]),
								_List_Nil)
							]))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));