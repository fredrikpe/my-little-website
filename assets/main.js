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

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


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
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
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

	if (typeof File === 'function' && value instanceof File)
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
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
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
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
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
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
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
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
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
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
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
	return elm$json$Json$Decode$errorToString(error);
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
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
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
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
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
				? elm$core$Result$Ok(decoder.c)
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
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

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
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

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
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
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
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

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
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
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
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

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
		r: converter,
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
	var converter = _Platform_effectManagers[name].r;

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

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

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



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done(elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done(elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done(elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? elm$http$Http$GoodStatus_ : elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return elm$core$Dict$empty;
	}

	var headers = elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? elm$core$Maybe$Just(event.total) : elm$core$Maybe$Nothing
		}))));
	});
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
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
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
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
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

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

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
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
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
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
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
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
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
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
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
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
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
var author$project$Dataset$ssbTreesUrl = 'http://data.ssb.no/api/v0/en/table/';
var author$project$Dataset$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var author$project$Dataset$leafConstructor = F2(
	function (id, text) {
		return author$project$Dataset$Leaf(
			{config: elm$core$Maybe$Nothing, id: id, text: text});
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$List$cons = _List_cons;
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Dict$foldr = F3(
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
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
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
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
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
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
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
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
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
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
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
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Dataset$leafDecoder = A3(
	elm$json$Json$Decode$map2,
	author$project$Dataset$leafConstructor,
	A2(
		elm$json$Json$Decode$map,
		function (s) {
			return A2(
				elm$core$Maybe$withDefault,
				'UNKNOWN',
				elm$core$List$head(
					A2(elm$core$String$split, ':', s)));
		},
		A2(elm$json$Json$Decode$field, 'text', elm$json$Json$Decode$string)),
	A2(elm$json$Json$Decode$field, 'text', elm$json$Json$Decode$string));
var author$project$Dataset$Category = F2(
	function (a, b) {
		return {$: 'Category', a: a, b: b};
	});
var author$project$Dataset$categoryConstructor = F3(
	function (id, text, subTree) {
		return A2(
			author$project$Dataset$Category,
			{id: id, isHidden: false, text: text},
			subTree);
	});
var elm$json$Json$Decode$map3 = _Json_map3;
var elm$json$Json$Decode$succeed = _Json_succeed;
var author$project$Dataset$subListDecoder = function (id) {
	return A4(
		elm$json$Json$Decode$map3,
		author$project$Dataset$categoryConstructor,
		A2(
			elm$json$Json$Decode$map,
			function (s) {
				return id + ('/' + s);
			},
			A2(elm$json$Json$Decode$field, 'id', elm$json$Json$Decode$string)),
		A2(elm$json$Json$Decode$field, 'text', elm$json$Json$Decode$string),
		elm$json$Json$Decode$succeed(_List_Nil));
};
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$fail = _Json_fail;
var author$project$Dataset$treeDecoder = function (id) {
	return A2(
		elm$json$Json$Decode$andThen,
		function (t) {
			switch (t) {
				case 'l':
					return author$project$Dataset$subListDecoder(id);
				case 't':
					return author$project$Dataset$leafDecoder;
				default:
					return elm$json$Json$Decode$fail('Couldn\'t decode error');
			}
		},
		A2(elm$json$Json$Decode$field, 'type', elm$json$Json$Decode$string));
};
var elm$json$Json$Decode$list = _Json_decodeList;
var author$project$Dataset$treeListDecoder = function (id) {
	return elm$json$Json$Decode$list(
		author$project$Dataset$treeDecoder(id));
};
var elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var elm$http$Http$NetworkError = {$: 'NetworkError'};
var elm$http$Http$Timeout = {$: 'Timeout'};
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var author$project$HttpUtil$responseToResult = F2(
	function (decoder, response) {
		switch (response.$) {
			case 'GoodStatus_':
				var metadata = response.a;
				var body = response.b;
				var _n1 = A2(elm$json$Json$Decode$decodeString, decoder, body);
				if (_n1.$ === 'Ok') {
					var decoded = _n1.a;
					return elm$core$Result$Ok(decoded);
				} else {
					return elm$core$Result$Err(
						elm$http$Http$BadStatus(2));
				}
			case 'BadStatus_':
				var medatada = response.a;
				var body = response.b;
				return elm$core$Result$Err(
					elm$http$Http$BadStatus(1));
			case 'BadUrl_':
				var string = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadUrl(string));
			case 'Timeout_':
				return elm$core$Result$Err(elm$http$Http$Timeout);
			default:
				return elm$core$Result$Err(elm$http$Http$NetworkError);
		}
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
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
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (_n0.$ === 'Just') {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var elm$http$Http$Timeout_ = {$: 'Timeout_'};
var elm$http$Http$emptyBody = _Http_emptyBody;
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$http$Http$stringResolver = A2(_Http_expect, '', elm$core$Basics$identity);
var elm$core$Task$fail = _Scheduler_fail;
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$http$Http$resultToTask = function (result) {
	if (result.$ === 'Ok') {
		var a = result.a;
		return elm$core$Task$succeed(a);
	} else {
		var x = result.a;
		return elm$core$Task$fail(x);
	}
};
var elm$http$Http$task = function (r) {
	return A3(
		_Http_toTask,
		_Utils_Tuple0,
		elm$http$Http$resultToTask,
		{allowCookiesFromOtherDomains: false, body: r.body, expect: r.resolver, headers: r.headers, method: r.method, timeout: r.timeout, tracker: elm$core$Maybe$Nothing, url: r.url});
};
var author$project$HttpUtil$httpGetFromJson = F2(
	function (url, decoder) {
		return elm$http$Http$task(
			{
				body: elm$http$Http$emptyBody,
				headers: _List_Nil,
				method: 'Get',
				resolver: elm$http$Http$stringResolver(
					author$project$HttpUtil$responseToResult(decoder)),
				timeout: elm$core$Maybe$Nothing,
				url: url
			});
	});
var author$project$Dataset$getTree = function (id) {
	return A2(
		author$project$HttpUtil$httpGetFromJson,
		_Utils_ap(author$project$Dataset$ssbTreesUrl, id),
		author$project$Dataset$treeListDecoder(id));
};
var author$project$Main$GotRoot = function (a) {
	return {$: 'GotRoot', a: a};
};
var author$project$Main$defaultModel = {dataset: elm$core$Maybe$Nothing, datasetConfig: elm$core$Maybe$Nothing, errorMsg: elm$core$Maybe$Nothing, hovered: elm$core$Maybe$Nothing, query: elm$core$Maybe$Nothing, showTree: false, trees: _List_Nil};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$List$foldrHelper = F4(
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
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
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
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$onError = _Scheduler_onError;
var elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(
					elm$core$Task$onError,
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
						elm$core$Result$Err),
					A2(
						elm$core$Task$andThen,
						A2(
							elm$core$Basics$composeL,
							A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
							elm$core$Result$Ok),
						task))));
	});
var author$project$Main$init = function (_n0) {
	return _Utils_Tuple2(
		author$project$Main$defaultModel,
		A2(
			elm$core$Task$attempt,
			author$project$Main$GotRoot,
			author$project$Dataset$getTree('')));
};
var elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Dataset$queryEncoder = function (query) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'query',
				A2(
					elm$json$Json$Encode$list,
					function (v) {
						return elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'code',
									elm$json$Json$Encode$string(v.code)),
									_Utils_Tuple2(
									'selection',
									elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'filter',
												elm$json$Json$Encode$string('item')),
												_Utils_Tuple2(
												'values',
												A2(
													elm$json$Json$Encode$list,
													function (x) {
														return elm$json$Json$Encode$string(x.value);
													},
													v.values))
											])))
								]));
					},
					query.dimensions)),
				_Utils_Tuple2(
				'response',
				elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'format',
							elm$json$Json$Encode$string('json-stat2'))
						])))
			]));
};
var author$project$Dataset$queryToString = function (query) {
	return A2(
		elm$json$Json$Encode$encode,
		4,
		author$project$Dataset$queryEncoder(query));
};
var author$project$Dataset$setHidden = F3(
	function (bool, id, tree) {
		if (tree.$ === 'Category') {
			var state = tree.a;
			var list = tree.b;
			return _Utils_eq(state.id, id) ? A2(
				author$project$Dataset$Category,
				_Utils_update(
					state,
					{isHidden: bool}),
				list) : A2(
				author$project$Dataset$Category,
				state,
				A2(
					elm$core$List$map,
					A2(author$project$Dataset$setHidden, bool, id),
					list));
		} else {
			var l = tree;
			return l;
		}
	});
var author$project$Dataset$setSubTree = F3(
	function (subTree, id, tree) {
		if (tree.$ === 'Category') {
			var state = tree.a;
			var list = tree.b;
			return _Utils_eq(state.id, id) ? A2(author$project$Dataset$Category, state, subTree) : A2(
				author$project$Dataset$Category,
				state,
				A2(
					elm$core$List$map,
					A2(author$project$Dataset$setSubTree, subTree, id),
					list));
		} else {
			return tree;
		}
	});
var author$project$Main$GotSubTree = F2(
	function (a, b) {
		return {$: 'GotSubTree', a: a, b: b};
	});
var author$project$Main$errorModel = function (errorMsg) {
	return _Utils_update(
		author$project$Main$defaultModel,
		{
			errorMsg: elm$core$Maybe$Just(errorMsg)
		});
};
var author$project$Dataset$blankQuery = F2(
	function (id, config) {
		return {
			dimensions: A2(
				elm$core$List$map,
				function (v) {
					return _Utils_update(
						v,
						{values: _List_Nil});
				},
				config.dimensions),
			id: id
		};
	});
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var author$project$Dataset$helper002 = F2(
	function (keyValueList, values) {
		return {
			dimensions: A2(
				elm$core$List$map,
				function (t) {
					return {code: t.a, text: 'unknown', values: t.b};
				},
				keyValueList),
			values: values
		};
	});
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$core$List$sortBy = _List_sortBy;
var author$project$Dataset$helper001 = F2(
	function (index, label) {
		var values = A2(
			elm$core$List$map,
			function (l) {
				return {index: -1, value: l.a, valueText: l.b};
			},
			label);
		var unsorted = A2(
			elm$core$List$map,
			function (d) {
				var found = elm$core$List$head(
					A2(
						elm$core$List$filter,
						function (t) {
							return _Utils_eq(t.a, d.value);
						},
						index));
				if (found.$ === 'Just') {
					var f = found.a;
					return _Utils_update(
						d,
						{index: f.b});
				} else {
					return d;
				}
			},
			values);
		return A2(
			elm$core$List$sortBy,
			function ($) {
				return $.index;
			},
			unsorted);
	});
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var author$project$Dataset$partialDimensionDecoder = A2(
	elm$json$Json$Decode$field,
	'category',
	A3(
		elm$json$Json$Decode$map2,
		author$project$Dataset$helper001,
		A2(
			elm$json$Json$Decode$field,
			'index',
			elm$json$Json$Decode$keyValuePairs(elm$json$Json$Decode$int)),
		A2(
			elm$json$Json$Decode$field,
			'label',
			elm$json$Json$Decode$keyValuePairs(elm$json$Json$Decode$string))));
var elm$json$Json$Decode$float = _Json_decodeFloat;
var author$project$Dataset$datasetDecoder = A3(
	elm$json$Json$Decode$map2,
	author$project$Dataset$helper002,
	A2(
		elm$json$Json$Decode$field,
		'dimension',
		elm$json$Json$Decode$keyValuePairs(author$project$Dataset$partialDimensionDecoder)),
	A2(
		elm$json$Json$Decode$field,
		'value',
		elm$json$Json$Decode$list(elm$json$Json$Decode$float)));
var author$project$HttpUtil$corsAnywhere = 'https://cors-anywhere.herokuapp.com/';
var elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2(elm$json$Json$Encode$encode, 0, value));
};
var author$project$HttpUtil$httpPostFromJson = F3(
	function (url, body, decoder) {
		return elm$http$Http$task(
			{
				body: elm$http$Http$jsonBody(body),
				headers: _List_Nil,
				method: 'POST',
				resolver: elm$http$Http$stringResolver(
					author$project$HttpUtil$responseToResult(decoder)),
				timeout: elm$core$Maybe$Nothing,
				url: _Utils_ap(author$project$HttpUtil$corsAnywhere, url)
			});
	});
var author$project$Dataset$getDataset = function (query) {
	return A3(
		author$project$HttpUtil$httpPostFromJson,
		_Utils_ap(author$project$Dataset$ssbTreesUrl, query.id),
		author$project$Dataset$queryEncoder(query),
		author$project$Dataset$datasetDecoder);
};
var author$project$Dataset$Config = F2(
	function (title, dimensions) {
		return {dimensions: dimensions, title: title};
	});
var author$project$Dataset$Dimension = F3(
	function (code, text, values) {
		return {code: code, text: text, values: values};
	});
var author$project$Dataset$dimValueConstructor = F2(
	function (value, valueText) {
		return {index: -1, value: value, valueText: valueText};
	});
var author$project$Dataset$dimensionDecoder = A4(
	elm$json$Json$Decode$map3,
	author$project$Dataset$Dimension,
	A2(elm$json$Json$Decode$field, 'code', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'text', elm$json$Json$Decode$string),
	A3(
		elm$json$Json$Decode$map2,
		elm$core$List$map2(author$project$Dataset$dimValueConstructor),
		A2(
			elm$json$Json$Decode$field,
			'values',
			elm$json$Json$Decode$list(elm$json$Json$Decode$string)),
		A2(
			elm$json$Json$Decode$field,
			'valueTexts',
			elm$json$Json$Decode$list(elm$json$Json$Decode$string))));
var author$project$Dataset$leafConfigDecoder = A3(
	elm$json$Json$Decode$map2,
	author$project$Dataset$Config,
	A2(elm$json$Json$Decode$field, 'title', elm$json$Json$Decode$string),
	A2(
		elm$json$Json$Decode$field,
		'variables',
		elm$json$Json$Decode$list(author$project$Dataset$dimensionDecoder)));
var author$project$Dataset$getLeafConfig = function (id) {
	return A2(
		author$project$HttpUtil$httpGetFromJson,
		_Utils_ap(author$project$Dataset$ssbTreesUrl, id),
		author$project$Dataset$leafConfigDecoder);
};
var author$project$Dataset$setLeafConfig = F3(
	function (config, id, tree) {
		if (tree.$ === 'Category') {
			var state = tree.a;
			var list = tree.b;
			return A2(
				author$project$Dataset$Category,
				state,
				A2(
					elm$core$List$map,
					A2(author$project$Dataset$setLeafConfig, config, id),
					list));
		} else {
			var leaf = tree.a;
			return _Utils_eq(leaf.id, id) ? author$project$Dataset$Leaf(
				_Utils_update(
					leaf,
					{
						config: elm$core$Maybe$Just(config)
					})) : tree;
		}
	});
var author$project$Main$DGotConfig = F2(
	function (a, b) {
		return {$: 'DGotConfig', a: a, b: b};
	});
var author$project$Main$DGotData = function (a) {
	return {$: 'DGotData', a: a};
};
var author$project$Main$DatasetMessage = function (a) {
	return {$: 'DatasetMessage', a: a};
};
var author$project$Main$HttpError = function (a) {
	return {$: 'HttpError', a: a};
};
var author$project$Main$setQuery = F2(
	function (query, model) {
		return _Utils_update(
			model,
			{query: query});
	});
var author$project$Main$updateTree = F3(
	function (id, f, model) {
		return _Utils_update(
			model,
			{
				trees: A2(
					elm$core$List$map,
					f(id),
					model.trees)
			});
	});
var author$project$Util$replaceIf = F3(
	function (predicate, value, list) {
		return A2(
			elm$core$List$map,
			function (x) {
				return predicate(x) ? value : x;
			},
			list);
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$Debug$toString = _Debug_toString;
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Main$handleDatasetMsg = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'DGetData':
				var _n1 = model.query;
				if (_n1.$ === 'Just') {
					var q = _n1.a;
					return _Utils_Tuple2(
						model,
						A2(
							elm$core$Task$attempt,
							function (x) {
								return author$project$Main$DatasetMessage(
									author$project$Main$DGotData(x));
							},
							author$project$Dataset$getDataset(q)));
				} else {
					return _Utils_Tuple2(
						author$project$Main$errorModel('Query was Nothing when trying to get data!'),
						elm$core$Platform$Cmd$none);
				}
			case 'DGotData':
				var result = msg.a;
				if (result.$ === 'Ok') {
					var dataset = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								dataset: elm$core$Maybe$Just(dataset),
								errorMsg: elm$core$Maybe$Just(
									elm$core$Debug$toString(dataset))
							}),
						elm$core$Platform$Cmd$none);
				} else {
					var e = result.a;
					return _Utils_Tuple2(
						author$project$Main$errorModel(
							elm$core$Debug$toString(e)),
						elm$core$Platform$Cmd$none);
				}
			case 'DSetQueryDimension':
				var dimension = msg.a;
				var _n3 = model.query;
				if (_n3.$ === 'Just') {
					var q = _n3.a;
					var dims = A3(
						author$project$Util$replaceIf,
						function (d) {
							return _Utils_eq(d.code, dimension.code);
						},
						dimension,
						q.dimensions);
					return _Utils_Tuple2(
						A2(
							author$project$Main$setQuery,
							elm$core$Maybe$Just(
								_Utils_update(
									q,
									{dimensions: dims})),
							model),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						author$project$Main$errorModel('Query was Nothing when it shouldn\'t have been!'),
						elm$core$Platform$Cmd$none);
				}
			case 'DGetConfig':
				var id = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						elm$core$Task$attempt,
						function (result) {
							if (result.$ === 'Ok') {
								var config = result.a;
								return author$project$Main$DatasetMessage(
									A2(author$project$Main$DGotConfig, id, config));
							} else {
								var e = result.a;
								return author$project$Main$HttpError(e);
							}
						},
						author$project$Dataset$getLeafConfig(id)));
			case 'DGotConfig':
				var id = msg.a;
				var config = msg.b;
				var newModel = A3(
					author$project$Main$updateTree,
					id,
					author$project$Dataset$setLeafConfig(config),
					A2(
						author$project$Main$setQuery,
						elm$core$Maybe$Just(
							A2(author$project$Dataset$blankQuery, id, config)),
						model));
				return _Utils_Tuple2(
					_Utils_update(
						newModel,
						{
							datasetConfig: elm$core$Maybe$Just(config),
							showTree: false
						}),
					elm$core$Platform$Cmd$none);
			case 'DSetConfig':
				var config = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							datasetConfig: config,
							showTree: !_Utils_eq(config, elm$core$Maybe$Nothing)
						}),
					elm$core$Platform$Cmd$none);
			default:
				var _n5 = model.query;
				if (_n5.$ === 'Just') {
					var q = _n5.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								errorMsg: elm$core$Maybe$Just(
									author$project$Dataset$queryToString(q))
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						author$project$Main$errorModel('Query was Nothing when it shouldn\'t have been!'),
						elm$core$Platform$Cmd$none);
				}
		}
	});
var author$project$Main$hide = function (id) {
	return A2(
		author$project$Main$updateTree,
		id,
		author$project$Dataset$setHidden(true));
};
var author$project$Main$show = function (id) {
	return A2(
		author$project$Main$updateTree,
		id,
		author$project$Dataset$setHidden(false));
};
var elm$core$Basics$not = _Basics_not;
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Pass':
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 'ShowTree':
				var cmd = elm$core$List$isEmpty(model.trees) ? A2(
					elm$core$Task$attempt,
					author$project$Main$GotRoot,
					author$project$Dataset$getTree('')) : elm$core$Platform$Cmd$none;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{showTree: !model.showTree}),
					cmd);
			case 'Hover':
				var hovered = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{hovered: hovered}),
					elm$core$Platform$Cmd$none);
			case 'ShowStrings':
				var s = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							errorMsg: elm$core$Maybe$Just(
								A2(elm$core$String$join, '', s))
						}),
					elm$core$Platform$Cmd$none);
			case 'HttpError':
				var e = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							errorMsg: elm$core$Maybe$Just(
								elm$core$Debug$toString(e))
						}),
					elm$core$Platform$Cmd$none);
			case 'ShowQuery':
				var _n1 = model.query;
				if (_n1.$ === 'Just') {
					var q = _n1.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								errorMsg: elm$core$Maybe$Just(
									author$project$Dataset$queryToString(q))
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			case 'GetRoot':
				return _Utils_Tuple2(
					model,
					A2(
						elm$core$Task$attempt,
						author$project$Main$GotRoot,
						author$project$Dataset$getTree('')));
			case 'GetSubTree':
				var id = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						elm$core$Task$attempt,
						author$project$Main$GotSubTree(id),
						author$project$Dataset$getTree(id)));
			case 'Show':
				var id = msg.a;
				return _Utils_Tuple2(
					A2(author$project$Main$show, id, model),
					elm$core$Platform$Cmd$none);
			case 'Hide':
				var id = msg.a;
				return _Utils_Tuple2(
					A2(author$project$Main$hide, id, model),
					elm$core$Platform$Cmd$none);
			case 'DatasetMessage':
				var dMsg = msg.a;
				return A2(author$project$Main$handleDatasetMsg, dMsg, model);
			case 'GotRoot':
				var result = msg.a;
				if (result.$ === 'Ok') {
					var trees = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{showTree: !model.showTree, trees: trees}),
						elm$core$Platform$Cmd$none);
				} else {
					var e = result.a;
					return _Utils_Tuple2(
						author$project$Main$errorModel(
							elm$core$Debug$toString(e)),
						elm$core$Platform$Cmd$none);
				}
			default:
				var id = msg.a;
				var result = msg.b;
				if (result.$ === 'Ok') {
					var subTree = result.a;
					return _Utils_Tuple2(
						A3(
							author$project$Main$updateTree,
							id,
							F2(
								function (x, y) {
									return A3(
										author$project$Dataset$setSubTree,
										subTree,
										x,
										A3(author$project$Dataset$setHidden, false, x, y));
								}),
							model),
						elm$core$Platform$Cmd$none);
				} else {
					var e = result.a;
					return _Utils_Tuple2(
						author$project$Main$errorModel(
							elm$core$Debug$toString(e)),
						elm$core$Platform$Cmd$none);
				}
		}
	});
var author$project$Main$Hover = function (a) {
	return {$: 'Hover', a: a};
};
var author$project$Main$DGetData = {$: 'DGetData'};
var abadi199$elm_input_extra$MultiSelect$Option = F3(
	function (value, text, selected) {
		return {selected: selected, text: text, value: value};
	});
var elm$json$Json$Decode$bool = _Json_decodeBool;
var abadi199$elm_input_extra$MultiSelect$optionDecoder = A4(
	elm$json$Json$Decode$map3,
	abadi199$elm_input_extra$MultiSelect$Option,
	A2(elm$json$Json$Decode$field, 'value', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'text', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'selected', elm$json$Json$Decode$bool));
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var elm$json$Json$Decode$maybe = function (decoder) {
	return elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(elm$json$Json$Decode$map, elm$core$Maybe$Just, decoder),
				elm$json$Json$Decode$succeed(elm$core$Maybe$Nothing)
			]));
};
var abadi199$elm_input_extra$MultiSelect$optionsDecoder = function () {
	var loop = F2(
		function (idx, xs) {
			return A2(
				elm$json$Json$Decode$andThen,
				A2(
					elm$core$Basics$composeR,
					elm$core$Maybe$map(
						function (x) {
							return A2(
								loop,
								idx + 1,
								A2(elm$core$List$cons, x, xs));
						}),
					elm$core$Maybe$withDefault(
						elm$json$Json$Decode$succeed(xs))),
				elm$json$Json$Decode$maybe(
					A2(
						elm$json$Json$Decode$field,
						elm$core$String$fromInt(idx),
						abadi199$elm_input_extra$MultiSelect$optionDecoder)));
		});
	return A2(
		elm$json$Json$Decode$map,
		elm$core$List$reverse,
		A2(
			elm$json$Json$Decode$field,
			'options',
			A2(loop, 0, _List_Nil)));
}();
var abadi199$elm_input_extra$MultiSelect$selectedOptionsDecoder = function () {
	var filterSelected = function (options) {
		return A2(
			elm$core$List$map,
			function ($) {
				return $.value;
			},
			A2(
				elm$core$List$filter,
				function ($) {
					return $.selected;
				},
				options));
	};
	return A2(
		elm$json$Json$Decode$map,
		filterSelected,
		A2(elm$json$Json$Decode$field, 'target', abadi199$elm_input_extra$MultiSelect$optionsDecoder));
}();
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
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
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var abadi199$elm_input_extra$MultiSelect$onChange = function (tagger) {
	return A2(
		elm$html$Html$Events$on,
		'change',
		A2(elm$json$Json$Decode$map, tagger, abadi199$elm_input_extra$MultiSelect$selectedOptionsDecoder));
};
var elm$core$List$any = F2(
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
var elm$html$Html$option = _VirtualDom_node('option');
var elm$html$Html$select = _VirtualDom_node('select');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var elm$html$Html$Attributes$multiple = elm$html$Html$Attributes$boolProperty('multiple');
var elm$html$Html$Attributes$selected = elm$html$Html$Attributes$boolProperty('selected');
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var abadi199$elm_input_extra$MultiSelect$multiSelect = F3(
	function (options, attributes, currentValue) {
		var isSelected = function (value) {
			return A2(
				elm$core$List$any,
				elm$core$Basics$eq(value),
				currentValue);
		};
		var toOption = function (_n0) {
			var value = _n0.value;
			var text = _n0.text;
			var enabled = _n0.enabled;
			return A2(
				elm$html$Html$option,
				_List_fromArray(
					[
						elm$html$Html$Attributes$value(value),
						elm$html$Html$Attributes$selected(
						isSelected(value)),
						elm$html$Html$Attributes$disabled(!enabled)
					]),
				_List_fromArray(
					[
						elm$html$Html$text(text)
					]));
		};
		return A2(
			elm$html$Html$select,
			_Utils_ap(
				attributes,
				_List_fromArray(
					[
						abadi199$elm_input_extra$MultiSelect$onChange(options.onChange),
						elm$html$Html$Attributes$multiple(true)
					])),
			A2(elm$core$List$map, toOption, options.items));
	});
var abadi199$elm_input_extra$MultiSelect$defaultOptions = function (onChangeHandler) {
	return {items: _List_Nil, onChange: onChangeHandler};
};
var author$project$Main$DSetQueryDimension = function (a) {
	return {$: 'DSetQueryDimension', a: a};
};
var author$project$Util$any = F2(
	function (predicate, list) {
		return A3(
			elm$core$List$foldl,
			F2(
				function (a, b) {
					return b || predicate(a);
				}),
			false,
			list);
	});
var author$project$Main$onQueryChange = F2(
	function (dimension, s) {
		return author$project$Main$DatasetMessage(
			author$project$Main$DSetQueryDimension(
				_Utils_update(
					dimension,
					{
						values: A2(
							elm$core$List$filter,
							function (v) {
								return A2(
									author$project$Util$any,
									function (x) {
										return _Utils_eq(x, v.value);
									},
									s);
							},
							dimension.values)
					})));
	});
var author$project$Main$querySelectOptions = function (dimension) {
	var defaultOptions = abadi199$elm_input_extra$MultiSelect$defaultOptions(
		author$project$Main$onQueryChange(dimension));
	return _Utils_update(
		defaultOptions,
		{
			items: A2(
				elm$core$List$map,
				function (v) {
					return {enabled: true, text: v.valueText, value: v.value};
				},
				dimension.values)
		});
};
var author$project$Main$dimensionHtml = function (dimension) {
	return A3(
		abadi199$elm_input_extra$MultiSelect$multiSelect,
		author$project$Main$querySelectOptions(dimension),
		_List_Nil,
		_List_Nil);
};
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Main$configHtml = function (config) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('view__config_div')
			]),
		function () {
			if (config.$ === 'Just') {
				var c = config.a;
				return _Utils_ap(
					A2(
						elm$core$List$map,
						function (v) {
							return author$project$Main$dimensionHtml(v);
						},
						c.dimensions),
					_List_fromArray(
						[
							A2(
							elm$html$Html$button,
							_List_fromArray(
								[
									elm$html$Html$Events$onClick(
									author$project$Main$DatasetMessage(author$project$Main$DGetData))
								]),
							_List_fromArray(
								[
									elm$html$Html$text('Show graph')
								]))
						]));
			} else {
				return _List_fromArray(
					[
						elm$html$Html$text('')
					]);
			}
		}());
};
var avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var avh4$elm_color$Color$blue = A4(avh4$elm_color$Color$RgbaSpace, 52 / 255, 101 / 255, 164 / 255, 1.0);
var avh4$elm_color$Color$brown = A4(avh4$elm_color$Color$RgbaSpace, 193 / 255, 125 / 255, 17 / 255, 1.0);
var avh4$elm_color$Color$green = A4(avh4$elm_color$Color$RgbaSpace, 115 / 255, 210 / 255, 22 / 255, 1.0);
var avh4$elm_color$Color$lightBlue = A4(avh4$elm_color$Color$RgbaSpace, 114 / 255, 159 / 255, 207 / 255, 1.0);
var avh4$elm_color$Color$lightBrown = A4(avh4$elm_color$Color$RgbaSpace, 233 / 255, 185 / 255, 110 / 255, 1.0);
var avh4$elm_color$Color$lightGreen = A4(avh4$elm_color$Color$RgbaSpace, 138 / 255, 226 / 255, 52 / 255, 1.0);
var avh4$elm_color$Color$lightOrange = A4(avh4$elm_color$Color$RgbaSpace, 252 / 255, 175 / 255, 62 / 255, 1.0);
var avh4$elm_color$Color$lightPurple = A4(avh4$elm_color$Color$RgbaSpace, 173 / 255, 127 / 255, 168 / 255, 1.0);
var avh4$elm_color$Color$lightRed = A4(avh4$elm_color$Color$RgbaSpace, 239 / 255, 41 / 255, 41 / 255, 1.0);
var avh4$elm_color$Color$lightYellow = A4(avh4$elm_color$Color$RgbaSpace, 255 / 255, 233 / 255, 79 / 255, 1.0);
var avh4$elm_color$Color$orange = A4(avh4$elm_color$Color$RgbaSpace, 245 / 255, 121 / 255, 0 / 255, 1.0);
var avh4$elm_color$Color$purple = A4(avh4$elm_color$Color$RgbaSpace, 117 / 255, 80 / 255, 123 / 255, 1.0);
var avh4$elm_color$Color$red = A4(avh4$elm_color$Color$RgbaSpace, 204 / 255, 0 / 255, 0 / 255, 1.0);
var avh4$elm_color$Color$yellow = A4(avh4$elm_color$Color$RgbaSpace, 237 / 255, 212 / 255, 0 / 255, 1.0);
var author$project$Chart$colors = _List_fromArray(
	[avh4$elm_color$Color$red, avh4$elm_color$Color$orange, avh4$elm_color$Color$yellow, avh4$elm_color$Color$green, avh4$elm_color$Color$blue, avh4$elm_color$Color$purple, avh4$elm_color$Color$brown, avh4$elm_color$Color$lightRed, avh4$elm_color$Color$lightOrange, avh4$elm_color$Color$lightYellow, avh4$elm_color$Color$lightGreen, avh4$elm_color$Color$lightBlue, avh4$elm_color$Color$lightPurple, avh4$elm_color$Color$lightBrown]);
var author$project$Chart$names = _List_fromArray(
	['a', 'b', 'c', 'd', 'e', 'f', 'g']);
var avh4$elm_color$Color$black = A4(avh4$elm_color$Color$RgbaSpace, 0 / 255, 0 / 255, 0 / 255, 1.0);
var terezka$line_charts$Internal$Axis$Tick$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Axis$Tick$custom = terezka$line_charts$Internal$Axis$Tick$Config;
var terezka$line_charts$LineChart$Axis$Tick$custom = terezka$line_charts$Internal$Axis$Tick$custom;
var terezka$line_charts$Internal$Axis$Tick$Positive = {$: 'Positive'};
var terezka$line_charts$LineChart$Axis$Tick$positive = terezka$line_charts$Internal$Axis$Tick$Positive;
var elm$core$Basics$round = _Basics_round;
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var elm$core$String$fromFloat = _String_fromNumber;
var avh4$elm_color$Color$toCssString = function (_n0) {
	var r = _n0.a;
	var g = _n0.b;
	var b = _n0.c;
	var a = _n0.d;
	var roundTo = function (x) {
		return elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return elm$core$Basics$round(x * 10000) / 100;
	};
	return elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				elm$core$String$fromFloat(
				pct(r)),
				'%,',
				elm$core$String$fromFloat(
				pct(g)),
				'%,',
				elm$core$String$fromFloat(
				pct(b)),
				'%,',
				elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var elm$svg$Svg$text = elm$virtual_dom$VirtualDom$text;
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$text_ = elm$svg$Svg$trustedNode('text');
var elm$svg$Svg$tspan = elm$svg$Svg$trustedNode('tspan');
var elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var terezka$line_charts$Internal$Svg$label = F2(
	function (color, string) {
		return A2(
			elm$svg$Svg$text_,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$fill(color),
					elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$tspan,
					_List_Nil,
					_List_fromArray(
						[
							elm$svg$Svg$text(string)
						]))
				]));
	});
var terezka$line_charts$LineChart$Junk$label = function (color) {
	return terezka$line_charts$Internal$Svg$label(
		avh4$elm_color$Color$toCssString(color));
};
var author$project$Chart$customTick = F2(
	function (toString, number) {
		var label = A2(
			terezka$line_charts$LineChart$Junk$label,
			avh4$elm_color$Color$black,
			toString(number));
		return terezka$line_charts$LineChart$Axis$Tick$custom(
			{
				color: avh4$elm_color$Color$black,
				direction: terezka$line_charts$LineChart$Axis$Tick$positive,
				grid: true,
				label: elm$core$Maybe$Just(label),
				length: 7,
				position: number,
				width: 1
			});
	});
var terezka$line_charts$Internal$Axis$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Axis$custom = terezka$line_charts$Internal$Axis$Config;
var terezka$line_charts$LineChart$Axis$custom = terezka$line_charts$Internal$Axis$custom;
var terezka$line_charts$Internal$Axis$Line$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Axis$Line$custom = terezka$line_charts$Internal$Axis$Line$Config;
var terezka$line_charts$Internal$Axis$Line$full = function (color) {
	return terezka$line_charts$Internal$Axis$Line$custom(
		F2(
			function (data, range) {
				return {color: color, end: range.max, events: _List_Nil, start: range.min, width: 1};
			}));
};
var terezka$line_charts$LineChart$Axis$Line$full = terezka$line_charts$Internal$Axis$Line$full;
var terezka$line_charts$Internal$Axis$Range$Padded = F2(
	function (a, b) {
		return {$: 'Padded', a: a, b: b};
	});
var terezka$line_charts$Internal$Axis$Range$padded = terezka$line_charts$Internal$Axis$Range$Padded;
var terezka$line_charts$LineChart$Axis$Range$padded = terezka$line_charts$Internal$Axis$Range$padded;
var terezka$line_charts$Internal$Axis$Ticks$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Axis$Ticks$custom = terezka$line_charts$Internal$Axis$Ticks$Config;
var terezka$line_charts$Internal$Axis$Values$Around = function (a) {
	return {$: 'Around', a: a};
};
var terezka$line_charts$Internal$Axis$Values$around = terezka$line_charts$Internal$Axis$Values$Around;
var terezka$line_charts$Internal$Axis$Values$ceilingTo = F2(
	function (prec, number) {
		return prec * elm$core$Basics$ceiling(number / prec);
	});
var terezka$line_charts$Internal$Axis$Values$getBeginning = F2(
	function (min, interval) {
		var multiple = min / interval;
		return _Utils_eq(
			multiple,
			elm$core$Basics$round(multiple)) ? min : A2(terezka$line_charts$Internal$Axis$Values$ceilingTo, interval, min);
	});
var elm$core$Basics$ge = _Utils_ge;
var elm$core$String$toFloat = _String_toFloat;
var elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var elm$core$Basics$isInfinite = _Basics_isInfinite;
var elm$core$Basics$isNaN = _Basics_isNaN;
var elm$core$String$length = _String_length;
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)));
	});
var elm$core$String$reverse = _String_reverse;
var elm$core$String$slice = _String_slice;
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			elm$core$List$any,
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('0'))) && (!_Utils_eq(
					c,
					_Utils_chr('.')));
			},
			elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var elm$core$Char$fromCode = _Char_fromCode;
var myrho$elm_round$Round$increaseNum = function (_n0) {
	var head = _n0.a;
	var tail = _n0.b;
	if (_Utils_eq(
		head,
		_Utils_chr('9'))) {
		var _n1 = elm$core$String$uncons(tail);
		if (_n1.$ === 'Nothing') {
			return '01';
		} else {
			var headtail = _n1.a;
			return A2(
				elm$core$String$cons,
				_Utils_chr('0'),
				myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			elm$core$String$cons,
			elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var myrho$elm_round$Round$splitComma = function (str) {
	var _n0 = A2(elm$core$String$split, '.', str);
	if (_n0.b) {
		if (_n0.b.b) {
			var before = _n0.a;
			var _n1 = _n0.b;
			var after = _n1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _n0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$core$String$toInt = _String_toInt;
var elm$core$Tuple$mapFirst = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var myrho$elm_round$Round$toDecimal = function (fl) {
	var _n0 = A2(
		elm$core$String$split,
		'e',
		elm$core$String$fromFloat(
			elm$core$Basics$abs(fl)));
	if (_n0.b) {
		if (_n0.b.b) {
			var num = _n0.a;
			var _n1 = _n0.b;
			var exp = _n1.a;
			var e = A2(
				elm$core$Maybe$withDefault,
				0,
				elm$core$String$toInt(
					A2(elm$core$String$startsWith, '+', exp) ? A2(elm$core$String$dropLeft, 1, exp) : exp));
			var _n2 = myrho$elm_round$Round$splitComma(num);
			var before = _n2.a;
			var after = _n2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				elm$core$Maybe$withDefault,
				'0',
				A2(
					elm$core$Maybe$map,
					function (_n3) {
						var a = _n3.a;
						var b = _n3.b;
						return a + ('.' + b);
					},
					A2(
						elm$core$Maybe$map,
						elm$core$Tuple$mapFirst(elm$core$String$fromChar),
						elm$core$String$uncons(
							_Utils_ap(
								A2(
									elm$core$String$repeat,
									elm$core$Basics$abs(e),
									'0'),
								total))))) : A3(
				elm$core$String$padRight,
				e + 1,
				_Utils_chr('0'),
				total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _n0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if (elm$core$Basics$isInfinite(fl) || elm$core$Basics$isNaN(fl)) {
			return elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _n0 = myrho$elm_round$Round$splitComma(
				myrho$elm_round$Round$toDecimal(
					elm$core$Basics$abs(fl)));
			var before = _n0.a;
			var after = _n0.b;
			var r = elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2(elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					elm$core$String$padRight,
					r,
					_Utils_chr('0'),
					_Utils_ap(before, after)));
			var totalLen = elm$core$String$length(normalized);
			var roundDigitIndex = A2(elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3(elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3(elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? elm$core$String$reverse(
				A2(
					elm$core$Maybe$withDefault,
					'1',
					A2(
						elm$core$Maybe$map,
						myrho$elm_round$Round$increaseNum,
						elm$core$String$uncons(
							elm$core$String$reverse(remains))))) : remains;
			var numLen = elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					elm$core$String$repeat,
					elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				elm$core$String$length(after)) < 0) ? (A3(elm$core$String$slice, 0, numLen - s, num) + ('.' + A3(elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3(
					elm$core$String$padRight,
					s,
					_Utils_chr('0'),
					after))));
			return A2(myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var myrho$elm_round$Round$round = myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _n0 = elm$core$String$uncons(str);
			if (_n0.$ === 'Nothing') {
				return false;
			} else {
				if ('5' === _n0.a.a.valueOf()) {
					if (_n0.a.b === '') {
						var _n1 = _n0.a;
						return !signed;
					} else {
						var _n2 = _n0.a;
						return true;
					}
				} else {
					var _n3 = _n0.a;
					var _int = _n3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						elm$core$Char$toCode(_int));
				}
			}
		}));
var terezka$line_charts$Internal$Axis$Values$correctFloat = function (prec) {
	return A2(
		elm$core$Basics$composeR,
		myrho$elm_round$Round$round(prec),
		A2(
			elm$core$Basics$composeR,
			elm$core$String$toFloat,
			elm$core$Maybe$withDefault(0)));
};
var terezka$line_charts$Internal$Axis$Values$getMultiples = F3(
	function (magnitude, allowDecimals, hasTickAmount) {
		var defaults = hasTickAmount ? _List_fromArray(
			[1, 1.2, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10]) : _List_fromArray(
			[1, 2, 2.5, 5, 10]);
		return allowDecimals ? defaults : ((magnitude === 1) ? A2(
			elm$core$List$filter,
			function (n) {
				return _Utils_eq(
					elm$core$Basics$round(n),
					n);
			},
			defaults) : ((magnitude <= 0.1) ? _List_fromArray(
			[1 / magnitude]) : defaults));
	});
var terezka$line_charts$Internal$Axis$Values$getPrecision = function (number) {
	var _n0 = A2(
		elm$core$String$split,
		'e',
		elm$core$String$fromFloat(number));
	if ((_n0.b && _n0.b.b) && (!_n0.b.b.b)) {
		var before = _n0.a;
		var _n1 = _n0.b;
		var after = _n1.a;
		return elm$core$Basics$abs(
			A2(
				elm$core$Maybe$withDefault,
				0,
				elm$core$String$toInt(after)));
	} else {
		var _n2 = A2(
			elm$core$String$split,
			'.',
			elm$core$String$fromFloat(number));
		if ((_n2.b && _n2.b.b) && (!_n2.b.b.b)) {
			var before = _n2.a;
			var _n3 = _n2.b;
			var after = _n3.a;
			return elm$core$String$length(after);
		} else {
			return 0;
		}
	}
};
var elm$core$Basics$e = _Basics_e;
var elm$core$Basics$pow = _Basics_pow;
var terezka$line_charts$Internal$Utils$magnitude = function (num) {
	return A2(
		elm$core$Basics$pow,
		10,
		elm$core$Basics$floor(
			A2(elm$core$Basics$logBase, elm$core$Basics$e, num) / A2(elm$core$Basics$logBase, elm$core$Basics$e, 10)));
};
var terezka$line_charts$Internal$Axis$Values$getInterval = F3(
	function (intervalRaw, allowDecimals, hasTickAmount) {
		var magnitude = terezka$line_charts$Internal$Utils$magnitude(intervalRaw);
		var multiples = A3(terezka$line_charts$Internal$Axis$Values$getMultiples, magnitude, allowDecimals, hasTickAmount);
		var normalized = intervalRaw / magnitude;
		var findMultipleExact = function (multiples_) {
			findMultipleExact:
			while (true) {
				if (multiples_.b) {
					var m1 = multiples_.a;
					var rest = multiples_.b;
					if (_Utils_cmp(m1 * magnitude, intervalRaw) > -1) {
						return m1;
					} else {
						var $temp$multiples_ = rest;
						multiples_ = $temp$multiples_;
						continue findMultipleExact;
					}
				} else {
					return 1;
				}
			}
		};
		var findMultiple = function (multiples_) {
			findMultiple:
			while (true) {
				if (multiples_.b) {
					if (multiples_.b.b) {
						var m1 = multiples_.a;
						var _n2 = multiples_.b;
						var m2 = _n2.a;
						var rest = _n2.b;
						if (_Utils_cmp(normalized, (m1 + m2) / 2) < 1) {
							return m1;
						} else {
							var $temp$multiples_ = A2(elm$core$List$cons, m2, rest);
							multiples_ = $temp$multiples_;
							continue findMultiple;
						}
					} else {
						var m1 = multiples_.a;
						var rest = multiples_.b;
						if (_Utils_cmp(normalized, m1) < 1) {
							return m1;
						} else {
							var $temp$multiples_ = rest;
							multiples_ = $temp$multiples_;
							continue findMultiple;
						}
					}
				} else {
					return 1;
				}
			}
		};
		var multiple = hasTickAmount ? findMultipleExact(multiples) : findMultiple(multiples);
		var precision = terezka$line_charts$Internal$Axis$Values$getPrecision(magnitude) + terezka$line_charts$Internal$Axis$Values$getPrecision(multiple);
		return A2(terezka$line_charts$Internal$Axis$Values$correctFloat, precision, multiple * magnitude);
	});
var terezka$line_charts$Internal$Axis$Values$positions = F5(
	function (range, beginning, interval, m, acc) {
		positions:
		while (true) {
			var next = A2(
				terezka$line_charts$Internal$Axis$Values$correctFloat,
				terezka$line_charts$Internal$Axis$Values$getPrecision(interval),
				beginning + (m * interval));
			if (_Utils_cmp(next, range.max) > 0) {
				return acc;
			} else {
				var $temp$range = range,
					$temp$beginning = beginning,
					$temp$interval = interval,
					$temp$m = m + 1,
					$temp$acc = _Utils_ap(
					acc,
					_List_fromArray(
						[next]));
				range = $temp$range;
				beginning = $temp$beginning;
				interval = $temp$interval;
				m = $temp$m;
				acc = $temp$acc;
				continue positions;
			}
		}
	});
var terezka$line_charts$Internal$Axis$Values$values = F4(
	function (allowDecimals, exact, amountRough, range) {
		var intervalRough = (range.max - range.min) / amountRough;
		var interval = A3(terezka$line_charts$Internal$Axis$Values$getInterval, intervalRough, allowDecimals, exact);
		var intervalSafe = (!interval) ? 1 : interval;
		var beginning = A2(terezka$line_charts$Internal$Axis$Values$getBeginning, range.min, intervalSafe);
		var amountRoughSafe = (!amountRough) ? 1 : amountRough;
		return A5(terezka$line_charts$Internal$Axis$Values$positions, range, beginning, intervalSafe, 0, _List_Nil);
	});
var terezka$line_charts$Internal$Axis$Values$float = function (amount) {
	if (amount.$ === 'Exactly') {
		var amount_ = amount.a;
		return A3(terezka$line_charts$Internal$Axis$Values$values, true, true, amount_);
	} else {
		var amount_ = amount.a;
		return A3(terezka$line_charts$Internal$Axis$Values$values, true, false, amount_);
	}
};
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var terezka$line_charts$Internal$Coordinate$smallestRange = F2(
	function (data, range_) {
		return {
			max: A2(elm$core$Basics$min, data.max, range_.max),
			min: A2(elm$core$Basics$max, data.min, range_.min)
		};
	});
var terezka$line_charts$Internal$Axis$Ticks$floatCustom = F2(
	function (amount, tick) {
		return terezka$line_charts$Internal$Axis$Ticks$custom(
			F2(
				function (data, range) {
					return A2(
						elm$core$List$map,
						tick,
						A2(
							terezka$line_charts$Internal$Axis$Values$float,
							terezka$line_charts$Internal$Axis$Values$around(amount),
							A2(terezka$line_charts$Internal$Coordinate$smallestRange, data, range)));
				}));
	});
var terezka$line_charts$LineChart$Axis$Ticks$floatCustom = terezka$line_charts$Internal$Axis$Ticks$floatCustom;
var terezka$line_charts$Internal$Axis$Title$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Axis$Title$custom = F4(
	function (position, x, y, title) {
		return terezka$line_charts$Internal$Axis$Title$Config(
			{
				offset: _Utils_Tuple2(x, y),
				position: position,
				view: title
			});
	});
var terezka$line_charts$Internal$Axis$Title$atPosition = F3(
	function (position, x, y) {
		return A2(
			elm$core$Basics$composeL,
			A3(terezka$line_charts$Internal$Axis$Title$custom, position, x, y),
			terezka$line_charts$Internal$Svg$label('inherit'));
	});
var terezka$line_charts$Internal$Axis$Title$atAxisMax = function () {
	var position = F2(
		function (data, range) {
			return range.max;
		});
	return terezka$line_charts$Internal$Axis$Title$atPosition(position);
}();
var terezka$line_charts$Internal$Axis$Title$default = A2(terezka$line_charts$Internal$Axis$Title$atAxisMax, 0, 0);
var terezka$line_charts$LineChart$Axis$Title$default = terezka$line_charts$Internal$Axis$Title$default;
var author$project$Chart$xAxisConfig = F2(
	function (toFloat, toString) {
		return terezka$line_charts$LineChart$Axis$custom(
			{
				axisLine: terezka$line_charts$LineChart$Axis$Line$full(avh4$elm_color$Color$black),
				pixels: 700,
				range: A2(terezka$line_charts$LineChart$Axis$Range$padded, 20, 20),
				ticks: A2(
					terezka$line_charts$LineChart$Axis$Ticks$floatCustom,
					7,
					author$project$Chart$customTick(toString)),
				title: terezka$line_charts$LineChart$Axis$Title$default('Year'),
				variable: A2(
					elm$core$Basics$composeL,
					elm$core$Maybe$Just,
					function (point) {
						return toFloat(point.x);
					})
			});
	});
var elm$core$List$map3 = _List_map3;
var terezka$line_charts$Internal$Line$Series = function (a) {
	return {$: 'Series', a: a};
};
var terezka$line_charts$Internal$Line$SeriesConfig = F5(
	function (color, shape, dashing, label, data) {
		return {color: color, dashing: dashing, data: data, label: label, shape: shape};
	});
var terezka$line_charts$Internal$Line$line = F4(
	function (color_, shape_, label_, data_) {
		return terezka$line_charts$Internal$Line$Series(
			A5(terezka$line_charts$Internal$Line$SeriesConfig, color_, shape_, _List_Nil, label_, data_));
	});
var terezka$line_charts$LineChart$line = terezka$line_charts$Internal$Line$line;
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$svg$Svg$defs = elm$svg$Svg$trustedNode('defs');
var elm$svg$Svg$g = elm$svg$Svg$trustedNode('g');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var terezka$line_charts$Internal$Axis$variable = function (_n0) {
	var config = _n0.a;
	return config.variable;
};
var elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var elm$svg$Svg$Attributes$clipPath = _VirtualDom_attribute('clip-path');
var terezka$line_charts$Internal$Utils$toChartAreaId = function (id) {
	return 'chart__chart-area--' + id;
};
var terezka$line_charts$Internal$Svg$withinChartArea = function (_n0) {
	var id = _n0.id;
	return elm$svg$Svg$Attributes$clipPath(
		'url(#' + (terezka$line_charts$Internal$Utils$toChartAreaId(id) + ')'));
};
var terezka$line_charts$Internal$Axis$attributesLine = F2(
	function (system, _n0) {
		var events = _n0.events;
		var width = _n0.width;
		var color = _n0.color;
		return _Utils_ap(
			events,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$strokeWidth(
					elm$core$String$fromFloat(width)),
					elm$svg$Svg$Attributes$stroke(
					avh4$elm_color$Color$toCssString(color)),
					terezka$line_charts$Internal$Svg$withinChartArea(system)
				]));
	});
var terezka$line_charts$Internal$Path$Line = function (a) {
	return {$: 'Line', a: a};
};
var terezka$line_charts$Internal$Path$Move = function (a) {
	return {$: 'Move', a: a};
};
var elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var terezka$line_charts$Internal$Path$join = function (commands) {
	return A2(elm$core$String$join, ' ', commands);
};
var terezka$line_charts$Internal$Path$bool = function (bool_) {
	return bool_ ? '1' : '0';
};
var terezka$line_charts$Internal$Path$point = function (point_) {
	return elm$core$String$fromFloat(point_.x) + (' ' + elm$core$String$fromFloat(point_.y));
};
var terezka$line_charts$Internal$Path$points = function (points_) {
	return A2(
		elm$core$String$join,
		',',
		A2(elm$core$List$map, terezka$line_charts$Internal$Path$point, points_));
};
var terezka$line_charts$Internal$Path$toString = function (command) {
	switch (command.$) {
		case 'Close':
			return 'Z';
		case 'Move':
			var p = command.a;
			return 'M' + terezka$line_charts$Internal$Path$point(p);
		case 'Line':
			var p = command.a;
			return 'L' + terezka$line_charts$Internal$Path$point(p);
		case 'Horizontal':
			var x = command.a;
			return 'H' + elm$core$String$fromFloat(x);
		case 'Vertical':
			var y = command.a;
			return 'V' + elm$core$String$fromFloat(y);
		case 'CubicBeziers':
			var c1 = command.a;
			var c2 = command.b;
			var p = command.c;
			return 'C' + terezka$line_charts$Internal$Path$points(
				_List_fromArray(
					[c1, c2, p]));
		case 'CubicBeziersShort':
			var c1 = command.a;
			var p = command.b;
			return 'Q' + terezka$line_charts$Internal$Path$points(
				_List_fromArray(
					[c1, p]));
		case 'QuadraticBeziers':
			var c1 = command.a;
			var p = command.b;
			return 'Q' + terezka$line_charts$Internal$Path$points(
				_List_fromArray(
					[c1, p]));
		case 'QuadraticBeziersShort':
			var p = command.a;
			return 'T' + terezka$line_charts$Internal$Path$point(p);
		default:
			var rx = command.a;
			var ry = command.b;
			var xAxisRotation = command.c;
			var largeArcFlag = command.d;
			var sweepFlag = command.e;
			var p = command.f;
			return 'A' + terezka$line_charts$Internal$Path$join(
				_List_fromArray(
					[
						elm$core$String$fromFloat(rx),
						elm$core$String$fromFloat(ry),
						elm$core$String$fromInt(xAxisRotation),
						terezka$line_charts$Internal$Path$bool(largeArcFlag),
						terezka$line_charts$Internal$Path$bool(sweepFlag),
						terezka$line_charts$Internal$Path$point(p)
					]));
	}
};
var terezka$line_charts$Internal$Path$Arc = F6(
	function (a, b, c, d, e, f) {
		return {$: 'Arc', a: a, b: b, c: c, d: d, e: e, f: f};
	});
var terezka$line_charts$Internal$Path$Close = {$: 'Close'};
var terezka$line_charts$Internal$Path$CubicBeziers = F3(
	function (a, b, c) {
		return {$: 'CubicBeziers', a: a, b: b, c: c};
	});
var terezka$line_charts$Internal$Path$CubicBeziersShort = F2(
	function (a, b) {
		return {$: 'CubicBeziersShort', a: a, b: b};
	});
var terezka$line_charts$Internal$Path$Horizontal = function (a) {
	return {$: 'Horizontal', a: a};
};
var terezka$line_charts$Internal$Path$QuadraticBeziers = F2(
	function (a, b) {
		return {$: 'QuadraticBeziers', a: a, b: b};
	});
var terezka$line_charts$Internal$Path$QuadraticBeziersShort = function (a) {
	return {$: 'QuadraticBeziersShort', a: a};
};
var terezka$line_charts$Internal$Path$Vertical = function (a) {
	return {$: 'Vertical', a: a};
};
var terezka$line_charts$Internal$Coordinate$lengthX = function (system) {
	return A2(elm$core$Basics$max, 1, (system.frame.size.width - system.frame.margin.left) - system.frame.margin.right);
};
var terezka$line_charts$Internal$Coordinate$reachX = function (system) {
	var diff = system.x.max - system.x.min;
	return (diff > 0) ? diff : 1;
};
var terezka$line_charts$LineChart$Coordinate$scaleSvgX = F2(
	function (system, value) {
		return (value * terezka$line_charts$Internal$Coordinate$lengthX(system)) / terezka$line_charts$Internal$Coordinate$reachX(system);
	});
var terezka$line_charts$LineChart$Coordinate$toSvgX = F2(
	function (system, value) {
		return A2(terezka$line_charts$LineChart$Coordinate$scaleSvgX, system, value - system.x.min) + system.frame.margin.left;
	});
var terezka$line_charts$Internal$Coordinate$lengthY = function (system) {
	return A2(elm$core$Basics$max, 1, (system.frame.size.height - system.frame.margin.bottom) - system.frame.margin.top);
};
var terezka$line_charts$Internal$Coordinate$reachY = function (system) {
	var diff = system.y.max - system.y.min;
	return (diff > 0) ? diff : 1;
};
var terezka$line_charts$LineChart$Coordinate$scaleSvgY = F2(
	function (system, value) {
		return (value * terezka$line_charts$Internal$Coordinate$lengthY(system)) / terezka$line_charts$Internal$Coordinate$reachY(system);
	});
var terezka$line_charts$LineChart$Coordinate$toSvgY = F2(
	function (system, value) {
		return A2(terezka$line_charts$LineChart$Coordinate$scaleSvgY, system, system.y.max - value) + system.frame.margin.top;
	});
var terezka$line_charts$LineChart$Coordinate$toSvg = F2(
	function (system, point) {
		return {
			x: A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, point.x),
			y: A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, point.y)
		};
	});
var terezka$line_charts$Internal$Path$translate = F2(
	function (system, command) {
		switch (command.$) {
			case 'Move':
				var p = command.a;
				return terezka$line_charts$Internal$Path$Move(
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 'Line':
				var p = command.a;
				return terezka$line_charts$Internal$Path$Line(
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 'Horizontal':
				var x = command.a;
				return terezka$line_charts$Internal$Path$Horizontal(
					A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, x));
			case 'Vertical':
				var y = command.a;
				return terezka$line_charts$Internal$Path$Vertical(
					A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, y));
			case 'CubicBeziers':
				var c1 = command.a;
				var c2 = command.b;
				var p = command.c;
				return A3(
					terezka$line_charts$Internal$Path$CubicBeziers,
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, c1),
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, c2),
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 'CubicBeziersShort':
				var c1 = command.a;
				var p = command.b;
				return A2(
					terezka$line_charts$Internal$Path$CubicBeziersShort,
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, c1),
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 'QuadraticBeziers':
				var c1 = command.a;
				var p = command.b;
				return A2(
					terezka$line_charts$Internal$Path$QuadraticBeziers,
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, c1),
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 'QuadraticBeziersShort':
				var p = command.a;
				return terezka$line_charts$Internal$Path$QuadraticBeziersShort(
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			case 'Arc':
				var rx = command.a;
				var ry = command.b;
				var xAxisRotation = command.c;
				var largeArcFlag = command.d;
				var sweepFlag = command.e;
				var p = command.f;
				return A6(
					terezka$line_charts$Internal$Path$Arc,
					rx,
					ry,
					xAxisRotation,
					largeArcFlag,
					sweepFlag,
					A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, p));
			default:
				return terezka$line_charts$Internal$Path$Close;
		}
	});
var terezka$line_charts$Internal$Path$description = F2(
	function (system, commands) {
		return terezka$line_charts$Internal$Path$join(
			A2(
				elm$core$List$map,
				A2(
					elm$core$Basics$composeR,
					terezka$line_charts$Internal$Path$translate(system),
					terezka$line_charts$Internal$Path$toString),
				commands));
	});
var elm$svg$Svg$path = elm$svg$Svg$trustedNode('path');
var terezka$line_charts$Internal$Path$viewPath = function (attributes) {
	return A2(elm$svg$Svg$path, attributes, _List_Nil);
};
var terezka$line_charts$Internal$Path$view = F3(
	function (system, attributes, commands) {
		return terezka$line_charts$Internal$Path$viewPath(
			_Utils_ap(
				attributes,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$d(
						A2(terezka$line_charts$Internal$Path$description, system, commands))
					])));
	});
var terezka$line_charts$Internal$Utils$concat = F3(
	function (first, second, third) {
		return _Utils_ap(
			first,
			_Utils_ap(second, third));
	});
var avh4$elm_color$Color$scaleFrom255 = function (c) {
	return c / 255;
};
var avh4$elm_color$Color$rgb255 = F3(
	function (r, g, b) {
		return A4(
			avh4$elm_color$Color$RgbaSpace,
			avh4$elm_color$Color$scaleFrom255(r),
			avh4$elm_color$Color$scaleFrom255(g),
			avh4$elm_color$Color$scaleFrom255(b),
			1.0);
	});
var terezka$line_charts$LineChart$Colors$gray = A3(avh4$elm_color$Color$rgb255, 163, 163, 163);
var terezka$line_charts$Internal$Svg$horizontal = F5(
	function (system, userAttributes, y, x1, x2) {
		var attributes = A3(
			terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$stroke(
					avh4$elm_color$Color$toCssString(terezka$line_charts$LineChart$Colors$gray)),
					elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A3(
			terezka$line_charts$Internal$Path$view,
			system,
			attributes,
			_List_fromArray(
				[
					terezka$line_charts$Internal$Path$Move(
					{x: x1, y: y}),
					terezka$line_charts$Internal$Path$Line(
					{x: x1, y: y}),
					terezka$line_charts$Internal$Path$Line(
					{x: x2, y: y})
				]));
	});
var terezka$line_charts$Internal$Axis$viewHorizontalAxisLine = F3(
	function (system, axisPosition, config) {
		return A5(
			terezka$line_charts$Internal$Svg$horizontal,
			system,
			A2(terezka$line_charts$Internal$Axis$attributesLine, system, config),
			axisPosition,
			config.start,
			config.end);
	});
var terezka$line_charts$Internal$Axis$attributesTick = function (_n0) {
	var width = _n0.width;
	var color = _n0.color;
	return _List_fromArray(
		[
			elm$svg$Svg$Attributes$strokeWidth(
			elm$core$String$fromFloat(width)),
			elm$svg$Svg$Attributes$stroke(
			avh4$elm_color$Color$toCssString(color))
		]);
};
var terezka$line_charts$Internal$Axis$Tick$isPositive = function (direction) {
	if (direction.$ === 'Positive') {
		return true;
	} else {
		return false;
	}
};
var terezka$line_charts$Internal$Axis$lengthOfTick = function (_n0) {
	var length = _n0.length;
	var direction = _n0.direction;
	return terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? (-length) : length;
};
var terezka$line_charts$Internal$Svg$Middle = {$: 'Middle'};
var terezka$line_charts$Internal$Svg$anchorStyle = function (anchor) {
	var anchorString = function () {
		switch (anchor.$) {
			case 'Start':
				return 'start';
			case 'Middle':
				return 'middle';
			default:
				return 'end';
		}
	}();
	return elm$svg$Svg$Attributes$style('text-anchor: ' + (anchorString + ';'));
};
var terezka$line_charts$Internal$Svg$Transfrom = F2(
	function (a, b) {
		return {$: 'Transfrom', a: a, b: b};
	});
var terezka$line_charts$Internal$Svg$move = F3(
	function (system, x, y) {
		return A2(
			terezka$line_charts$Internal$Svg$Transfrom,
			A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, x),
			A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, y));
	});
var terezka$line_charts$Internal$Svg$offset = F2(
	function (x, y) {
		return A2(terezka$line_charts$Internal$Svg$Transfrom, x, y);
	});
var elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var terezka$line_charts$Internal$Svg$addPosition = F2(
	function (_n0, _n1) {
		var x = _n0.a;
		var y = _n0.b;
		var xf = _n1.a;
		var yf = _n1.b;
		return A2(terezka$line_charts$Internal$Svg$Transfrom, xf + x, yf + y);
	});
var terezka$line_charts$Internal$Svg$toPosition = A2(
	elm$core$List$foldr,
	terezka$line_charts$Internal$Svg$addPosition,
	A2(terezka$line_charts$Internal$Svg$Transfrom, 0, 0));
var terezka$line_charts$Internal$Svg$transform = function (translations) {
	var _n0 = terezka$line_charts$Internal$Svg$toPosition(translations);
	var x = _n0.a;
	var y = _n0.b;
	return elm$svg$Svg$Attributes$transform(
		'translate(' + (elm$core$String$fromFloat(x) + (', ' + (elm$core$String$fromFloat(y) + ')'))));
};
var terezka$line_charts$Internal$Axis$viewHorizontalLabel = F4(
	function (system, _n0, position, view) {
		var direction = _n0.direction;
		var length = _n0.length;
		var yOffset = terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? ((-5) - length) : (15 + length);
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3(terezka$line_charts$Internal$Svg$move, system, position.x, position.y),
							A2(terezka$line_charts$Internal$Svg$offset, 0, yOffset)
						])),
					terezka$line_charts$Internal$Svg$anchorStyle(terezka$line_charts$Internal$Svg$Middle)
				]),
			_List_fromArray(
				[view]));
	});
var elm$svg$Svg$line = elm$svg$Svg$trustedNode('line');
var elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var terezka$line_charts$Internal$Svg$xTick = F5(
	function (system, height, userAttributes, y, x) {
		var attributes = A3(
			terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$stroke(
					avh4$elm_color$Color$toCssString(terezka$line_charts$LineChart$Colors$gray))
				]),
			userAttributes,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$x1(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, x))),
					elm$svg$Svg$Attributes$x2(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, x))),
					elm$svg$Svg$Attributes$y1(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, y))),
					elm$svg$Svg$Attributes$y2(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, y) + height))
				]));
		return A2(elm$svg$Svg$line, attributes, _List_Nil);
	});
var terezka$line_charts$Internal$Utils$viewMaybe = F2(
	function (a, view) {
		return A2(
			elm$core$Maybe$withDefault,
			elm$svg$Svg$text(''),
			A2(elm$core$Maybe$map, view, a));
	});
var terezka$line_charts$Internal$Axis$viewHorizontalTick = F3(
	function (system, point, tick) {
		var x = point.x;
		var y = point.y;
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__tick')
				]),
			_List_fromArray(
				[
					A5(
					terezka$line_charts$Internal$Svg$xTick,
					system,
					terezka$line_charts$Internal$Axis$lengthOfTick(tick),
					terezka$line_charts$Internal$Axis$attributesTick(tick),
					y,
					x),
					A2(
					terezka$line_charts$Internal$Utils$viewMaybe,
					tick.label,
					A3(terezka$line_charts$Internal$Axis$viewHorizontalLabel, system, tick, point))
				]));
	});
var terezka$line_charts$Internal$Svg$Start = {$: 'Start'};
var terezka$line_charts$Internal$Axis$viewHorizontalTitle = F3(
	function (system, at, _n0) {
		var title = _n0.title;
		var position = at(
			A2(title.position, system.xData, system.x));
		var _n1 = title.offset;
		var xOffset = _n1.a;
		var yOffset = _n1.b;
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__title'),
					terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3(terezka$line_charts$Internal$Svg$move, system, position.x, position.y),
							A2(terezka$line_charts$Internal$Svg$offset, xOffset + 15, yOffset + 5)
						])),
					terezka$line_charts$Internal$Svg$anchorStyle(terezka$line_charts$Internal$Svg$Start)
				]),
			_List_fromArray(
				[title.view]));
	});
var terezka$line_charts$Internal$Axis$Intersection$getY = function (_n0) {
	var func = _n0.a;
	return A2(
		elm$core$Basics$composeL,
		function ($) {
			return $.y;
		},
		func);
};
var terezka$line_charts$Internal$Axis$Line$config = function (_n0) {
	var config_ = _n0.a;
	return config_;
};
var terezka$line_charts$Internal$Axis$Tick$properties = function (_n0) {
	var properties_ = _n0.a;
	return properties_;
};
var terezka$line_charts$Internal$Axis$Ticks$ticks = F3(
	function (dataRange, range, _n0) {
		var values = _n0.a;
		return A2(
			elm$core$List$map,
			terezka$line_charts$Internal$Axis$Tick$properties,
			A2(values, dataRange, range));
	});
var terezka$line_charts$Internal$Axis$Title$config = function (_n0) {
	var title = _n0.a;
	return title;
};
var terezka$line_charts$Internal$Axis$viewHorizontal = F3(
	function (system, intersection, _n0) {
		var config = _n0.a;
		var viewConfig = {
			intersection: A2(terezka$line_charts$Internal$Axis$Intersection$getY, intersection, system),
			line: A3(terezka$line_charts$Internal$Axis$Line$config, config.axisLine, system.xData, system.x),
			ticks: A3(terezka$line_charts$Internal$Axis$Ticks$ticks, system.xData, system.x, config.ticks),
			title: terezka$line_charts$Internal$Axis$Title$config(config.title)
		};
		var viewAxisLine = A2(terezka$line_charts$Internal$Axis$viewHorizontalAxisLine, system, viewConfig.intersection);
		var at = function (x) {
			return {x: x, y: viewConfig.intersection};
		};
		var viewTick = function (tick) {
			return A3(
				terezka$line_charts$Internal$Axis$viewHorizontalTick,
				system,
				at(tick.position),
				tick);
		};
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__axis--horizontal')
				]),
			_List_fromArray(
				[
					A3(terezka$line_charts$Internal$Axis$viewHorizontalTitle, system, at, viewConfig),
					viewAxisLine(viewConfig.line),
					A2(
					elm$svg$Svg$g,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$class('chart__ticks')
						]),
					A2(elm$core$List$map, viewTick, viewConfig.ticks))
				]));
	});
var terezka$line_charts$Internal$Svg$vertical = F5(
	function (system, userAttributes, x, y1, y2) {
		var attributes = A3(
			terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$stroke(
					avh4$elm_color$Color$toCssString(terezka$line_charts$LineChart$Colors$gray)),
					elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A3(
			terezka$line_charts$Internal$Path$view,
			system,
			attributes,
			_List_fromArray(
				[
					terezka$line_charts$Internal$Path$Move(
					{x: x, y: y1}),
					terezka$line_charts$Internal$Path$Line(
					{x: x, y: y1}),
					terezka$line_charts$Internal$Path$Line(
					{x: x, y: y2})
				]));
	});
var terezka$line_charts$Internal$Axis$viewVerticalAxisLine = F3(
	function (system, axisPosition, config) {
		return A5(
			terezka$line_charts$Internal$Svg$vertical,
			system,
			A2(terezka$line_charts$Internal$Axis$attributesLine, system, config),
			axisPosition,
			config.start,
			config.end);
	});
var terezka$line_charts$Internal$Svg$End = {$: 'End'};
var terezka$line_charts$Internal$Axis$viewVerticalLabel = F4(
	function (system, _n0, position, view) {
		var direction = _n0.direction;
		var length = _n0.length;
		var xOffset = terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? (5 + length) : ((-5) - length);
		var anchor = terezka$line_charts$Internal$Axis$Tick$isPositive(direction) ? terezka$line_charts$Internal$Svg$Start : terezka$line_charts$Internal$Svg$End;
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3(terezka$line_charts$Internal$Svg$move, system, position.x, position.y),
							A2(terezka$line_charts$Internal$Svg$offset, xOffset, 5)
						])),
					terezka$line_charts$Internal$Svg$anchorStyle(anchor)
				]),
			_List_fromArray(
				[view]));
	});
var terezka$line_charts$Internal$Svg$yTick = F5(
	function (system, width, userAttributes, x, y) {
		var attributes = A3(
			terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__tick'),
					elm$svg$Svg$Attributes$stroke(
					avh4$elm_color$Color$toCssString(terezka$line_charts$LineChart$Colors$gray))
				]),
			userAttributes,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$x1(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, x))),
					elm$svg$Svg$Attributes$x2(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, x) - width)),
					elm$svg$Svg$Attributes$y1(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, y))),
					elm$svg$Svg$Attributes$y2(
					elm$core$String$fromFloat(
						A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, y)))
				]));
		return A2(elm$svg$Svg$line, attributes, _List_Nil);
	});
var terezka$line_charts$Internal$Axis$viewVerticalTick = F3(
	function (system, point, tick) {
		var x = point.x;
		var y = point.y;
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__tick')
				]),
			_List_fromArray(
				[
					A5(
					terezka$line_charts$Internal$Svg$yTick,
					system,
					terezka$line_charts$Internal$Axis$lengthOfTick(tick),
					terezka$line_charts$Internal$Axis$attributesTick(tick),
					x,
					y),
					A2(
					terezka$line_charts$Internal$Utils$viewMaybe,
					tick.label,
					A3(terezka$line_charts$Internal$Axis$viewVerticalLabel, system, tick, point))
				]));
	});
var terezka$line_charts$Internal$Axis$viewVerticalTitle = F3(
	function (system, at, _n0) {
		var title = _n0.title;
		var position = at(
			A2(title.position, system.yData, system.y));
		var _n1 = title.offset;
		var xOffset = _n1.a;
		var yOffset = _n1.b;
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__title'),
					terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3(terezka$line_charts$Internal$Svg$move, system, position.x, position.y),
							A2(terezka$line_charts$Internal$Svg$offset, xOffset + 2, yOffset - 10)
						])),
					terezka$line_charts$Internal$Svg$anchorStyle(terezka$line_charts$Internal$Svg$End)
				]),
			_List_fromArray(
				[title.view]));
	});
var terezka$line_charts$Internal$Axis$Intersection$getX = function (_n0) {
	var func = _n0.a;
	return A2(
		elm$core$Basics$composeL,
		function ($) {
			return $.x;
		},
		func);
};
var terezka$line_charts$Internal$Axis$viewVertical = F3(
	function (system, intersection, _n0) {
		var config = _n0.a;
		var viewConfig = {
			intersection: A2(terezka$line_charts$Internal$Axis$Intersection$getX, intersection, system),
			line: A3(terezka$line_charts$Internal$Axis$Line$config, config.axisLine, system.yData, system.y),
			ticks: A3(terezka$line_charts$Internal$Axis$Ticks$ticks, system.yData, system.y, config.ticks),
			title: terezka$line_charts$Internal$Axis$Title$config(config.title)
		};
		var viewAxisLine = A2(terezka$line_charts$Internal$Axis$viewVerticalAxisLine, system, viewConfig.intersection);
		var at = function (y) {
			return {x: viewConfig.intersection, y: y};
		};
		var viewTick = function (tick) {
			return A3(
				terezka$line_charts$Internal$Axis$viewVerticalTick,
				system,
				at(tick.position),
				tick);
		};
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__axis--vertical')
				]),
			_List_fromArray(
				[
					A3(terezka$line_charts$Internal$Axis$viewVerticalTitle, system, at, viewConfig),
					viewAxisLine(viewConfig.line),
					A2(
					elm$svg$Svg$g,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$class('chart__ticks')
						]),
					A2(elm$core$List$map, viewTick, viewConfig.ticks))
				]));
	});
var terezka$line_charts$Internal$Container$properties = F2(
	function (f, _n0) {
		var properties_ = _n0.a;
		return f(properties_);
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var terezka$line_charts$Internal$Events$toContainerAttributes = F3(
	function (data, system, _n0) {
		var events = _n0.a;
		var order = function (_n1) {
			var outside = _n1.a;
			var event = _n1.b;
			return outside ? elm$core$Maybe$Just(
				A2(event, data, system)) : elm$core$Maybe$Nothing;
		};
		return A2(elm$core$List$filterMap, order, events);
	});
var terezka$line_charts$Internal$Axis$ticks = function (_n0) {
	var config = _n0.a;
	return config.ticks;
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$svg$Svg$circle = elm$svg$Svg$trustedNode('circle');
var elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var terezka$line_charts$Internal$Svg$gridDot = F3(
	function (radius, color, point) {
		return A2(
			elm$svg$Svg$circle,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$cx(
					elm$core$String$fromFloat(point.x)),
					elm$svg$Svg$Attributes$cy(
					elm$core$String$fromFloat(point.y)),
					elm$svg$Svg$Attributes$r(
					elm$core$String$fromFloat(radius)),
					elm$svg$Svg$Attributes$fill(
					avh4$elm_color$Color$toCssString(color))
				]),
			_List_Nil);
	});
var terezka$line_charts$LineChart$Coordinate$Point = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var terezka$line_charts$Internal$Grid$viewDots = F5(
	function (system, verticals, horizontals, radius, color) {
		var dot = F2(
			function (x, y) {
				return A2(
					terezka$line_charts$LineChart$Coordinate$toSvg,
					system,
					A2(terezka$line_charts$LineChart$Coordinate$Point, x, y));
			});
		var dots_ = function (g) {
			return A2(
				elm$core$List$map,
				dot(g),
				horizontals);
		};
		var alldots = A2(elm$core$List$concatMap, dots_, verticals);
		return A2(
			elm$core$List$map,
			A2(terezka$line_charts$Internal$Svg$gridDot, radius, color),
			alldots);
	});
var terezka$line_charts$Internal$Svg$horizontalGrid = F3(
	function (system, userAttributes, y) {
		var attributes = A3(
			terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$stroke(
					avh4$elm_color$Color$toCssString(terezka$line_charts$LineChart$Colors$gray)),
					elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A5(terezka$line_charts$Internal$Svg$horizontal, system, attributes, y, system.x.min, system.x.max);
	});
var terezka$line_charts$Internal$Svg$verticalGrid = F3(
	function (system, userAttributes, x) {
		var attributes = A3(
			terezka$line_charts$Internal$Utils$concat,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$stroke(
					avh4$elm_color$Color$toCssString(terezka$line_charts$LineChart$Colors$gray)),
					elm$svg$Svg$Attributes$style('pointer-events: none;')
				]),
			userAttributes,
			_List_Nil);
		return A5(terezka$line_charts$Internal$Svg$vertical, system, attributes, x, system.y.min, system.y.max);
	});
var terezka$line_charts$Internal$Grid$viewLines = F5(
	function (system, verticals, horizontals, width, color) {
		var attributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$strokeWidth(
				elm$core$String$fromFloat(width)),
				elm$svg$Svg$Attributes$stroke(
				avh4$elm_color$Color$toCssString(color))
			]);
		return _Utils_ap(
			A2(
				elm$core$List$map,
				A2(terezka$line_charts$Internal$Svg$horizontalGrid, system, attributes),
				horizontals),
			A2(
				elm$core$List$map,
				A2(terezka$line_charts$Internal$Svg$verticalGrid, system, attributes),
				verticals));
	});
var terezka$line_charts$Internal$Grid$view = F4(
	function (system, xAxis, yAxis, grid) {
		var hasGrid = function (tick) {
			return tick.grid ? elm$core$Maybe$Just(tick.position) : elm$core$Maybe$Nothing;
		};
		var horizontals = A2(
			elm$core$List$filterMap,
			hasGrid,
			A3(
				terezka$line_charts$Internal$Axis$Ticks$ticks,
				system.yData,
				system.y,
				terezka$line_charts$Internal$Axis$ticks(yAxis)));
		var verticals = A2(
			elm$core$List$filterMap,
			hasGrid,
			A3(
				terezka$line_charts$Internal$Axis$Ticks$ticks,
				system.xData,
				system.x,
				terezka$line_charts$Internal$Axis$ticks(xAxis)));
		if (grid.$ === 'Dots') {
			var radius = grid.a;
			var color = grid.b;
			return A5(terezka$line_charts$Internal$Grid$viewDots, system, verticals, horizontals, radius, color);
		} else {
			var width = grid.a;
			var color = grid.b;
			return A5(terezka$line_charts$Internal$Grid$viewLines, system, verticals, horizontals, width, color);
		}
	});
var terezka$line_charts$Internal$Junk$addBelow = F2(
	function (below, layers) {
		return _Utils_update(
			layers,
			{
				below: _Utils_ap(below, layers.below)
			});
	});
var terezka$line_charts$Internal$Junk$getLayers = F5(
	function (series, toX, toY, system, _n0) {
		var toLayers = _n0.a;
		return A4(toLayers, series, toX, toY, system);
	});
var terezka$line_charts$Internal$Line$label = function (_n0) {
	var config = _n0.a;
	return config.label;
};
var terezka$line_charts$Internal$Legends$viewFree = F5(
	function (system, placement, viewLabel, line, data) {
		var _n0 = function () {
			if (placement.$ === 'Beginning') {
				return _Utils_Tuple3(data, terezka$line_charts$Internal$Svg$End, -10);
			} else {
				return _Utils_Tuple3(
					elm$core$List$reverse(data),
					terezka$line_charts$Internal$Svg$Start,
					10);
			}
		}();
		var orderedPoints = _n0.a;
		var anchor = _n0.b;
		var xOffset = _n0.c;
		var transform = function (_n3) {
			var x = _n3.x;
			var y = _n3.y;
			return terezka$line_charts$Internal$Svg$transform(
				_List_fromArray(
					[
						A3(terezka$line_charts$Internal$Svg$move, system, x, y),
						A2(terezka$line_charts$Internal$Svg$offset, xOffset, 3)
					]));
		};
		var viewLegend = function (_n2) {
			var point = _n2.point;
			return A2(
				elm$svg$Svg$g,
				_List_fromArray(
					[
						transform(point),
						terezka$line_charts$Internal$Svg$anchorStyle(anchor)
					]),
				_List_fromArray(
					[
						viewLabel(
						terezka$line_charts$Internal$Line$label(line))
					]));
		};
		return A2(
			terezka$line_charts$Internal$Utils$viewMaybe,
			elm$core$List$head(orderedPoints),
			viewLegend);
	});
var terezka$line_charts$Internal$Legends$viewFrees = F3(
	function (_n0, placement, view_) {
		var system = _n0.system;
		var lines = _n0.lines;
		var data = _n0.data;
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__legends')
				]),
			A3(
				elm$core$List$map2,
				A3(terezka$line_charts$Internal$Legends$viewFree, system, placement, view_),
				lines,
				data));
	});
var terezka$line_charts$Internal$Data$Point = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var elm$core$Basics$pi = _Basics_pi;
var elm$core$Basics$sqrt = _Basics_sqrt;
var elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var terezka$line_charts$Internal$Dots$varietyAttributes = F2(
	function (color, variety) {
		switch (variety.$) {
			case 'Empty':
				var width = variety.a;
				return _List_fromArray(
					[
						elm$svg$Svg$Attributes$stroke(
						avh4$elm_color$Color$toCssString(color)),
						elm$svg$Svg$Attributes$strokeWidth(
						elm$core$String$fromInt(width)),
						elm$svg$Svg$Attributes$fill('white')
					]);
			case 'Aura':
				var width = variety.a;
				var opacity = variety.b;
				return _List_fromArray(
					[
						elm$svg$Svg$Attributes$stroke(
						avh4$elm_color$Color$toCssString(color)),
						elm$svg$Svg$Attributes$strokeWidth(
						elm$core$String$fromInt(width)),
						elm$svg$Svg$Attributes$strokeOpacity(
						elm$core$String$fromFloat(opacity)),
						elm$svg$Svg$Attributes$fill(
						avh4$elm_color$Color$toCssString(color))
					]);
			case 'Disconnected':
				var width = variety.a;
				return _List_fromArray(
					[
						elm$svg$Svg$Attributes$stroke('white'),
						elm$svg$Svg$Attributes$strokeWidth(
						elm$core$String$fromInt(width)),
						elm$svg$Svg$Attributes$fill(
						avh4$elm_color$Color$toCssString(color))
					]);
			default:
				return _List_fromArray(
					[
						elm$svg$Svg$Attributes$fill(
						avh4$elm_color$Color$toCssString(color))
					]);
		}
	});
var terezka$line_charts$Internal$Dots$viewCircle = F5(
	function (events, variety, color, area, point) {
		var radius = elm$core$Basics$sqrt(area / elm$core$Basics$pi);
		var attributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$cx(
				elm$core$String$fromFloat(point.x)),
				elm$svg$Svg$Attributes$cy(
				elm$core$String$fromFloat(point.y)),
				elm$svg$Svg$Attributes$r(
				elm$core$String$fromFloat(radius))
			]);
		return A2(
			elm$svg$Svg$circle,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2(terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var terezka$line_charts$Internal$Dots$pathPlus = F2(
	function (area, point) {
		var side = elm$core$Basics$sqrt(area / 5);
		var r6 = side / 2;
		var r3 = side;
		var commands = _List_fromArray(
			[
				'M' + (elm$core$String$fromFloat(point.x - r6) + (' ' + elm$core$String$fromFloat((point.y - r3) - r6))),
				'v' + elm$core$String$fromFloat(r3),
				'h' + elm$core$String$fromFloat(-r3),
				'v' + elm$core$String$fromFloat(r3),
				'h' + elm$core$String$fromFloat(r3),
				'v' + elm$core$String$fromFloat(r3),
				'h' + elm$core$String$fromFloat(r3),
				'v' + elm$core$String$fromFloat(-r3),
				'h' + elm$core$String$fromFloat(r3),
				'v' + elm$core$String$fromFloat(-r3),
				'h' + elm$core$String$fromFloat(-r3),
				'v' + elm$core$String$fromFloat(-r3),
				'h' + elm$core$String$fromFloat(-r3),
				'v' + elm$core$String$fromFloat(r3)
			]);
		return A2(elm$core$String$join, ' ', commands);
	});
var terezka$line_charts$Internal$Dots$viewCross = F5(
	function (events, variety, color, area, point) {
		var rotation = 'rotate(45 ' + (elm$core$String$fromFloat(point.x) + (' ' + (elm$core$String$fromFloat(point.y) + ')')));
		var attributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$d(
				A2(terezka$line_charts$Internal$Dots$pathPlus, area, point)),
				elm$svg$Svg$Attributes$transform(rotation)
			]);
		return A2(
			elm$svg$Svg$path,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2(terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var elm$svg$Svg$rect = elm$svg$Svg$trustedNode('rect');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var terezka$line_charts$Internal$Dots$viewDiamond = F5(
	function (events, variety, color, area, point) {
		var side = elm$core$Basics$sqrt(area);
		var rotation = 'rotate(45 ' + (elm$core$String$fromFloat(point.x) + (' ' + (elm$core$String$fromFloat(point.y) + ')')));
		var attributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$x(
				elm$core$String$fromFloat(point.x - (side / 2))),
				elm$svg$Svg$Attributes$y(
				elm$core$String$fromFloat(point.y - (side / 2))),
				elm$svg$Svg$Attributes$width(
				elm$core$String$fromFloat(side)),
				elm$svg$Svg$Attributes$height(
				elm$core$String$fromFloat(side)),
				elm$svg$Svg$Attributes$transform(rotation)
			]);
		return A2(
			elm$svg$Svg$rect,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2(terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var terezka$line_charts$Internal$Dots$viewPlus = F5(
	function (events, variety, color, area, point) {
		var attributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$d(
				A2(terezka$line_charts$Internal$Dots$pathPlus, area, point))
			]);
		return A2(
			elm$svg$Svg$path,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2(terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var terezka$line_charts$Internal$Dots$viewSquare = F5(
	function (events, variety, color, area, point) {
		var side = elm$core$Basics$sqrt(area);
		var attributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$x(
				elm$core$String$fromFloat(point.x - (side / 2))),
				elm$svg$Svg$Attributes$y(
				elm$core$String$fromFloat(point.y - (side / 2))),
				elm$svg$Svg$Attributes$width(
				elm$core$String$fromFloat(side)),
				elm$svg$Svg$Attributes$height(
				elm$core$String$fromFloat(side))
			]);
		return A2(
			elm$svg$Svg$rect,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2(terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * elm$core$Basics$pi) / 180;
};
var elm$core$Basics$tan = _Basics_tan;
var terezka$line_charts$Internal$Dots$pathTriangle = F2(
	function (area, point) {
		var side = elm$core$Basics$sqrt(
			(area * 4) / elm$core$Basics$sqrt(3));
		var height = (elm$core$Basics$sqrt(3) * side) / 2;
		var fromMiddle = height - ((elm$core$Basics$tan(
			elm$core$Basics$degrees(30)) * side) / 2);
		var commands = _List_fromArray(
			[
				'M' + (elm$core$String$fromFloat(point.x) + (' ' + elm$core$String$fromFloat(point.y - fromMiddle))),
				'l' + (elm$core$String$fromFloat((-side) / 2) + (' ' + elm$core$String$fromFloat(height))),
				'h' + elm$core$String$fromFloat(side),
				'z'
			]);
		return A2(elm$core$String$join, ' ', commands);
	});
var terezka$line_charts$Internal$Dots$viewTriangle = F5(
	function (events, variety, color, area, point) {
		var attributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$d(
				A2(terezka$line_charts$Internal$Dots$pathTriangle, area, point))
			]);
		return A2(
			elm$svg$Svg$path,
			_Utils_ap(
				events,
				_Utils_ap(
					attributes,
					A2(terezka$line_charts$Internal$Dots$varietyAttributes, color, variety))),
			_List_Nil);
	});
var terezka$line_charts$Internal$Dots$viewShape = F5(
	function (system, _n0, shape, color, point) {
		var radius = _n0.radius;
		var variety = _n0.variety;
		var view_ = function () {
			switch (shape.$) {
				case 'Circle':
					return terezka$line_charts$Internal$Dots$viewCircle;
				case 'Triangle':
					return terezka$line_charts$Internal$Dots$viewTriangle;
				case 'Square':
					return terezka$line_charts$Internal$Dots$viewSquare;
				case 'Diamond':
					return terezka$line_charts$Internal$Dots$viewDiamond;
				case 'Cross':
					return terezka$line_charts$Internal$Dots$viewCross;
				case 'Plus':
					return terezka$line_charts$Internal$Dots$viewPlus;
				default:
					return F5(
						function (_n2, _n3, _n4, _n5, _n6) {
							return elm$svg$Svg$text('');
						});
			}
		}();
		var size = (2 * elm$core$Basics$pi) * radius;
		var pointSvg = A2(terezka$line_charts$LineChart$Coordinate$toSvg, system, point);
		return A5(view_, _List_Nil, variety, color, size, pointSvg);
	});
var terezka$line_charts$Internal$Dots$viewSample = F5(
	function (_n0, shape, color, system, data) {
		var config = _n0.a;
		var _n1 = config.legend(
			A2(
				elm$core$List$map,
				function ($) {
					return $.user;
				},
				data));
		var style_ = _n1.a;
		return A4(terezka$line_charts$Internal$Dots$viewShape, system, style_, shape, color);
	});
var terezka$line_charts$Internal$Line$color = F3(
	function (_n0, _n1, data_) {
		var config = _n0.a;
		var line_ = _n1.a;
		var _n2 = config(
			A2(
				elm$core$List$map,
				function ($) {
					return $.user;
				},
				data_));
		var style_ = _n2.a;
		return style_.color(line_.color);
	});
var terezka$line_charts$Internal$Line$shape = function (_n0) {
	var config = _n0.a;
	return config.shape;
};
var elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var terezka$line_charts$Internal$Area$hasArea = function (config) {
	switch (config.$) {
		case 'None':
			return false;
		case 'Normal':
			return true;
		case 'Stacked':
			return true;
		default:
			return true;
	}
};
var terezka$line_charts$Internal$Area$opacity = function (config) {
	switch (config.$) {
		case 'None':
			return 0;
		case 'Normal':
			var opacity_ = config.a;
			return opacity_;
		case 'Stacked':
			var opacity_ = config.a;
			return opacity_;
		default:
			var opacity_ = config.a;
			return opacity_;
	}
};
var terezka$line_charts$Internal$Line$toAreaAttributes = F3(
	function (_n0, _n1, area) {
		var serie = _n0.a;
		var style_ = _n1.a;
		return _List_fromArray(
			[
				elm$svg$Svg$Attributes$class('chart__interpolation__area__fragment'),
				elm$svg$Svg$Attributes$fill(
				avh4$elm_color$Color$toCssString(
					style_.color(serie.color)))
			]);
	});
var elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var terezka$line_charts$Internal$Line$toSeriesAttributes = F2(
	function (_n0, _n1) {
		var serie = _n0.a;
		var style_ = _n1.a;
		return _List_fromArray(
			[
				elm$svg$Svg$Attributes$style('pointer-events: none;'),
				elm$svg$Svg$Attributes$class('chart__interpolation__line__fragment'),
				elm$svg$Svg$Attributes$stroke(
				avh4$elm_color$Color$toCssString(
					style_.color(serie.color))),
				elm$svg$Svg$Attributes$strokeWidth(
				elm$core$String$fromFloat(style_.width)),
				elm$svg$Svg$Attributes$strokeDasharray(
				A2(
					elm$core$String$join,
					' ',
					A2(elm$core$List$map, elm$core$String$fromFloat, serie.dashing))),
				elm$svg$Svg$Attributes$fill('transparent')
			]);
	});
var terezka$line_charts$Internal$Utils$viewIf = F2(
	function (condition, view) {
		return condition ? view(_Utils_Tuple0) : elm$svg$Svg$text('');
	});
var terezka$line_charts$Internal$Line$viewSample = F5(
	function (_n0, line_, area, data_, sampleWidth) {
		var look = _n0.a;
		var style_ = look(
			A2(
				elm$core$List$map,
				function ($) {
					return $.user;
				},
				data_));
		var sizeAttributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$x1('0'),
				elm$svg$Svg$Attributes$y1('0'),
				elm$svg$Svg$Attributes$x2(
				elm$core$String$fromFloat(sampleWidth)),
				elm$svg$Svg$Attributes$y2('0')
			]);
		var rectangleAttributes = _List_fromArray(
			[
				elm$svg$Svg$Attributes$x('0'),
				elm$svg$Svg$Attributes$y('0'),
				elm$svg$Svg$Attributes$height('9'),
				elm$svg$Svg$Attributes$width(
				elm$core$String$fromFloat(sampleWidth))
			]);
		var lineAttributes = A2(terezka$line_charts$Internal$Line$toSeriesAttributes, line_, style_);
		var areaAttributes = A2(
			elm$core$List$cons,
			elm$svg$Svg$Attributes$fillOpacity(
				elm$core$String$fromFloat(
					terezka$line_charts$Internal$Area$opacity(area))),
			A3(terezka$line_charts$Internal$Line$toAreaAttributes, line_, style_, area));
		var viewRectangle = function (_n1) {
			return A2(
				elm$svg$Svg$rect,
				_Utils_ap(areaAttributes, rectangleAttributes),
				_List_Nil);
		};
		return A2(
			elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$line,
					_Utils_ap(lineAttributes, sizeAttributes),
					_List_Nil),
					A2(
					terezka$line_charts$Internal$Utils$viewIf,
					terezka$line_charts$Internal$Area$hasArea(area),
					viewRectangle)
				]));
	});
var terezka$line_charts$LineChart$Coordinate$scaleDataX = F2(
	function (system, value) {
		return (value * terezka$line_charts$Internal$Coordinate$reachX(system)) / terezka$line_charts$Internal$Coordinate$lengthX(system);
	});
var terezka$line_charts$LineChart$Coordinate$toDataX = F2(
	function (system, value) {
		return system.x.min + A2(terezka$line_charts$LineChart$Coordinate$scaleDataX, system, value - system.frame.margin.left);
	});
var terezka$line_charts$LineChart$Coordinate$scaleDataY = F2(
	function (system, value) {
		return (value * terezka$line_charts$Internal$Coordinate$reachY(system)) / terezka$line_charts$Internal$Coordinate$lengthY(system);
	});
var terezka$line_charts$LineChart$Coordinate$toDataY = F2(
	function (system, value) {
		return system.y.max - A2(terezka$line_charts$LineChart$Coordinate$scaleDataY, system, value - system.frame.margin.top);
	});
var terezka$line_charts$LineChart$Coordinate$toData = F2(
	function (system, point) {
		return {
			x: A2(terezka$line_charts$LineChart$Coordinate$toDataX, system, point.x),
			y: A2(terezka$line_charts$LineChart$Coordinate$toDataY, system, point.y)
		};
	});
var terezka$line_charts$Internal$Legends$viewSample = F4(
	function (_n0, sampleWidth, line, data) {
		var system = _n0.system;
		var lineConfig = _n0.lineConfig;
		var dotsConfig = _n0.dotsConfig;
		var area = _n0.area;
		var shape = terezka$line_charts$Internal$Line$shape(line);
		var dotPosition = A2(
			terezka$line_charts$LineChart$Coordinate$toData,
			system,
			A2(terezka$line_charts$Internal$Data$Point, sampleWidth / 2, 0));
		var color = A3(terezka$line_charts$Internal$Line$color, lineConfig, line, data);
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__sample')
				]),
			_List_fromArray(
				[
					A5(terezka$line_charts$Internal$Line$viewSample, lineConfig, line, area, data, sampleWidth),
					A6(terezka$line_charts$Internal$Dots$viewSample, dotsConfig, shape, color, system, data, dotPosition)
				]));
	});
var terezka$line_charts$Internal$Legends$viewGrouped = F3(
	function (_arguments, sampleWidth, container) {
		var toLegend = F2(
			function (line, data) {
				return {
					label: terezka$line_charts$Internal$Line$label(line),
					sample: A4(terezka$line_charts$Internal$Legends$viewSample, _arguments, sampleWidth, line, data)
				};
			});
		var legends = A3(elm$core$List$map2, toLegend, _arguments.lines, _arguments.data);
		return A2(container, _arguments.system, legends);
	});
var terezka$line_charts$Internal$Legends$view = function (_arguments) {
	var _n0 = _arguments.legends;
	switch (_n0.$) {
		case 'Free':
			var placement = _n0.a;
			var view_ = _n0.b;
			return A3(terezka$line_charts$Internal$Legends$viewFrees, _arguments, placement, view_);
		case 'Grouped':
			var sampleWidth = _n0.a;
			var container = _n0.b;
			return A3(
				terezka$line_charts$Internal$Legends$viewGrouped,
				_arguments,
				sampleWidth,
				container(_arguments));
		default:
			return elm$svg$Svg$text('');
	}
};
var terezka$line_charts$Internal$Line$data = function (_n0) {
	var config = _n0.a;
	return config.data;
};
var terezka$line_charts$Internal$Area$opacityContainer = function (config) {
	switch (config.$) {
		case 'None':
			return 1;
		case 'Normal':
			var opacity_ = config.a;
			return 1;
		case 'Stacked':
			var opacity_ = config.a;
			return opacity_;
		default:
			var opacity_ = config.a;
			return opacity_;
	}
};
var terezka$line_charts$Internal$Line$viewNormal = function (_n0) {
	var areas = _n0.a;
	var lines = _n0.b;
	var dots = _n0.c;
	var view_ = F3(
		function (area_, line_, dots_) {
			return A2(
				elm$svg$Svg$g,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$class('chart__line')
					]),
				_List_fromArray(
					[area_, line_, dots_]));
		});
	return A4(elm$core$List$map3, view_, areas, lines, dots);
};
var elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var terezka$line_charts$Internal$Data$isWithinRange = F2(
	function (system, point) {
		return _Utils_eq(
			A3(elm$core$Basics$clamp, system.x.min, system.x.max, point.x),
			point.x) && _Utils_eq(
			A3(elm$core$Basics$clamp, system.y.min, system.y.max, point.y),
			point.y);
	});
var elm$core$Tuple$mapSecond = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var terezka$line_charts$Internal$Interpolation$linear = elm$core$List$map(
	elm$core$List$map(terezka$line_charts$Internal$Path$Line));
var terezka$line_charts$Internal$Interpolation$First = {$: 'First'};
var terezka$line_charts$Internal$Interpolation$Previous = function (a) {
	return {$: 'Previous', a: a};
};
var terezka$line_charts$Internal$Interpolation$monotoneCurve = F4(
	function (point0, point1, tangent0, tangent1) {
		var dx = (point1.x - point0.x) / 3;
		return A3(
			terezka$line_charts$Internal$Path$CubicBeziers,
			{x: point0.x + dx, y: point0.y + (dx * tangent0)},
			{x: point1.x - dx, y: point1.y - (dx * tangent1)},
			point1);
	});
var terezka$line_charts$Internal$Interpolation$slope2 = F3(
	function (point0, point1, t) {
		var h = point1.x - point0.x;
		return h ? ((((3 * (point1.y - point0.y)) / h) - t) / 2) : t;
	});
var terezka$line_charts$Internal$Interpolation$sign = function (x) {
	return (x < 0) ? (-1) : 1;
};
var terezka$line_charts$Internal$Interpolation$toH = F2(
	function (h0, h1) {
		return (!h0) ? ((h1 < 0) ? (0 * (-1)) : h1) : h0;
	});
var terezka$line_charts$Internal$Interpolation$slope3 = F3(
	function (point0, point1, point2) {
		var h1 = point2.x - point1.x;
		var h0 = point1.x - point0.x;
		var s0h = A2(terezka$line_charts$Internal$Interpolation$toH, h0, h1);
		var s0 = (point1.y - point0.y) / s0h;
		var s1h = A2(terezka$line_charts$Internal$Interpolation$toH, h1, h0);
		var s1 = (point2.y - point1.y) / s1h;
		var p = ((s0 * h1) + (s1 * h0)) / (h0 + h1);
		var slope = (terezka$line_charts$Internal$Interpolation$sign(s0) + terezka$line_charts$Internal$Interpolation$sign(s1)) * A2(
			elm$core$Basics$min,
			A2(
				elm$core$Basics$min,
				elm$core$Basics$abs(s0),
				elm$core$Basics$abs(s1)),
			0.5 * elm$core$Basics$abs(p));
		return elm$core$Basics$isNaN(slope) ? 0 : slope;
	});
var terezka$line_charts$Internal$Interpolation$monotonePart = F2(
	function (points, _n0) {
		var tangent = _n0.a;
		var commands = _n0.b;
		var _n1 = _Utils_Tuple2(tangent, points);
		_n1$4:
		while (true) {
			if (_n1.a.$ === 'First') {
				if (_n1.b.b && _n1.b.b.b) {
					if (_n1.b.b.b.b) {
						var _n2 = _n1.a;
						var _n3 = _n1.b;
						var p0 = _n3.a;
						var _n4 = _n3.b;
						var p1 = _n4.a;
						var _n5 = _n4.b;
						var p2 = _n5.a;
						var rest = _n5.b;
						var t1 = A3(terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p2);
						var t0 = A3(terezka$line_charts$Internal$Interpolation$slope2, p0, p1, t1);
						return A2(
							terezka$line_charts$Internal$Interpolation$monotonePart,
							A2(
								elm$core$List$cons,
								p1,
								A2(elm$core$List$cons, p2, rest)),
							_Utils_Tuple2(
								terezka$line_charts$Internal$Interpolation$Previous(t1),
								_Utils_ap(
									commands,
									_List_fromArray(
										[
											A4(terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1)
										]))));
					} else {
						var _n9 = _n1.a;
						var _n10 = _n1.b;
						var p0 = _n10.a;
						var _n11 = _n10.b;
						var p1 = _n11.a;
						var t1 = A3(terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p1);
						return _Utils_Tuple2(
							terezka$line_charts$Internal$Interpolation$Previous(t1),
							_Utils_ap(
								commands,
								_List_fromArray(
									[
										A4(terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t1, t1),
										terezka$line_charts$Internal$Path$Line(p1)
									])));
					}
				} else {
					break _n1$4;
				}
			} else {
				if (_n1.b.b && _n1.b.b.b) {
					if (_n1.b.b.b.b) {
						var t0 = _n1.a.a;
						var _n6 = _n1.b;
						var p0 = _n6.a;
						var _n7 = _n6.b;
						var p1 = _n7.a;
						var _n8 = _n7.b;
						var p2 = _n8.a;
						var rest = _n8.b;
						var t1 = A3(terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p2);
						return A2(
							terezka$line_charts$Internal$Interpolation$monotonePart,
							A2(
								elm$core$List$cons,
								p1,
								A2(elm$core$List$cons, p2, rest)),
							_Utils_Tuple2(
								terezka$line_charts$Internal$Interpolation$Previous(t1),
								_Utils_ap(
									commands,
									_List_fromArray(
										[
											A4(terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1)
										]))));
					} else {
						var t0 = _n1.a.a;
						var _n12 = _n1.b;
						var p0 = _n12.a;
						var _n13 = _n12.b;
						var p1 = _n13.a;
						var t1 = A3(terezka$line_charts$Internal$Interpolation$slope3, p0, p1, p1);
						return _Utils_Tuple2(
							terezka$line_charts$Internal$Interpolation$Previous(t1),
							_Utils_ap(
								commands,
								_List_fromArray(
									[
										A4(terezka$line_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1),
										terezka$line_charts$Internal$Path$Line(p1)
									])));
					}
				} else {
					break _n1$4;
				}
			}
		}
		return _Utils_Tuple2(tangent, commands);
	});
var terezka$line_charts$Internal$Interpolation$monotoneSection = F2(
	function (points, _n0) {
		var tangent = _n0.a;
		var acc = _n0.b;
		var _n1 = function () {
			if (points.b) {
				var p0 = points.a;
				var rest = points.b;
				return A2(
					terezka$line_charts$Internal$Interpolation$monotonePart,
					A2(elm$core$List$cons, p0, rest),
					_Utils_Tuple2(
						tangent,
						_List_fromArray(
							[
								terezka$line_charts$Internal$Path$Line(p0)
							])));
			} else {
				return _Utils_Tuple2(tangent, _List_Nil);
			}
		}();
		var t0 = _n1.a;
		var commands = _n1.b;
		return _Utils_Tuple2(
			t0,
			A2(elm$core$List$cons, commands, acc));
	});
var terezka$line_charts$Internal$Interpolation$monotone = function (sections) {
	return A3(
		elm$core$List$foldr,
		terezka$line_charts$Internal$Interpolation$monotoneSection,
		_Utils_Tuple2(terezka$line_charts$Internal$Interpolation$First, _List_Nil),
		sections).b;
};
var terezka$line_charts$Internal$Interpolation$after = F2(
	function (a, b) {
		return _List_fromArray(
			[
				a,
				A2(terezka$line_charts$Internal$Data$Point, b.x, a.y),
				b
			]);
	});
var terezka$line_charts$Internal$Interpolation$stepped = function (sections) {
	var expand = F2(
		function (result, section) {
			expand:
			while (true) {
				if (section.a.b) {
					if (section.a.b.b) {
						var _n1 = section.a;
						var a = _n1.a;
						var _n2 = _n1.b;
						var b = _n2.a;
						var rest = _n2.b;
						var broken = section.b;
						var $temp$result = _Utils_ap(
							result,
							A2(terezka$line_charts$Internal$Interpolation$after, a, b)),
							$temp$section = _Utils_Tuple2(
							A2(elm$core$List$cons, b, rest),
							broken);
						result = $temp$result;
						section = $temp$section;
						continue expand;
					} else {
						if (section.b.$ === 'Just') {
							var _n3 = section.a;
							var last = _n3.a;
							var broken = section.b.a;
							return _Utils_ap(
								result,
								_List_fromArray(
									[
										A2(terezka$line_charts$Internal$Data$Point, broken.x, last.y)
									]));
						} else {
							var _n4 = section.a;
							var last = _n4.a;
							var _n5 = section.b;
							return result;
						}
					}
				} else {
					return result;
				}
			}
		});
	return A2(
		elm$core$List$map,
		A2(
			elm$core$Basics$composeR,
			expand(_List_Nil),
			elm$core$List$map(terezka$line_charts$Internal$Path$Line)),
		sections);
};
var terezka$line_charts$Internal$Interpolation$toCommands = F2(
	function (interpolation, data) {
		var pointsSections = elm$core$List$map(
			A2(
				elm$core$Basics$composeR,
				elm$core$Tuple$mapFirst(
					elm$core$List$map(
						function ($) {
							return $.point;
						})),
				elm$core$Tuple$mapSecond(
					elm$core$Maybe$map(
						function ($) {
							return $.point;
						}))));
		var points = elm$core$List$map(
			A2(
				elm$core$Basics$composeR,
				elm$core$Tuple$first,
				elm$core$List$map(
					function ($) {
						return $.point;
					})));
		switch (interpolation.$) {
			case 'Linear':
				return terezka$line_charts$Internal$Interpolation$linear(
					points(data));
			case 'Monotone':
				return terezka$line_charts$Internal$Interpolation$monotone(
					points(data));
			default:
				return terezka$line_charts$Internal$Interpolation$stepped(
					pointsSections(data));
		}
	});
var terezka$line_charts$Internal$Area$opacitySingle = function (config) {
	switch (config.$) {
		case 'None':
			return 0;
		case 'Normal':
			var opacity_ = config.a;
			return opacity_;
		case 'Stacked':
			var opacity_ = config.a;
			return 1;
		default:
			var opacity_ = config.a;
			return 1;
	}
};
var terezka$line_charts$Internal$Path$toPoint = function (command) {
	switch (command.$) {
		case 'Close':
			return A2(terezka$line_charts$LineChart$Coordinate$Point, 0, 0);
		case 'Move':
			var p = command.a;
			return p;
		case 'Line':
			var p = command.a;
			return p;
		case 'Horizontal':
			var x = command.a;
			return A2(terezka$line_charts$LineChart$Coordinate$Point, x, 0);
		case 'Vertical':
			var y = command.a;
			return A2(terezka$line_charts$LineChart$Coordinate$Point, 0, y);
		case 'CubicBeziers':
			var c1 = command.a;
			var c2 = command.b;
			var p = command.c;
			return p;
		case 'CubicBeziersShort':
			var c1 = command.a;
			var p = command.b;
			return p;
		case 'QuadraticBeziers':
			var c1 = command.a;
			var p = command.b;
			return p;
		case 'QuadraticBeziersShort':
			var p = command.a;
			return p;
		default:
			var rx = command.a;
			var ry = command.b;
			var xAxisRotation = command.c;
			var largeArcFlag = command.d;
			var sweepFlag = command.e;
			var p = command.f;
			return p;
	}
};
var terezka$line_charts$Internal$Utils$towardsZero = function (_n0) {
	var max = _n0.max;
	var min = _n0.min;
	return A3(elm$core$Basics$clamp, min, max, 0);
};
var elm$core$List$drop = F2(
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
var terezka$line_charts$Internal$Utils$last = function (list) {
	return elm$core$List$head(
		A2(
			elm$core$List$drop,
			elm$core$List$length(list) - 1,
			list));
};
var terezka$line_charts$Internal$Utils$lastSafe = F2(
	function (first, rest) {
		return A2(
			elm$core$Maybe$withDefault,
			first,
			terezka$line_charts$Internal$Utils$last(rest));
	});
var terezka$line_charts$Internal$Utils$viewWithEdges = F2(
	function (stuff, view) {
		if (stuff.b) {
			var first = stuff.a;
			var rest = stuff.b;
			return A3(
				view,
				first,
				rest,
				A2(terezka$line_charts$Internal$Utils$lastSafe, first, rest));
		} else {
			return elm$svg$Svg$text('');
		}
	});
var terezka$line_charts$LineChart$Junk$withinChartArea = terezka$line_charts$Internal$Svg$withinChartArea;
var terezka$line_charts$Internal$Line$viewArea = F5(
	function (_n0, line_, style_, interpolation, data_) {
		var system = _n0.system;
		var lineConfig = _n0.lineConfig;
		var area = _n0.area;
		var ground = function (point) {
			return A2(
				terezka$line_charts$Internal$Data$Point,
				point.x,
				terezka$line_charts$Internal$Utils$towardsZero(system.y));
		};
		var commands = F3(
			function (first, middle, last) {
				return A3(
					terezka$line_charts$Internal$Utils$concat,
					_List_fromArray(
						[
							terezka$line_charts$Internal$Path$Move(
							ground(
								terezka$line_charts$Internal$Path$toPoint(first))),
							terezka$line_charts$Internal$Path$Line(
							terezka$line_charts$Internal$Path$toPoint(first))
						]),
					interpolation,
					_List_fromArray(
						[
							terezka$line_charts$Internal$Path$Line(
							ground(
								terezka$line_charts$Internal$Path$toPoint(last)))
						]));
			});
		var attributes = A2(
			elm$core$List$cons,
			terezka$line_charts$LineChart$Junk$withinChartArea(system),
			A2(
				elm$core$List$cons,
				elm$svg$Svg$Attributes$fillOpacity(
					elm$core$String$fromFloat(
						terezka$line_charts$Internal$Area$opacitySingle(area))),
				A3(terezka$line_charts$Internal$Line$toAreaAttributes, line_, style_, area)));
		return A2(
			terezka$line_charts$Internal$Utils$viewWithEdges,
			interpolation,
			F3(
				function (first, middle, last) {
					return A3(
						terezka$line_charts$Internal$Path$view,
						system,
						attributes,
						A3(commands, first, middle, last));
				}));
	});
var terezka$line_charts$Internal$Dots$view = F2(
	function (_n0, data) {
		var system = _n0.system;
		var dotsConfig = _n0.dotsConfig;
		var shape = _n0.shape;
		var color = _n0.color;
		var _n1 = dotsConfig;
		var config = _n1.a;
		var _n2 = config.individual(data.user);
		var style_ = _n2.a;
		return A5(terezka$line_charts$Internal$Dots$viewShape, system, style_, shape, color, data.point);
	});
var terezka$line_charts$Internal$Line$viewDot = F3(
	function (_arguments, _n0, _n1) {
		var lineConfig = _n0.a;
		var style_ = _n1.a;
		return terezka$line_charts$Internal$Dots$view(
			{
				color: style_.color(lineConfig.color),
				dotsConfig: _arguments.dotsConfig,
				shape: lineConfig.shape,
				system: _arguments.system
			});
	});
var terezka$line_charts$Internal$Utils$viewWithFirst = F2(
	function (stuff, view) {
		if (stuff.b) {
			var first = stuff.a;
			var rest = stuff.b;
			return A2(view, first, rest);
		} else {
			return elm$svg$Svg$text('');
		}
	});
var terezka$line_charts$Internal$Line$viewSeries = F5(
	function (_n0, line_, style_, interpolation, data_) {
		var system = _n0.system;
		var lineConfig = _n0.lineConfig;
		var attributes = A2(
			elm$core$List$cons,
			terezka$line_charts$LineChart$Junk$withinChartArea(system),
			A2(terezka$line_charts$Internal$Line$toSeriesAttributes, line_, style_));
		return A2(
			terezka$line_charts$Internal$Utils$viewWithFirst,
			data_,
			F2(
				function (first, _n1) {
					return A3(
						terezka$line_charts$Internal$Path$view,
						system,
						attributes,
						A2(
							elm$core$List$cons,
							terezka$line_charts$Internal$Path$Move(first.point),
							interpolation));
				}));
	});
var terezka$line_charts$Internal$Utils$part = F4(
	function (isReal, points, current, parts) {
		part:
		while (true) {
			if (points.b) {
				var first = points.a;
				var rest = points.b;
				if (isReal(first)) {
					var $temp$isReal = isReal,
						$temp$points = rest,
						$temp$current = _Utils_ap(
						current,
						_List_fromArray(
							[first])),
						$temp$parts = parts;
					isReal = $temp$isReal;
					points = $temp$points;
					current = $temp$current;
					parts = $temp$parts;
					continue part;
				} else {
					var $temp$isReal = isReal,
						$temp$points = rest,
						$temp$current = _List_Nil,
						$temp$parts = A2(
						elm$core$List$cons,
						_Utils_Tuple2(
							current,
							elm$core$Maybe$Just(first)),
						parts);
					isReal = $temp$isReal;
					points = $temp$points;
					current = $temp$current;
					parts = $temp$parts;
					continue part;
				}
			} else {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(current, elm$core$Maybe$Nothing),
					parts);
			}
		}
	});
var terezka$line_charts$Internal$Line$viewSingle = F3(
	function (_arguments, line_, data_) {
		var style_ = function (_n1) {
			var look = _n1.a;
			return look(
				A2(
					elm$core$List$map,
					function ($) {
						return $.user;
					},
					data_));
		}(_arguments.lineConfig);
		var sections = A4(
			terezka$line_charts$Internal$Utils$part,
			function ($) {
				return $.isReal;
			},
			data_,
			_List_Nil,
			_List_Nil);
		var parts = A2(elm$core$List$map, elm$core$Tuple$first, sections);
		var viewDots = A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__dots')
				]),
			A2(
				elm$core$List$map,
				A3(terezka$line_charts$Internal$Line$viewDot, _arguments, line_, style_),
				A2(
					elm$core$List$filter,
					A2(
						elm$core$Basics$composeL,
						terezka$line_charts$Internal$Data$isWithinRange(_arguments.system),
						function ($) {
							return $.point;
						}),
					elm$core$List$concat(parts))));
		var commands = A2(terezka$line_charts$Internal$Interpolation$toCommands, _arguments.interpolation, sections);
		var viewAreas = function (_n0) {
			return A2(
				elm$svg$Svg$g,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$class('chart__interpolation__area')
					]),
				A3(
					elm$core$List$map2,
					A3(terezka$line_charts$Internal$Line$viewArea, _arguments, line_, style_),
					commands,
					parts));
		};
		var viewSeriess = A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__interpolation__line')
				]),
			A3(
				elm$core$List$map2,
				A3(terezka$line_charts$Internal$Line$viewSeries, _arguments, line_, style_),
				commands,
				parts));
		return _Utils_Tuple3(
			A2(
				terezka$line_charts$Internal$Utils$viewIf,
				terezka$line_charts$Internal$Area$hasArea(_arguments.area),
				viewAreas),
			viewSeriess,
			viewDots);
	});
var terezka$line_charts$Internal$Line$viewStacked = F2(
	function (area, _n0) {
		var areas = _n0.a;
		var lines = _n0.b;
		var dots = _n0.c;
		var toList = F2(
			function (l, d) {
				return _List_fromArray(
					[l, d]);
			});
		var opacity = 'opacity: ' + elm$core$String$fromFloat(
			terezka$line_charts$Internal$Area$opacityContainer(area));
		var bottoms = elm$core$List$concat(
			A3(elm$core$List$map2, toList, lines, dots));
		return _List_fromArray(
			[
				A2(
				elm$svg$Svg$g,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$class('chart__bottoms'),
						elm$svg$Svg$Attributes$style(opacity)
					]),
				areas),
				A2(
				elm$svg$Svg$g,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$class('chart__tops')
					]),
				bottoms)
			]);
	});
var terezka$line_charts$Internal$Utils$unzip3 = function (pairs) {
	var step = F2(
		function (_n0, _n1) {
			var a = _n0.a;
			var b = _n0.b;
			var c = _n0.c;
			var aas = _n1.a;
			var bs = _n1.b;
			var cs = _n1.c;
			return _Utils_Tuple3(
				A2(elm$core$List$cons, a, aas),
				A2(elm$core$List$cons, b, bs),
				A2(elm$core$List$cons, c, cs));
		});
	return A3(
		elm$core$List$foldr,
		step,
		_Utils_Tuple3(_List_Nil, _List_Nil, _List_Nil),
		pairs);
};
var terezka$line_charts$Internal$Line$view = F3(
	function (_arguments, lines, datas) {
		var container = elm$svg$Svg$g(
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__lines')
				]));
		var buildSeriesViews = (terezka$line_charts$Internal$Area$opacityContainer(_arguments.area) < 1) ? terezka$line_charts$Internal$Line$viewStacked(_arguments.area) : terezka$line_charts$Internal$Line$viewNormal;
		return container(
			buildSeriesViews(
				terezka$line_charts$Internal$Utils$unzip3(
					A3(
						elm$core$List$map2,
						terezka$line_charts$Internal$Line$viewSingle(_arguments),
						lines,
						datas))));
	});
var terezka$line_charts$Internal$Events$toChartAttributes = F3(
	function (data, system, _n0) {
		var events = _n0.a;
		var order = function (_n1) {
			var outside = _n1.a;
			var event = _n1.b;
			return outside ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(
				A2(event, data, system));
		};
		return A2(elm$core$List$filterMap, order, events);
	});
var terezka$line_charts$LineChart$chartAreaAttributes = function (system) {
	return _List_fromArray(
		[
			elm$svg$Svg$Attributes$x(
			elm$core$String$fromFloat(system.frame.margin.left)),
			elm$svg$Svg$Attributes$y(
			elm$core$String$fromFloat(system.frame.margin.top)),
			elm$svg$Svg$Attributes$width(
			elm$core$String$fromFloat(
				terezka$line_charts$Internal$Coordinate$lengthX(system))),
			elm$svg$Svg$Attributes$height(
			elm$core$String$fromFloat(
				terezka$line_charts$Internal$Coordinate$lengthY(system)))
		]);
};
var terezka$line_charts$LineChart$chartAreaPlatform = F3(
	function (config, data, system) {
		var attributes = elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[
						elm$svg$Svg$Attributes$fill('transparent')
					]),
					terezka$line_charts$LineChart$chartAreaAttributes(system),
					A3(terezka$line_charts$Internal$Events$toChartAttributes, data, system, config.events)
				]));
		return A2(elm$svg$Svg$rect, attributes, _List_Nil);
	});
var elm$svg$Svg$clipPath = elm$svg$Svg$trustedNode('clipPath');
var elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var terezka$line_charts$LineChart$clipPath = function (system) {
	return A2(
		elm$svg$Svg$clipPath,
		_List_fromArray(
			[
				elm$svg$Svg$Attributes$id(
				terezka$line_charts$Internal$Utils$toChartAreaId(system.id))
			]),
		_List_fromArray(
			[
				A2(
				elm$svg$Svg$rect,
				terezka$line_charts$LineChart$chartAreaAttributes(system),
				_List_Nil)
			]));
};
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var terezka$line_charts$Internal$Container$sizeStyles = F3(
	function (_n0, width, height) {
		var properties_ = _n0.a;
		var _n1 = properties_.size;
		if (_n1.$ === 'Static') {
			return _List_fromArray(
				[
					A2(
					elm$html$Html$Attributes$style,
					'height',
					elm$core$String$fromFloat(height) + 'px'),
					A2(
					elm$html$Html$Attributes$style,
					'width',
					elm$core$String$fromFloat(width) + 'px')
				]);
		} else {
			return _List_Nil;
		}
	});
var terezka$line_charts$LineChart$container = F4(
	function (config, _n0, junkHtml, plot) {
		var frame = _n0.frame;
		var userAttributes = A2(
			terezka$line_charts$Internal$Container$properties,
			function ($) {
				return $.attributesHtml;
			},
			config.container);
		var sizeStyles = A3(terezka$line_charts$Internal$Container$sizeStyles, config.container, frame.size.width, frame.size.height);
		var styles = A2(
			elm$core$List$cons,
			A2(elm$html$Html$Attributes$style, 'position', 'relative'),
			sizeStyles);
		return A2(
			elm$html$Html$div,
			_Utils_ap(styles, userAttributes),
			A2(elm$core$List$cons, plot, junkHtml));
	});
var terezka$line_charts$Internal$Data$Data = F3(
	function (user, point, isReal) {
		return {isReal: isReal, point: point, user: user};
	});
var terezka$line_charts$LineChart$setY = F2(
	function (datum, y) {
		return A3(
			terezka$line_charts$Internal$Data$Data,
			datum.user,
			A2(terezka$line_charts$Internal$Data$Point, datum.point.x, y),
			datum.isReal);
	});
var terezka$line_charts$LineChart$normalize = function (datasets) {
	if (datasets.b) {
		var highest = datasets.a;
		var belows = datasets.b;
		var toPercentage = F2(
			function (highest_, datum) {
				return A2(terezka$line_charts$LineChart$setY, datum, (100 * datum.point.y) / highest_.point.y);
			});
		return A2(
			elm$core$List$map,
			A2(elm$core$List$map2, toPercentage, highest),
			A2(elm$core$List$cons, highest, belows));
	} else {
		return datasets;
	}
};
var terezka$line_charts$Internal$Utils$withFirst = F2(
	function (stuff, process) {
		if (stuff.b) {
			var first = stuff.a;
			var rest = stuff.b;
			return elm$core$Maybe$Just(
				A2(process, first, rest));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var terezka$line_charts$LineChart$addBelows = F2(
	function (alldata, dataBelowAll) {
		var add = F2(
			function (below, datum) {
				return A2(terezka$line_charts$LineChart$setY, below, below.point.y + datum.point.y);
			});
		var iterate = F4(
			function (datum0, dataTop, dataBelowTop, result) {
				iterate:
				while (true) {
					var _n0 = _Utils_Tuple2(dataTop, dataBelowTop);
					if (_n0.a.b) {
						if (_n0.b.b) {
							var _n1 = _n0.a;
							var datum1 = _n1.a;
							var data = _n1.b;
							var _n2 = _n0.b;
							var datumBelow = _n2.a;
							var dataBelow = _n2.b;
							if (_Utils_cmp(datum1.point.x, datumBelow.point.x) > 0) {
								if (datumBelow.isReal) {
									var $temp$datum0 = datum0,
										$temp$dataTop = A2(elm$core$List$cons, datum1, data),
										$temp$dataBelowTop = dataBelow,
										$temp$result = A2(
										elm$core$List$cons,
										A2(add, datumBelow, datum0),
										result);
									datum0 = $temp$datum0;
									dataTop = $temp$dataTop;
									dataBelowTop = $temp$dataBelowTop;
									result = $temp$result;
									continue iterate;
								} else {
									var breakdata = _Utils_update(
										datum0,
										{isReal: false});
									var $temp$datum0 = datum0,
										$temp$dataTop = A2(elm$core$List$cons, datum1, data),
										$temp$dataBelowTop = dataBelow,
										$temp$result = A2(
										elm$core$List$cons,
										A2(add, datumBelow, datum0),
										result);
									datum0 = $temp$datum0;
									dataTop = $temp$dataTop;
									dataBelowTop = $temp$dataBelowTop;
									result = $temp$result;
									continue iterate;
								}
							} else {
								var $temp$datum0 = datum1,
									$temp$dataTop = data,
									$temp$dataBelowTop = A2(elm$core$List$cons, datumBelow, dataBelow),
									$temp$result = result;
								datum0 = $temp$datum0;
								dataTop = $temp$dataTop;
								dataBelowTop = $temp$dataBelowTop;
								result = $temp$result;
								continue iterate;
							}
						} else {
							var _n4 = _n0.a;
							var datum1 = _n4.a;
							var data = _n4.b;
							return result;
						}
					} else {
						if (_n0.b.b) {
							var _n3 = _n0.b;
							var datumBelow = _n3.a;
							var dataBelow = _n3.b;
							if (_Utils_cmp(datum0.point.x, datumBelow.point.x) < 1) {
								var $temp$datum0 = datum0,
									$temp$dataTop = _List_Nil,
									$temp$dataBelowTop = dataBelow,
									$temp$result = A2(
									elm$core$List$cons,
									A2(add, datumBelow, datum0),
									result);
								datum0 = $temp$datum0;
								dataTop = $temp$dataTop;
								dataBelowTop = $temp$dataBelowTop;
								result = $temp$result;
								continue iterate;
							} else {
								var $temp$datum0 = datum0,
									$temp$dataTop = _List_Nil,
									$temp$dataBelowTop = dataBelow,
									$temp$result = A2(elm$core$List$cons, datumBelow, result);
								datum0 = $temp$datum0;
								dataTop = $temp$dataTop;
								dataBelowTop = $temp$dataBelowTop;
								result = $temp$result;
								continue iterate;
							}
						} else {
							return result;
						}
					}
				}
			});
		return elm$core$List$reverse(
			A2(
				elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					terezka$line_charts$Internal$Utils$withFirst,
					alldata,
					F2(
						function (first, rest) {
							return A4(iterate, first, rest, dataBelowAll, _List_Nil);
						}))));
	});
var terezka$line_charts$LineChart$stack = function (dataset) {
	var stackBelows = F2(
		function (dataset_, result) {
			if (dataset_.b) {
				var data = dataset_.a;
				var belows = dataset_.b;
				return A2(
					stackBelows,
					belows,
					A2(
						elm$core$List$cons,
						A3(elm$core$List$foldl, terezka$line_charts$LineChart$addBelows, data, belows),
						result));
			} else {
				return result;
			}
		});
	return elm$core$List$reverse(
		A2(stackBelows, dataset, _List_Nil));
};
var terezka$line_charts$LineChart$toDataPoints = F2(
	function (config, lines) {
		var y = terezka$line_charts$Internal$Axis$variable(config.y);
		var x = terezka$line_charts$Internal$Axis$variable(config.x);
		var addPoint = function (datum) {
			var _n1 = _Utils_Tuple2(
				x(datum),
				y(datum));
			if (_n1.a.$ === 'Just') {
				if (_n1.b.$ === 'Just') {
					var x_ = _n1.a.a;
					var y_ = _n1.b.a;
					return elm$core$Maybe$Just(
						A3(
							terezka$line_charts$Internal$Data$Data,
							datum,
							A2(terezka$line_charts$Internal$Data$Point, x_, y_),
							true));
				} else {
					var x_ = _n1.a.a;
					var _n2 = _n1.b;
					return elm$core$Maybe$Just(
						A3(
							terezka$line_charts$Internal$Data$Data,
							datum,
							A2(terezka$line_charts$Internal$Data$Point, x_, 0),
							false));
				}
			} else {
				if (_n1.b.$ === 'Just') {
					var _n3 = _n1.a;
					var y_ = _n1.b.a;
					return elm$core$Maybe$Nothing;
				} else {
					var _n4 = _n1.a;
					var _n5 = _n1.b;
					return elm$core$Maybe$Nothing;
				}
			}
		};
		var data = A2(
			elm$core$List$map,
			A2(
				elm$core$Basics$composeR,
				terezka$line_charts$Internal$Line$data,
				elm$core$List$filterMap(addPoint)),
			lines);
		var _n0 = config.area;
		switch (_n0.$) {
			case 'None':
				return data;
			case 'Normal':
				return data;
			case 'Stacked':
				return terezka$line_charts$LineChart$stack(data);
			default:
				return terezka$line_charts$LineChart$normalize(
					terezka$line_charts$LineChart$stack(data));
		}
	});
var terezka$line_charts$Internal$Axis$pixels = function (_n0) {
	var config = _n0.a;
	return config.pixels;
};
var terezka$line_charts$Internal$Axis$range = function (_n0) {
	var config = _n0.a;
	return config.range;
};
var terezka$line_charts$LineChart$Coordinate$Range = F2(
	function (min, max) {
		return {max: max, min: min};
	});
var terezka$line_charts$Internal$Axis$Range$applyX = F2(
	function (range, system) {
		switch (range.$) {
			case 'Padded':
				var padMin = range.a;
				var padMax = range.b;
				var _n1 = system;
				var frame = _n1.frame;
				var _n2 = frame;
				var size = _n2.size;
				var system_ = _Utils_update(
					system,
					{
						frame: _Utils_update(
							frame,
							{
								size: _Utils_update(
									size,
									{
										width: A2(elm$core$Basics$max, 1, (size.width - padMin) - padMax)
									})
							})
					});
				var scale = terezka$line_charts$LineChart$Coordinate$scaleDataX(system_);
				return A2(
					terezka$line_charts$LineChart$Coordinate$Range,
					system.x.min - scale(padMin),
					system.x.max + scale(padMax));
			case 'Window':
				var min = range.a;
				var max = range.b;
				return A2(terezka$line_charts$LineChart$Coordinate$Range, min, max);
			default:
				var toRange = range.a;
				return toRange(system.x);
		}
	});
var terezka$line_charts$Internal$Axis$Range$applyY = F2(
	function (range, system) {
		switch (range.$) {
			case 'Padded':
				var padMin = range.a;
				var padMax = range.b;
				var _n1 = system;
				var frame = _n1.frame;
				var _n2 = frame;
				var size = _n2.size;
				var system_ = _Utils_update(
					system,
					{
						frame: _Utils_update(
							frame,
							{
								size: _Utils_update(
									size,
									{
										height: A2(elm$core$Basics$max, 1, (size.height - padMin) - padMax)
									})
							})
					});
				var scale = terezka$line_charts$LineChart$Coordinate$scaleDataY(system_);
				return A2(
					terezka$line_charts$LineChart$Coordinate$Range,
					system.y.min - scale(padMin),
					system.y.max + scale(padMax));
			case 'Window':
				var min = range.a;
				var max = range.b;
				return A2(terezka$line_charts$LineChart$Coordinate$Range, min, max);
			default:
				var toRange = range.a;
				return toRange(system.y);
		}
	});
var terezka$line_charts$Internal$Coordinate$Frame = F2(
	function (margin, size) {
		return {margin: margin, size: size};
	});
var terezka$line_charts$Internal$Coordinate$Size = F2(
	function (width, height) {
		return {height: height, width: width};
	});
var terezka$line_charts$Internal$Coordinate$ground = function (range_) {
	return _Utils_update(
		range_,
		{
			min: A2(elm$core$Basics$min, range_.min, 0)
		});
};
var elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$max, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var terezka$line_charts$Internal$Coordinate$maximum = function (toValue) {
	return A2(
		elm$core$Basics$composeR,
		elm$core$List$map(toValue),
		A2(
			elm$core$Basics$composeR,
			elm$core$List$maximum,
			elm$core$Maybe$withDefault(1)));
};
var elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$min, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var terezka$line_charts$Internal$Coordinate$minimum = function (toValue) {
	return A2(
		elm$core$Basics$composeR,
		elm$core$List$map(toValue),
		A2(
			elm$core$Basics$composeR,
			elm$core$List$minimum,
			elm$core$Maybe$withDefault(0)));
};
var terezka$line_charts$Internal$Coordinate$range = F2(
	function (toValue, data) {
		var range_ = {
			max: A2(terezka$line_charts$Internal$Coordinate$maximum, toValue, data),
			min: A2(terezka$line_charts$Internal$Coordinate$minimum, toValue, data)
		};
		return _Utils_eq(range_.min, range_.max) ? _Utils_update(
			range_,
			{max: range_.max + 1}) : range_;
	});
var terezka$line_charts$LineChart$toSystem = F2(
	function (config, data) {
		var yRange = A2(
			terezka$line_charts$Internal$Coordinate$range,
			A2(
				elm$core$Basics$composeR,
				function ($) {
					return $.point;
				},
				function ($) {
					return $.y;
				}),
			data);
		var xRange = A2(
			terezka$line_charts$Internal$Coordinate$range,
			A2(
				elm$core$Basics$composeR,
				function ($) {
					return $.point;
				},
				function ($) {
					return $.x;
				}),
			data);
		var size = A2(
			terezka$line_charts$Internal$Coordinate$Size,
			terezka$line_charts$Internal$Axis$pixels(config.x),
			terezka$line_charts$Internal$Axis$pixels(config.y));
		var hasArea = terezka$line_charts$Internal$Area$hasArea(config.area);
		var container_ = A2(terezka$line_charts$Internal$Container$properties, elm$core$Basics$identity, config.container);
		var frame = A2(terezka$line_charts$Internal$Coordinate$Frame, container_.margin, size);
		var adjustDomainRange = function (domain) {
			return hasArea ? terezka$line_charts$Internal$Coordinate$ground(domain) : domain;
		};
		var system = {
			frame: frame,
			id: container_.id,
			x: xRange,
			xData: xRange,
			y: adjustDomainRange(yRange),
			yData: yRange
		};
		return _Utils_update(
			system,
			{
				x: A2(
					terezka$line_charts$Internal$Axis$Range$applyX,
					terezka$line_charts$Internal$Axis$range(config.x),
					system),
				y: A2(
					terezka$line_charts$Internal$Axis$Range$applyY,
					terezka$line_charts$Internal$Axis$range(config.y),
					system)
			});
	});
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var terezka$line_charts$LineChart$viewBoxAttribute = function (_n0) {
	var frame = _n0.frame;
	return elm$svg$Svg$Attributes$viewBox(
		'0 0 ' + (elm$core$String$fromFloat(frame.size.width) + (' ' + elm$core$String$fromFloat(frame.size.height))));
};
var terezka$line_charts$LineChart$viewCustom = F2(
	function (config, lines) {
		var junkLineInfo = function (line_) {
			return _Utils_Tuple3(
				A3(terezka$line_charts$Internal$Line$color, config.line, line_, _List_Nil),
				terezka$line_charts$Internal$Line$label(line_),
				terezka$line_charts$Internal$Line$data(line_));
		};
		var getJunk = A3(
			terezka$line_charts$Internal$Junk$getLayers,
			A2(elm$core$List$map, junkLineInfo, lines),
			terezka$line_charts$Internal$Axis$variable(config.x),
			terezka$line_charts$Internal$Axis$variable(config.y));
		var data = A2(terezka$line_charts$LineChart$toDataPoints, config, lines);
		var dataAll = elm$core$List$concat(data);
		var dataSafe = A2(
			elm$core$List$map,
			elm$core$List$filter(
				function ($) {
					return $.isReal;
				}),
			data);
		var dataAllSafe = elm$core$List$concat(dataSafe);
		var system = A2(terezka$line_charts$LineChart$toSystem, config, dataAllSafe);
		var viewLines = terezka$line_charts$Internal$Line$view(
			{area: config.area, dotsConfig: config.dots, interpolation: config.interpolation, lineConfig: config.line, system: system});
		var viewLegends = terezka$line_charts$Internal$Legends$view(
			{
				area: config.area,
				data: dataSafe,
				dotsConfig: config.dots,
				legends: config.legends,
				lineConfig: config.line,
				lines: lines,
				system: system,
				x: terezka$line_charts$Internal$Axis$variable(config.x),
				y: terezka$line_charts$Internal$Axis$variable(config.y)
			});
		var attributes = elm$core$List$concat(
			_List_fromArray(
				[
					A2(
					terezka$line_charts$Internal$Container$properties,
					function ($) {
						return $.attributesSvg;
					},
					config.container),
					A3(terezka$line_charts$Internal$Events$toContainerAttributes, dataAll, system, config.events),
					_List_fromArray(
					[
						terezka$line_charts$LineChart$viewBoxAttribute(system)
					])
				]));
		var addGrid = terezka$line_charts$Internal$Junk$addBelow(
			A4(terezka$line_charts$Internal$Grid$view, system, config.x, config.y, config.grid));
		var junk = addGrid(
			A2(getJunk, system, config.junk));
		return A4(
			terezka$line_charts$LineChart$container,
			config,
			system,
			junk.html,
			A2(
				elm$svg$Svg$svg,
				attributes,
				_List_fromArray(
					[
						A2(
						elm$svg$Svg$defs,
						_List_Nil,
						_List_fromArray(
							[
								terezka$line_charts$LineChart$clipPath(system)
							])),
						A2(
						elm$svg$Svg$g,
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$class('chart__junk--below')
							]),
						junk.below),
						A2(viewLines, lines, data),
						A3(terezka$line_charts$LineChart$chartAreaPlatform, config, dataAll, system),
						A3(terezka$line_charts$Internal$Axis$viewHorizontal, system, config.intersection, config.x),
						A3(terezka$line_charts$Internal$Axis$viewVertical, system, config.intersection, config.y),
						viewLegends,
						A2(
						elm$svg$Svg$g,
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$class('chart__junk--above')
							]),
						junk.above)
					])));
	});
var terezka$line_charts$Internal$Area$None = {$: 'None'};
var terezka$line_charts$Internal$Area$none = terezka$line_charts$Internal$Area$None;
var terezka$line_charts$LineChart$Area$default = terezka$line_charts$Internal$Area$none;
var terezka$line_charts$Internal$Axis$Line$rangeFrame = function (color) {
	return terezka$line_charts$Internal$Axis$Line$custom(
		F2(
			function (data, range) {
				var smallest = A2(terezka$line_charts$Internal$Coordinate$smallestRange, data, range);
				return {color: color, end: smallest.max, events: _List_Nil, start: smallest.min, width: 1};
			}));
};
var terezka$line_charts$Internal$Axis$Title$atDataMax = function () {
	var position = F2(
		function (data, range) {
			return A2(elm$core$Basics$min, data.max, range.max);
		});
	return terezka$line_charts$Internal$Axis$Title$atPosition(position);
}();
var terezka$line_charts$Internal$Axis$Tick$Negative = {$: 'Negative'};
var terezka$line_charts$Internal$Axis$Tick$float = function (n) {
	return terezka$line_charts$Internal$Axis$Tick$custom(
		{
			color: terezka$line_charts$LineChart$Colors$gray,
			direction: terezka$line_charts$Internal$Axis$Tick$Negative,
			grid: true,
			label: elm$core$Maybe$Just(
				A2(
					terezka$line_charts$Internal$Svg$label,
					'inherit',
					elm$core$String$fromFloat(n))),
			length: 5,
			position: n,
			width: 1
		});
};
var terezka$line_charts$LineChart$Axis$Tick$float = terezka$line_charts$Internal$Axis$Tick$float;
var terezka$line_charts$Internal$Axis$default = F3(
	function (pixels_, title_, variable_) {
		return terezka$line_charts$Internal$Axis$custom(
			{
				axisLine: terezka$line_charts$Internal$Axis$Line$rangeFrame(terezka$line_charts$LineChart$Colors$gray),
				pixels: pixels_,
				range: A2(terezka$line_charts$Internal$Axis$Range$padded, 20, 20),
				ticks: terezka$line_charts$Internal$Axis$Ticks$custom(
					F2(
						function (data, range_) {
							var smallest = A2(terezka$line_charts$Internal$Coordinate$smallestRange, data, range_);
							var rangeSmall = smallest.max - smallest.min;
							var rangeLong = range_.max - range_.min;
							var diff = 1 - ((rangeLong - rangeSmall) / rangeLong);
							var amount = elm$core$Basics$round((diff * pixels_) / 90);
							return A2(
								elm$core$List$map,
								terezka$line_charts$LineChart$Axis$Tick$float,
								A2(
									terezka$line_charts$Internal$Axis$Values$float,
									terezka$line_charts$Internal$Axis$Values$around(amount),
									smallest));
						})),
				title: A3(terezka$line_charts$Internal$Axis$Title$atDataMax, 0, 0, title_),
				variable: A2(elm$core$Basics$composeL, elm$core$Maybe$Just, variable_)
			});
	});
var terezka$line_charts$LineChart$Axis$default = terezka$line_charts$Internal$Axis$default;
var terezka$line_charts$Internal$Axis$Intersection$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Axis$Intersection$custom = F2(
	function (toX, toY) {
		return terezka$line_charts$Internal$Axis$Intersection$Config(
			function (_n0) {
				var x = _n0.x;
				var y = _n0.y;
				return A2(
					terezka$line_charts$Internal$Data$Point,
					toX(x),
					toY(y));
			});
	});
var terezka$line_charts$Internal$Axis$Intersection$default = A2(
	terezka$line_charts$Internal$Axis$Intersection$custom,
	function ($) {
		return $.min;
	},
	function ($) {
		return $.min;
	});
var terezka$line_charts$LineChart$Axis$Intersection$default = terezka$line_charts$Internal$Axis$Intersection$default;
var terezka$line_charts$Internal$Container$Margin = F4(
	function (top, right, bottom, left) {
		return {bottom: bottom, left: left, right: right, top: top};
	});
var terezka$line_charts$Internal$Container$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Container$custom = terezka$line_charts$Internal$Container$Config;
var terezka$line_charts$Internal$Container$Static = {$: 'Static'};
var terezka$line_charts$Internal$Container$static = terezka$line_charts$Internal$Container$Static;
var terezka$line_charts$Internal$Container$styled = F2(
	function (id, styles) {
		return terezka$line_charts$Internal$Container$custom(
			{
				attributesHtml: A2(
					elm$core$List$map,
					function (_n0) {
						var p = _n0.a;
						var v = _n0.b;
						return A2(elm$html$Html$Attributes$style, p, v);
					},
					styles),
				attributesSvg: _List_Nil,
				id: id,
				margin: A4(terezka$line_charts$Internal$Container$Margin, 60, 140, 60, 80),
				size: terezka$line_charts$Internal$Container$static
			});
	});
var terezka$line_charts$LineChart$Container$styled = terezka$line_charts$Internal$Container$styled;
var terezka$line_charts$Internal$Dots$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Dots$customAny = terezka$line_charts$Internal$Dots$Config;
var terezka$line_charts$Internal$Dots$Aura = F2(
	function (a, b) {
		return {$: 'Aura', a: a, b: b};
	});
var terezka$line_charts$Internal$Dots$Style = function (a) {
	return {$: 'Style', a: a};
};
var terezka$line_charts$Internal$Dots$style = F2(
	function (radius, variety) {
		return terezka$line_charts$Internal$Dots$Style(
			{radius: radius, variety: variety});
	});
var terezka$line_charts$Internal$Dots$aura = F3(
	function (radius, aura_, opacity) {
		return A2(
			terezka$line_charts$Internal$Dots$style,
			radius,
			A2(terezka$line_charts$Internal$Dots$Aura, aura_, opacity));
	});
var terezka$line_charts$LineChart$Dots$aura = terezka$line_charts$Internal$Dots$aura;
var terezka$line_charts$Internal$Dots$Disconnected = function (a) {
	return {$: 'Disconnected', a: a};
};
var terezka$line_charts$Internal$Dots$disconnected = F2(
	function (radius, border) {
		return A2(
			terezka$line_charts$Internal$Dots$style,
			radius,
			terezka$line_charts$Internal$Dots$Disconnected(border));
	});
var terezka$line_charts$LineChart$Dots$disconnected = terezka$line_charts$Internal$Dots$disconnected;
var terezka$line_charts$LineChart$Dots$hoverOne = function (maybeHovered) {
	var styleLegend = function (_n0) {
		return A2(terezka$line_charts$LineChart$Dots$disconnected, 10, 2);
	};
	var styleIndividual = function (datum) {
		return _Utils_eq(
			elm$core$Maybe$Just(datum),
			maybeHovered) ? A3(terezka$line_charts$LineChart$Dots$aura, 7, 6, 0.3) : A2(terezka$line_charts$LineChart$Dots$disconnected, 10, 2);
	};
	return terezka$line_charts$Internal$Dots$customAny(
		{individual: styleIndividual, legend: styleLegend});
};
var terezka$line_charts$Internal$Dots$None = {$: 'None'};
var terezka$line_charts$LineChart$Dots$none = terezka$line_charts$Internal$Dots$None;
var terezka$line_charts$Internal$Events$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Events$custom = terezka$line_charts$Internal$Events$Config;
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var terezka$line_charts$Internal$Events$Decoder = function (a) {
	return {$: 'Decoder', a: a};
};
var terezka$line_charts$Internal$Events$distanceX = F3(
	function (system, searched, dot) {
		return elm$core$Basics$abs(
			A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, dot.x) - A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, searched.x));
	});
var terezka$line_charts$Internal$Events$distanceY = F3(
	function (system, searched, dot) {
		return elm$core$Basics$abs(
			A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, dot.y) - A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, searched.y));
	});
var terezka$line_charts$Internal$Events$distance = F3(
	function (system, searched, dot) {
		return elm$core$Basics$sqrt(
			A2(
				elm$core$Basics$pow,
				A3(terezka$line_charts$Internal$Events$distanceX, system, searched, dot),
				2) + A2(
				elm$core$Basics$pow,
				A3(terezka$line_charts$Internal$Events$distanceY, system, searched, dot),
				2));
	});
var terezka$line_charts$Internal$Events$getNearestHelp = F3(
	function (points, system, searched) {
		var distance_ = A2(terezka$line_charts$Internal$Events$distance, system, searched);
		var getClosest = F2(
			function (point, closest) {
				return (_Utils_cmp(
					distance_(closest.point),
					distance_(point.point)) < 0) ? closest : point;
			});
		return A2(
			terezka$line_charts$Internal$Utils$withFirst,
			A2(
				elm$core$List$filter,
				function ($) {
					return $.isReal;
				},
				points),
			elm$core$List$foldl(getClosest));
	});
var terezka$line_charts$Internal$Events$withinRadius = F4(
	function (system, radius, searched, dot) {
		return _Utils_cmp(
			A3(terezka$line_charts$Internal$Events$distance, system, searched, dot),
			radius) < 1;
	});
var terezka$line_charts$Internal$Events$getWithin = function (radius) {
	return terezka$line_charts$Internal$Events$Decoder(
		F3(
			function (points, system, searchedSvg) {
				var searched = A2(terezka$line_charts$LineChart$Coordinate$toData, system, searchedSvg);
				var keepIfEligible = function (closest) {
					return A4(terezka$line_charts$Internal$Events$withinRadius, system, radius, searched, closest.point) ? elm$core$Maybe$Just(closest.user) : elm$core$Maybe$Nothing;
				};
				return A2(
					elm$core$Maybe$andThen,
					keepIfEligible,
					A3(terezka$line_charts$Internal$Events$getNearestHelp, points, system, searched));
			}));
};
var elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var elm$svg$Svg$Events$custom = elm$html$Html$Events$custom;
var terezka$line_charts$Internal$Events$Event = F2(
	function (a, b) {
		return {$: 'Event', a: a, b: b};
	});
var terezka$line_charts$Internal$Events$Options = F3(
	function (stopPropagation, preventDefault, catchOutsideChart) {
		return {catchOutsideChart: catchOutsideChart, preventDefault: preventDefault, stopPropagation: stopPropagation};
	});
var terezka$line_charts$Internal$Events$map = F2(
	function (f, _n0) {
		var a = _n0.a;
		return terezka$line_charts$Internal$Events$Decoder(
			F3(
				function (ps, s, p) {
					return f(
						A3(a, ps, s, p));
				}));
	});
var debois$elm_dom$DOM$target = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'target', decoder);
};
var debois$elm_dom$DOM$offsetHeight = A2(elm$json$Json$Decode$field, 'offsetHeight', elm$json$Json$Decode$float);
var debois$elm_dom$DOM$offsetWidth = A2(elm$json$Json$Decode$field, 'offsetWidth', elm$json$Json$Decode$float);
var debois$elm_dom$DOM$offsetLeft = A2(elm$json$Json$Decode$field, 'offsetLeft', elm$json$Json$Decode$float);
var elm$json$Json$Decode$null = _Json_decodeNull;
var debois$elm_dom$DOM$offsetParent = F2(
	function (x, decoder) {
		return elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					elm$json$Json$Decode$field,
					'offsetParent',
					elm$json$Json$Decode$null(x)),
					A2(elm$json$Json$Decode$field, 'offsetParent', decoder)
				]));
	});
var debois$elm_dom$DOM$offsetTop = A2(elm$json$Json$Decode$field, 'offsetTop', elm$json$Json$Decode$float);
var debois$elm_dom$DOM$scrollLeft = A2(elm$json$Json$Decode$field, 'scrollLeft', elm$json$Json$Decode$float);
var debois$elm_dom$DOM$scrollTop = A2(elm$json$Json$Decode$field, 'scrollTop', elm$json$Json$Decode$float);
var elm$json$Json$Decode$map4 = _Json_map4;
var debois$elm_dom$DOM$position = F2(
	function (x, y) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (_n0) {
				var x_ = _n0.a;
				var y_ = _n0.b;
				return A2(
					debois$elm_dom$DOM$offsetParent,
					_Utils_Tuple2(x_, y_),
					A2(debois$elm_dom$DOM$position, x_, y_));
			},
			A5(
				elm$json$Json$Decode$map4,
				F4(
					function (scrollLeftP, scrollTopP, offsetLeftP, offsetTopP) {
						return _Utils_Tuple2((x + offsetLeftP) - scrollLeftP, (y + offsetTopP) - scrollTopP);
					}),
				debois$elm_dom$DOM$scrollLeft,
				debois$elm_dom$DOM$scrollTop,
				debois$elm_dom$DOM$offsetLeft,
				debois$elm_dom$DOM$offsetTop));
	});
var debois$elm_dom$DOM$boundingClientRect = A4(
	elm$json$Json$Decode$map3,
	F3(
		function (_n0, width, height) {
			var x = _n0.a;
			var y = _n0.b;
			return {height: height, left: x, top: y, width: width};
		}),
	A2(debois$elm_dom$DOM$position, 0, 0),
	debois$elm_dom$DOM$offsetWidth,
	debois$elm_dom$DOM$offsetHeight);
var debois$elm_dom$DOM$parentElement = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'parentElement', decoder);
};
var elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		elm$json$Json$Decode$andThen,
		thunk,
		elm$json$Json$Decode$succeed(_Utils_Tuple0));
};
function terezka$line_charts$Internal$Events$cyclic$position() {
	return elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				debois$elm_dom$DOM$boundingClientRect,
				elm$json$Json$Decode$lazy(
				function (_n0) {
					return debois$elm_dom$DOM$parentElement(
						terezka$line_charts$Internal$Events$cyclic$position());
				})
			]));
}
try {
	var terezka$line_charts$Internal$Events$position = terezka$line_charts$Internal$Events$cyclic$position();
	terezka$line_charts$Internal$Events$cyclic$position = function () {
		return terezka$line_charts$Internal$Events$position;
	};
} catch ($) {
throw 'Some top-level definitions from `Internal.Events` are causing infinite recursion:\n\n  ┌─────┐\n  │    position\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var terezka$line_charts$Internal$Events$toJsonDecoder = F4(
	function (options, data, system, _n0) {
		var decoder = _n0.a;
		var withOptions = function (msg) {
			return {message: msg, preventDefault: options.preventDefault, stopPropagation: options.stopPropagation};
		};
		var handle = F3(
			function (mouseX, mouseY, _n1) {
				var left = _n1.left;
				var top = _n1.top;
				var height = _n1.height;
				var width = _n1.width;
				var y = mouseY - top;
				var x = mouseX - left;
				var widthPercent = width / system.frame.size.width;
				var newSize = {height: height, width: width};
				var heightPercent = height / system.frame.size.height;
				var newMargin = {bottom: system.frame.margin.bottom * heightPercent, left: system.frame.margin.left * widthPercent, right: system.frame.margin.right * widthPercent, top: system.frame.margin.top * heightPercent};
				var newSystem = _Utils_update(
					system,
					{
						frame: {margin: newMargin, size: newSize}
					});
				return A3(
					decoder,
					data,
					newSystem,
					A2(terezka$line_charts$LineChart$Coordinate$Point, x, y));
			});
		return A2(
			elm$json$Json$Decode$map,
			withOptions,
			A4(
				elm$json$Json$Decode$map3,
				handle,
				A2(elm$json$Json$Decode$field, 'pageX', elm$json$Json$Decode$float),
				A2(elm$json$Json$Decode$field, 'pageY', elm$json$Json$Decode$float),
				debois$elm_dom$DOM$target(terezka$line_charts$Internal$Events$position)));
	});
var terezka$line_charts$Internal$Events$on = F3(
	function (event, toMsg, decoder) {
		return A2(
			terezka$line_charts$Internal$Events$Event,
			false,
			F2(
				function (data, system) {
					var defaultOptions = A3(terezka$line_charts$Internal$Events$Options, false, false, false);
					return A2(
						elm$svg$Svg$Events$custom,
						event,
						A4(
							terezka$line_charts$Internal$Events$toJsonDecoder,
							defaultOptions,
							data,
							system,
							A2(terezka$line_charts$Internal$Events$map, toMsg, decoder)));
				}));
	});
var elm$svg$Svg$Events$on = elm$html$Html$Events$on;
var terezka$line_charts$Internal$Events$onMouseLeave = function (msg) {
	return A2(
		terezka$line_charts$Internal$Events$Event,
		false,
		F2(
			function (_n0, _n1) {
				return A2(
					elm$svg$Svg$Events$on,
					'mouseleave',
					elm$json$Json$Decode$succeed(msg));
			}));
};
var terezka$line_charts$Internal$Events$onMouseMove = terezka$line_charts$Internal$Events$on('mousemove');
var terezka$line_charts$Internal$Events$hoverOne = function (msg) {
	return terezka$line_charts$Internal$Events$custom(
		_List_fromArray(
			[
				A2(
				terezka$line_charts$Internal$Events$onMouseMove,
				msg,
				terezka$line_charts$Internal$Events$getWithin(30)),
				A3(
				terezka$line_charts$Internal$Events$on,
				'touchstart',
				msg,
				terezka$line_charts$Internal$Events$getWithin(100)),
				A3(
				terezka$line_charts$Internal$Events$on,
				'touchmove',
				msg,
				terezka$line_charts$Internal$Events$getWithin(100)),
				terezka$line_charts$Internal$Events$onMouseLeave(
				msg(elm$core$Maybe$Nothing))
			]));
};
var terezka$line_charts$LineChart$Events$hoverOne = terezka$line_charts$Internal$Events$hoverOne;
var terezka$line_charts$Internal$Grid$Lines = F2(
	function (a, b) {
		return {$: 'Lines', a: a, b: b};
	});
var terezka$line_charts$Internal$Grid$lines = terezka$line_charts$Internal$Grid$Lines;
var terezka$line_charts$LineChart$Colors$grayLightest = A3(avh4$elm_color$Color$rgb255, 243, 243, 243);
var terezka$line_charts$Internal$Grid$default = A2(terezka$line_charts$Internal$Grid$lines, 1, terezka$line_charts$LineChart$Colors$grayLightest);
var terezka$line_charts$LineChart$Grid$default = terezka$line_charts$Internal$Grid$default;
var terezka$line_charts$Internal$Interpolation$Linear = {$: 'Linear'};
var terezka$line_charts$LineChart$Interpolation$linear = terezka$line_charts$Internal$Interpolation$Linear;
var terezka$line_charts$LineChart$Interpolation$default = terezka$line_charts$LineChart$Interpolation$linear;
var terezka$line_charts$Internal$Junk$Config = function (a) {
	return {$: 'Config', a: a};
};
var elm$html$Html$p = _VirtualDom_node('p');
var terezka$line_charts$Internal$Junk$find = F2(
	function (hovered, data) {
		find:
		while (true) {
			if (!hovered.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var first = hovered.a;
				var rest = hovered.b;
				if (A2(
					elm$core$List$any,
					elm$core$Basics$eq(first),
					data)) {
					return elm$core$Maybe$Just(first);
				} else {
					var $temp$hovered = rest,
						$temp$data = data;
					hovered = $temp$hovered;
					data = $temp$data;
					continue find;
				}
			}
		}
	});
var terezka$line_charts$Internal$Junk$findSeries = F2(
	function (hovered, datas) {
		findSeries:
		while (true) {
			if (!datas.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var _n1 = datas.a;
				var color = _n1.a;
				var label = _n1.b;
				var data = _n1.c;
				var rest = datas.b;
				var _n2 = A2(
					terezka$line_charts$Internal$Junk$find,
					_List_fromArray(
						[hovered]),
					data);
				if (_n2.$ === 'Just') {
					var found = _n2.a;
					return elm$core$Maybe$Just(
						_Utils_Tuple3(color, label, data));
				} else {
					var $temp$hovered = hovered,
						$temp$datas = rest;
					hovered = $temp$hovered;
					datas = $temp$datas;
					continue findSeries;
				}
			}
		}
	});
var terezka$line_charts$Internal$Junk$shouldFlip = F2(
	function (system, x) {
		return _Utils_cmp(x - system.x.min, system.x.max - x) > 0;
	});
var terezka$line_charts$Internal$Junk$standardStyles = _List_fromArray(
	[
		_Utils_Tuple2('padding', '5px'),
		_Utils_Tuple2('min-width', '100px'),
		_Utils_Tuple2('background', 'rgba(255,255,255,0.8)'),
		_Utils_Tuple2('border', '1px solid #d3d3d3'),
		_Utils_Tuple2('border-radius', '5px'),
		_Utils_Tuple2('pointer-events', 'none')
	]);
var terezka$line_charts$Internal$Junk$hoverAt = F5(
	function (system, x, y, styles, view) {
		var yPercentage = (A2(terezka$line_charts$LineChart$Coordinate$toSvgY, system, y) * 100) / system.frame.size.height;
		var space = A2(terezka$line_charts$Internal$Junk$shouldFlip, system, x) ? (-15) : 15;
		var xPercentage = ((A2(terezka$line_charts$LineChart$Coordinate$toSvgX, system, x) + space) * 100) / system.frame.size.width;
		var posititonStyles = _List_fromArray(
			[
				_Utils_Tuple2(
				'left',
				elm$core$String$fromFloat(xPercentage) + '%'),
				_Utils_Tuple2(
				'top',
				elm$core$String$fromFloat(yPercentage) + '%'),
				_Utils_Tuple2('margin-right', '-400px'),
				_Utils_Tuple2('position', 'absolute'),
				A2(terezka$line_charts$Internal$Junk$shouldFlip, system, x) ? _Utils_Tuple2('transform', 'translateX(-100%)') : _Utils_Tuple2('transform', 'translateX(0)')
			]);
		var containerStyles = _Utils_ap(
			terezka$line_charts$Internal$Junk$standardStyles,
			_Utils_ap(posititonStyles, styles));
		return A2(
			elm$html$Html$div,
			A2(
				elm$core$List$map,
				function (_n0) {
					var p = _n0.a;
					var v = _n0.b;
					return A2(elm$html$Html$Attributes$style, p, v);
				},
				styles),
			view);
	});
var terezka$line_charts$Internal$Junk$middle = F2(
	function (r, system) {
		var range = r(system);
		return range.min + ((range.max - range.min) / 2);
	});
var terezka$line_charts$Internal$Junk$viewHeader = elm$html$Html$p(
	_List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'margin-top', '3px'),
			A2(elm$html$Html$Attributes$style, 'margin-bottom', '5px'),
			A2(elm$html$Html$Attributes$style, 'padding', '3px'),
			A2(elm$html$Html$Attributes$style, 'border-bottom', '1px solid rgb(163, 163, 163)')
		]));
var terezka$line_charts$Internal$Junk$viewRow = F3(
	function (color, label, value) {
		return A2(
			elm$html$Html$p,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'margin', '3px'),
					A2(elm$html$Html$Attributes$style, 'color', color)
				]),
			_List_fromArray(
				[
					elm$html$Html$text(label + (': ' + value))
				]));
	});
var terezka$line_charts$Internal$Junk$hoverOneHtml = F6(
	function (series, system, toX, toY, properties, hovered) {
		var y = A2(
			elm$core$Maybe$withDefault,
			A2(
				terezka$line_charts$Internal$Junk$middle,
				function ($) {
					return $.y;
				},
				system),
			toY(hovered));
		var x = A2(
			elm$core$Maybe$withDefault,
			A2(
				terezka$line_charts$Internal$Junk$middle,
				function ($) {
					return $.x;
				},
				system),
			toX(hovered));
		var viewValue = function (_n1) {
			var label = _n1.a;
			var value = _n1.b;
			return A3(
				terezka$line_charts$Internal$Junk$viewRow,
				'inherit',
				label,
				value(hovered));
		};
		var viewColorLabel = F2(
			function (color, label) {
				return A2(
					elm$html$Html$p,
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'margin', '0'),
							A2(elm$html$Html$Attributes$style, 'color', color)
						]),
					_List_fromArray(
						[
							elm$html$Html$text(label)
						]));
			});
		var viewHeaderOne = A2(
			terezka$line_charts$Internal$Utils$viewMaybe,
			A2(terezka$line_charts$Internal$Junk$findSeries, hovered, series),
			function (_n0) {
				var color = _n0.a;
				var label = _n0.b;
				return terezka$line_charts$Internal$Junk$viewHeader(
					_List_fromArray(
						[
							A2(
							viewColorLabel,
							avh4$elm_color$Color$toCssString(color),
							label)
						]));
			});
		return A5(
			terezka$line_charts$Internal$Junk$hoverAt,
			system,
			x,
			y,
			_List_Nil,
			A2(
				elm$core$List$cons,
				viewHeaderOne,
				A2(elm$core$List$map, viewValue, properties)));
	});
var terezka$line_charts$Internal$Junk$hoverOne = F2(
	function (hovered, properties) {
		return terezka$line_charts$Internal$Junk$Config(
			F4(
				function (series, toX, toY, system) {
					return {
						above: _List_Nil,
						below: _List_Nil,
						html: _List_fromArray(
							[
								A2(
								terezka$line_charts$Internal$Utils$viewMaybe,
								hovered,
								A5(terezka$line_charts$Internal$Junk$hoverOneHtml, series, system, toX, toY, properties))
							])
					};
				}));
	});
var terezka$line_charts$LineChart$Junk$hoverOne = terezka$line_charts$Internal$Junk$hoverOne;
var terezka$line_charts$Internal$Legends$Grouped = F2(
	function (a, b) {
		return {$: 'Grouped', a: a, b: b};
	});
var terezka$line_charts$Internal$Legends$defaultLegend = F2(
	function (index, _n0) {
		var sample = _n0.sample;
		var label = _n0.label;
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__legend'),
					terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A2(terezka$line_charts$Internal$Svg$offset, 20, index * 20)
						]))
				]),
			_List_fromArray(
				[
					sample,
					A2(
					elm$svg$Svg$g,
					_List_fromArray(
						[
							terezka$line_charts$Internal$Svg$transform(
							_List_fromArray(
								[
									A2(terezka$line_charts$Internal$Svg$offset, 40, 4)
								]))
						]),
					_List_fromArray(
						[
							A2(terezka$line_charts$Internal$Svg$label, 'inherit', label)
						]))
				]));
	});
var terezka$line_charts$Internal$Legends$defaultLegends = F8(
	function (toX, toY, offsetX, offsetY, hovered, _arguments, system, legends) {
		return A2(
			elm$svg$Svg$g,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$class('chart__legends'),
					terezka$line_charts$Internal$Svg$transform(
					_List_fromArray(
						[
							A3(
							terezka$line_charts$Internal$Svg$move,
							system,
							toX(system.x),
							toY(system.y)),
							A2(terezka$line_charts$Internal$Svg$offset, offsetX, offsetY)
						]))
				]),
			A2(elm$core$List$indexedMap, terezka$line_charts$Internal$Legends$defaultLegend, legends));
	});
var terezka$line_charts$Internal$Legends$hover = function (data) {
	return A2(
		terezka$line_charts$Internal$Legends$Grouped,
		30,
		A5(
			terezka$line_charts$Internal$Legends$defaultLegends,
			function ($) {
				return $.max;
			},
			function ($) {
				return $.max;
			},
			0,
			10,
			data));
};
var terezka$line_charts$Internal$Legends$default = terezka$line_charts$Internal$Legends$hover(_List_Nil);
var terezka$line_charts$LineChart$Legends$default = terezka$line_charts$Internal$Legends$default;
var terezka$line_charts$Internal$Line$Config = function (a) {
	return {$: 'Config', a: a};
};
var terezka$line_charts$Internal$Line$Style = function (a) {
	return {$: 'Style', a: a};
};
var terezka$line_charts$Internal$Line$style = F2(
	function (width, color_) {
		return terezka$line_charts$Internal$Line$Style(
			{color: color_, width: width});
	});
var terezka$line_charts$Internal$Line$default = terezka$line_charts$Internal$Line$Config(
	function (_n0) {
		return A2(terezka$line_charts$Internal$Line$style, 1, elm$core$Basics$identity);
	});
var terezka$line_charts$LineChart$Line$default = terezka$line_charts$Internal$Line$default;
var author$project$Chart$viewChart = F5(
	function (chart, toFloat, toString, msg, hovered) {
		return A2(
			terezka$line_charts$LineChart$viewCustom,
			{
				area: terezka$line_charts$LineChart$Area$default,
				container: A2(
					terezka$line_charts$LineChart$Container$styled,
					'line-chart-1',
					_List_fromArray(
						[
							_Utils_Tuple2('font-family', 'monospace')
						])),
				dots: terezka$line_charts$LineChart$Dots$hoverOne(hovered),
				events: terezka$line_charts$LineChart$Events$hoverOne(msg),
				grid: terezka$line_charts$LineChart$Grid$default,
				interpolation: terezka$line_charts$LineChart$Interpolation$default,
				intersection: terezka$line_charts$LineChart$Axis$Intersection$default,
				junk: A2(
					terezka$line_charts$LineChart$Junk$hoverOne,
					hovered,
					_List_fromArray(
						[
							_Utils_Tuple2(
							'Age',
							A2(
								elm$core$Basics$composeL,
								elm$core$Debug$toString,
								function ($) {
									return $.x;
								})),
							_Utils_Tuple2(
							'Weight',
							A2(
								elm$core$Basics$composeL,
								elm$core$Debug$toString,
								function ($) {
									return $.y;
								}))
						])),
				legends: terezka$line_charts$LineChart$Legends$default,
				line: terezka$line_charts$LineChart$Line$default,
				x: A2(author$project$Chart$xAxisConfig, toFloat, toString),
				y: A3(
					terezka$line_charts$LineChart$Axis$default,
					450,
					'Weight',
					function ($) {
						return $.y;
					})
			},
			A4(
				elm$core$List$map3,
				F3(
					function (c, color, name) {
						return A4(terezka$line_charts$LineChart$line, color, terezka$line_charts$LineChart$Dots$none, name, c.points);
					}),
				chart,
				author$project$Chart$colors,
				author$project$Chart$names));
	});
var author$project$Util$indexOf = F2(
	function (f, xs) {
		var helper = F2(
			function (list, n) {
				helper:
				while (true) {
					if (list.b) {
						var y = list.a;
						var ys = list.b;
						if (f(y)) {
							return elm$core$Maybe$Just(n);
						} else {
							var $temp$list = ys,
								$temp$n = n + 1;
							list = $temp$list;
							n = $temp$n;
							continue helper;
						}
					} else {
						return elm$core$Maybe$Nothing;
					}
				}
			});
		return A2(helper, xs, 0);
	});
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var author$project$Util$last = A2(
	elm$core$List$foldl,
	A2(elm$core$Basics$composeR, elm$core$Maybe$Just, elm$core$Basics$always),
	elm$core$Maybe$Nothing);
var author$project$Dataset$dateConverter = function (dataset) {
	var _n0 = author$project$Util$last(dataset.dimensions);
	if (_n0.$ === 'Just') {
		var dim = _n0.a;
		return function (s) {
			return A2(
				elm$core$Maybe$withDefault,
				1,
				A2(
					elm$core$Maybe$map,
					elm$core$Basics$toFloat,
					A2(
						author$project$Util$indexOf,
						function (x) {
							return _Utils_eq(x, s);
						},
						A2(
							elm$core$List$map,
							function (v) {
								return v.value;
							},
							dim.values))));
		};
	} else {
		return function (s) {
			return 1;
		};
	}
};
var author$project$Util$getAt = F2(
	function (i, list) {
		getAt:
		while (true) {
			if (list.b) {
				var x = list.a;
				var xs = list.b;
				if (!i) {
					return elm$core$Maybe$Just(x);
				} else {
					var n = i;
					var $temp$i = n - 1,
						$temp$list = xs;
					i = $temp$i;
					list = $temp$list;
					continue getAt;
				}
			} else {
				return elm$core$Maybe$Nothing;
			}
		}
	});
var author$project$Dataset$firstLongDimensionSize = function (dataset) {
	return A2(
		elm$core$Maybe$withDefault,
		1,
		A2(
			elm$core$Maybe$map,
			function (dim) {
				return elm$core$List$length(dim.values);
			},
			A2(
				elm$core$Maybe$andThen,
				function (idx) {
					return A2(author$project$Util$getAt, idx, dataset.dimensions);
				},
				A2(
					author$project$Util$indexOf,
					function (dim) {
						return elm$core$List$length(dim.values) > 1;
					},
					dataset.dimensions))));
};
var elm$core$List$takeReverse = F3(
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
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var elm_community$list_extra$List$Extra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2(elm$core$List$take, n, xs),
			A2(elm$core$List$drop, n, xs));
	});
var author$project$Util$splitEvery = F2(
	function (n, list) {
		var _n0 = A2(elm_community$list_extra$List$Extra$splitAt, n, list);
		var head = _n0.a;
		var tail = _n0.b;
		if (!tail.b) {
			return _List_fromArray(
				[head]);
		} else {
			var l = tail;
			return _Utils_ap(
				_List_fromArray(
					[head]),
				A2(author$project$Util$splitEvery, n, l));
		}
	});
var author$project$Dataset$splitToCharts = F2(
	function (n, lines) {
		return A2(author$project$Util$splitEvery, n, lines);
	});
var author$project$Dataset$Point = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return elm$core$Result$Ok(v);
		} else {
			return elm$core$Result$Err(err);
		}
	});
var author$project$Dataset$splitToLines = function (dataset) {
	return A2(
		elm$core$Result$map,
		function (lastDim) {
			return A2(
				elm$core$List$map,
				function (values) {
					return {
						points: A3(
							elm$core$List$map2,
							author$project$Dataset$Point,
							A2(
								elm$core$List$map,
								function (x) {
									return x.value;
								},
								lastDim.values),
							values)
					};
				},
				A2(
					author$project$Util$splitEvery,
					elm$core$List$length(lastDim.values),
					dataset.values));
		},
		A2(
			elm$core$Result$fromMaybe,
			'No last dim!',
			author$project$Util$last(dataset.dimensions)));
};
var author$project$Util$indexesOf = F2(
	function (f, list) {
		var helper = F2(
			function (xs, n) {
				var _n0 = A2(author$project$Util$indexOf, f, xs);
				if (_n0.$ === 'Just') {
					var idx = _n0.a;
					return _Utils_ap(
						_List_fromArray(
							[n + idx]),
						A2(
							helper,
							A2(elm$core$List$drop, idx + 1, xs),
							(n + idx) + 1));
				} else {
					return _List_Nil;
				}
			});
		return A2(helper, list, 0);
	});
var author$project$Dataset$makeCharts = function (dataset) {
	var firstSize = author$project$Dataset$firstLongDimensionSize(dataset);
	var dimsNotOne = A2(
		author$project$Util$indexesOf,
		function (dim) {
			return elm$core$List$length(dim.values) !== 1;
		},
		dataset.dimensions);
	var _n0 = elm$core$List$length(dimsNotOne);
	switch (_n0) {
		case 2:
			return A2(
				elm$core$Result$map,
				function (lines) {
					return A2(author$project$Dataset$splitToCharts, firstSize, lines);
				},
				author$project$Dataset$splitToLines(dataset));
		case 1:
			return A2(
				elm$core$Result$map,
				function (lines) {
					return A2(author$project$Dataset$splitToCharts, 1, lines);
				},
				author$project$Dataset$splitToLines(dataset));
		default:
			return elm$core$Result$Err('Wrong size of dataset');
	}
};
var author$project$Chart$viewDataset = F3(
	function (dataset, msg, hovered) {
		var toString = function (x) {
			return 'string';
		};
		var toFloat = author$project$Dataset$dateConverter(dataset);
		var rcharts = author$project$Dataset$makeCharts(dataset);
		if (rcharts.$ === 'Ok') {
			var charts = rcharts.a;
			return A5(
				author$project$Chart$viewChart,
				A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					elm$core$List$head(charts)),
				toFloat,
				toString,
				msg,
				hovered);
		} else {
			var e = rcharts.a;
			return elm$html$Html$text(e);
		}
	});
var author$project$Main$viewChart = F2(
	function (model, msg) {
		var _n0 = model.dataset;
		if (_n0.$ === 'Just') {
			var d = _n0.a;
			return A3(author$project$Chart$viewDataset, d, msg, model.hovered);
		} else {
			return elm$html$Html$text('');
		}
	});
var author$project$Main$ShowTree = {$: 'ShowTree'};
var author$project$Main$DGetConfig = function (a) {
	return {$: 'DGetConfig', a: a};
};
var author$project$Main$DSetConfig = function (a) {
	return {$: 'DSetConfig', a: a};
};
var author$project$Main$leafOnClick = F2(
	function (leaf, oldConfig) {
		var _n0 = leaf.config;
		if (_n0.$ === 'Just') {
			var config = _n0.a;
			return _Utils_eq(
				oldConfig,
				elm$core$Maybe$Just(config)) ? author$project$Main$DatasetMessage(
				author$project$Main$DSetConfig(elm$core$Maybe$Nothing)) : author$project$Main$DatasetMessage(
				author$project$Main$DSetConfig(
					elm$core$Maybe$Just(config)));
		} else {
			return author$project$Main$DatasetMessage(
				author$project$Main$DGetConfig(leaf.id));
		}
	});
var author$project$Main$GetSubTree = function (a) {
	return {$: 'GetSubTree', a: a};
};
var author$project$Main$Hide = function (a) {
	return {$: 'Hide', a: a};
};
var author$project$Main$Show = function (a) {
	return {$: 'Show', a: a};
};
var author$project$Main$treeListOnClick = F2(
	function (state, list) {
		return (!elm$core$List$length(list)) ? author$project$Main$GetSubTree(state.id) : (state.isHidden ? author$project$Main$Show(state.id) : author$project$Main$Hide(state.id));
	});
var elm$html$Html$li = _VirtualDom_node('li');
var elm$html$Html$ul = _VirtualDom_node('ul');
var author$project$Main$treeHtml = F2(
	function (oldConfig, tree) {
		if (tree.$ === 'Category') {
			var state = tree.a;
			var list = tree.b;
			return A2(
				elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Events$onClick(
								A2(author$project$Main$treeListOnClick, state, list))
							]),
						_List_fromArray(
							[
								elm$html$Html$text(state.text)
							])),
						A2(
						elm$html$Html$ul,
						_List_Nil,
						state.isHidden ? _List_Nil : A2(
							elm$core$List$map,
							author$project$Main$treeHtml(oldConfig),
							list))
					]));
		} else {
			var leaf = tree.a;
			return A2(
				elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Events$onClick(
								A2(author$project$Main$leafOnClick, leaf, oldConfig))
							]),
						_List_fromArray(
							[
								elm$html$Html$text(leaf.text)
							]))
					]));
		}
	});
var author$project$Main$viewTree = function (model) {
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				elm$html$Html$text(
				A2(elm$core$Maybe$withDefault, '', model.errorMsg)),
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(author$project$Main$ShowTree),
						A2(elm$html$Html$Attributes$style, 'display', 'block')
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Choose Dataset')
					])),
				model.showTree ? A2(
				elm$html$Html$ul,
				_List_Nil,
				A2(
					elm$core$List$map,
					author$project$Main$treeHtml(model.datasetConfig),
					model.trees)) : elm$html$Html$text('')
			]));
};
var elm$html$Html$h2 = _VirtualDom_node('h2');
var author$project$Main$view = function (model) {
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text('SSB Datasets')
					])),
				author$project$Main$viewTree(model),
				author$project$Main$configHtml(model.datasetConfig),
				A2(author$project$Main$viewChart, model, author$project$Main$Hover)
			]));
};
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$element = _Browser_element;
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Main$main = elm$browser$Browser$element(
	{
		init: author$project$Main$init,
		subscriptions: elm$core$Basics$always(elm$core$Platform$Sub$none),
		update: author$project$Main$update,
		view: author$project$Main$view
	});
_Platform_export({'Main':{'init':author$project$Main$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));