enum Const {
	CInt( v : Int );
	CFloat( f : Float );
	CString( s : String );
}

enum Op {
    OAdd;
    OSubstract;
    OMult;
    ODivide;
}

enum Token {
    TOp(op:Op);
    TConst(c:Const);
    TEof;
}

enum Expr {
    EConst( c : Const );
    EBlock( e : Array<Expr> );
    EBinop( op : Op, e1 : Expr, e2 : Expr );
    ETernary( cond : Expr, e1 : Expr, e2 : Expr );
}

enum EError {
    EInvalidChar( c : String );
    EUnexpected( s : String );
}

class Error {
	public var e : EError;
	public var origin : String;
	public var line : Int;
	public function new(e, origin, line) {
		this.e = e;
		this.origin = origin;
		this.line = line;
	}

	public function toString(): String {
		return '$e, from $origin at $line';
	}
}

class Main
{
    public static function main() 
    {
        new Main();
    }

    var aExprs:Array<Expr>;
    var aTokens:Array<Token>;
    var idToken:Int;
    var input:String;

    function new()
    {
        this.idToken = 0;
        this.aTokens = [];
        
        var input = "27 * 35 + 7 ";
        trace("Evaluating : "+input);
        this.aExprs = parseStream(input);

        if( aExprs.length > 0 )
        {
            trace("Parsed, ready for evaluation");
            trace(aExprs);

            for( e in aExprs ) 
                trace('Eval '+evaluate(e));
        }
    }

    function evaluate( e: Expr ):Dynamic
    {
        switch( e ) 
        {
            case EConst(c) :
                return switch(c) 
                {
                    case CString(s): s;
                    case CInt(i): i;
                    case CFloat(f): f;
                }
                
            case EBlock( e ):
                var v = null;
                for( expr in e )
                {
                    v = evaluate(expr);
                }
                return v;
            case EBinop( op, e1, e2 ):
                var f = switch(op) {
                    case Op.OAdd : function(e1, e2) { return evaluate(e1) + evaluate(e2); };
                    case Op.ODivide : function(e1, e2) { return evaluate(e1) / evaluate(e2); };
                    case Op.OMult : function(e1, e2) { return evaluate(e1) * evaluate(e2); };
                    case Op.OSubstract : function(e1, e2) { return evaluate(e1) - evaluate(e2); };
                }
                
                return f(e1, e2);

            case ETernary( cond, e1, e2  ): 
                throw "unsupported";
        }
    }

    function parseStream(stream:String)
    {
        this.input = stream;
        parseString(stream);
        trace(aTokens);
        var exprs = [];
        try {
            var e = parseExpr();
            if( e != null ) exprs.push(e);
            else trace("expression was null");
            trace(exprs);
        } catch( e : Error ) {
            trace("Erreur "+e);
        }
        return exprs;
    }

    function parseExpr():Null<Expr>
    {
        var t = token();
        //no line terminal
        if( t == null || t == TEof ) return null;
        switch(t)
        {
            case TEof: return null;
            case TConst(c):
                return parseNextExpr(EConst(c));
            default : 
                error(EUnexpected('parseExpr::Invalid synthax - unrecognized grammar at ${aTokens[idToken]}'));
                return null;
        }
    }

    function parseNextExpr(e:Expr):Null<Expr>
    {
        var t = token();
        if( t == null  || t == TEof)  return e;
        //
        switch(t)
        {
            case TOp(op): 
                return makeOp(op, e, parseExpr());
            default : 
                error(EUnexpected('parseNextExpr::Invalid synthax - unrecognized grammar at ${aTokens[idToken]}'));
                return null;
        }
    }

    function makeOp(op:Op, e1:Expr, e2:Expr):Expr
    {
        //en fonction de ce qu'aura généré le parsing de la partie droite...
        return switch(e2)
        {
            case EConst(c): 
                EBinop(op, e1, e2);
            case EBinop(op2, e3, e4):
                //check operator priority comes here...
                if( checkPriority(op, op2) ) {
                    EBinop(op2, makeOp(op, e1, e3), e4);
                } else {
                    EBinop(op, e1, e2);                    
                }
                
            default :
                error(EUnexpected('makeOp::Invalid synthax - unrecognized grammar $e2'));
                null;
        }
    }

    function checkPriority(op1:Op, op2:Op):Bool
    {
        return switch([op1, op2])
        {
            case [Op.ODivide, _] : true;
            case [_, Op.ODivide] : false;
            case [_, Op.OMult] : false;
            default : true;
        }
    }

    function error(expr:EError) 
    {
        throw new Error(expr, this.input, 0);
        return;
    }

    function addExpr(e:Expr):Expr
    {
        this.aExprs.push(e);
        return e;
    }

    function token()
    {
        return aTokens[idToken++];
    }

    function readToken()
    {
        return aTokens[idToken];
    }

    function parseString(stringExpr:String):Array<Token>
    {
        var tokens = [];
        var i = 0;
        var char = "";
        var token = "";
        var waitMode = true;

        while(true)
        {
            try char = stringExpr.charAt(i) catch(e:Dynamic) { trace("end parsing"); break; }
            trace(char);
            switch(char)
            {
                case "": 
                    if( !waitMode ) tokens.push( tokenize(token) );
                    tokens.push(TEof);
                    break;
                case " ":
                    if( !waitMode ) tokens.push( tokenize(token) );
                    waitMode = true;
                    token = "";
                default:
                    token += char;
                    waitMode = false;
            }

            i++;
        }
        this.aTokens = tokens.copy();
        return tokens;
    }

    function tokenize(token:String):Token
    {
        return switch(token)
        {
            case "+": TOp(OAdd);
            case "-": TOp(OSubstract);
            case "*": TOp(OMult);
            case "/": TOp(ODivide);
            default:
                if( Std.parseFloat(token) != Math.NaN ) TConst(CFloat(Std.parseFloat(token)));
                else if( Std.parseInt(token) != null ) TConst(CInt(Std.parseInt(token)));
                else throw 'invalid token $token';
        }
    }
}
