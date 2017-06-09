export interface Child<TParent> {
    parent: TParent | undefined;
}

export interface HasName {
    name: string;
}

export abstract class DeclarationBase {
    jsDocComment?: string;
    comment?: string;
    flags?: DeclarationFlags;
}

export interface EnumMemberDeclaration extends DeclarationBase, HasName {
    kind: "enum-value";
}

export interface EnumDeclaration extends DeclarationBase, HasName {
    kind: "enum";
    members: EnumMemberDeclaration[];
    constant: boolean;
}

export interface PropertyDeclaration extends DeclarationBase, HasName {
    kind: "property";
    type: Type;
}

export class Parameter implements HasName {
    constructor(public name: string,
                public type: Type) { }

    kind: "parameter" = "parameter";

    // region optional

    private _optional: boolean;
    get optional(): boolean {
        return this._optional;
    }

    set optional(value: boolean) {
        this._optional = value;
    }

    // endregion

    rest: boolean;
}

export interface TypeParameter {
    kind: "type-parameter";
    name: string;
    baseType?: ObjectTypeReference | TypeParameter;
    default?: Type;
}

export interface IndexSignature {
    kind: "index-signature";
    name: string;
    indexType: ("string" | "number");
    valueType: Type;
}

export interface CallSignature extends DeclarationBase, GenericType {
    kind: "call-signature";
    parameters: Parameter[];
    returnType: ReturnType;
}

export interface MethodDeclaration extends DeclarationBase, HasName, GenericType {
    kind: "method";
    parameters: Parameter[];
    returnType: ReturnType;
}

export interface FunctionDeclaration extends DeclarationBase, HasName, GenericType, Child<NamespaceDeclaration> {
    kind: "function";
    parameters: Parameter[];
    returnType: ReturnType;
}

export interface ConstructorDeclaration extends DeclarationBase {
    kind: "constructor";
    parameters: Parameter[];
}

export interface ClassDeclaration extends DeclarationBase, HasName, GenericType, Child<NamespaceDeclaration> {
    kind: "class";
    members: ClassMember[];
    implements: (InterfaceDeclaration | NamedTypeReference)[];
    baseType?: ObjectTypeReference;
}

export interface InterfaceDeclaration extends DeclarationBase, HasName, GenericType, Child<NamespaceDeclaration> {
    kind: "interface";
    members: ObjectTypeMember[];
    baseTypes: ObjectTypeReference[];
}

export interface ImportAllDeclaration extends DeclarationBase {
    kind: "importAll";
    name: string;
    from: string;
}

export interface ImportNamedDeclaration extends DeclarationBase {
    kind: "importNamed";
    name: string;
    as?: string;
    from: string;
}

export interface ImportDefaultDeclaration extends DeclarationBase {
    kind: "importDefault";
    name: string;
    from: string;
}

export class NamespaceDeclaration extends DeclarationBase implements HasName, Child<NamespaceDeclaration> {
    constructor(public readonly name: string) {
        super();

        const oldPush = this.members.push.bind(this.members);
        const self = this;
        this.members.push = function(...items: NamespaceMember[]) {
            for (const item of items) {
                item.parent = self;
            }

            return oldPush(...items);
        };
    }

    readonly kind: "namespace" = "namespace";
    readonly members: NamespaceMember[] = [];
    parent: NamespaceDeclaration;
}

export class GlobalDeclaration {
    readonly kind: "global" = "global";
    readonly members: NamespaceMember[] = [];
}

export interface ConstDeclaration extends DeclarationBase, HasName, Child<NamespaceDeclaration> {
    kind: "const";
    type: Type;
}

export interface ExportEqualsDeclaration extends DeclarationBase {
    kind: "export=";
    target: string;
}

export interface ModuleDeclaration extends DeclarationBase {
    kind: "module";
    name: string;
    members: ModuleMember[];
}

export interface ObjectType {
    kind: "object";
    members: ObjectTypeMember[];
}

export interface UnionType {
    kind: "union";
    members: Type[];
}

export interface IntersectionType {
    kind: "intersection";
    members: Type[];
}

export interface FunctionType extends GenericType {
    kind: "function-type";
    parameters: Parameter[];
    returnType: ReturnType;
}

export interface TypeAliasDeclaration extends DeclarationBase, HasName, GenericType, Child<NamespaceDeclaration> {
    kind: "alias";
    type: Type;
}

export interface NamedTypeReference extends HasName, GenericType, Child<NamespaceDeclaration> {
    kind: "name";
}

export interface TypeofReference {
    kind: "typeof";
    type: TypeReference;
}

export interface TypePredicate {
    kind: "type-predicate";
    parameter: Parameter;
    type: Type;
}

export interface StringLiteral {
    kind: "string-literal";
    value: string;
}

export interface NumberLiteral {
    kind: "number-literal";
    value: number;
}

export interface GenericType {
    kind: string;
    typeParameters: TypeParameter[];
}

export class GenericTypeReference {
    constructor(public type: GenericType) { }

    kind: "generic-type-reference" = "generic-type-reference";
    typeArguments: TypeReference[] = [];
}

export type PrimitiveType = "string" | "number" | "boolean" | "any" | "void" | "object" | "null" | "undefined" | "true" | "false" | StringLiteral | NumberLiteral;

export type ThisType = "this";

export type TypeReference = TopLevelDeclaration | NamedTypeReference | GenericTypeReference | PrimitiveType | ThisType | UnionType;

export type ObjectTypeReference = ClassDeclaration | InterfaceDeclaration | NamedTypeReference | GenericTypeReference;
export type ObjectTypeMember = PropertyDeclaration | MethodDeclaration | IndexSignature | CallSignature;
export type ClassMember = PropertyDeclaration | MethodDeclaration | IndexSignature | ConstructorDeclaration;

export type Type = TypeReference | UnionType | IntersectionType | ObjectType | TypeofReference | FunctionType | TypeParameter | ThisType;
export type ReturnType = Type | TypePredicate;

export type Import = ImportAllDeclaration | ImportDefaultDeclaration | ImportNamedDeclaration;

export type NamespaceMember = InterfaceDeclaration | TypeAliasDeclaration | ClassDeclaration | NamespaceDeclaration | ConstDeclaration | FunctionDeclaration;
export type ModuleMember = InterfaceDeclaration | TypeAliasDeclaration | ClassDeclaration | NamespaceDeclaration | ConstDeclaration | FunctionDeclaration | Import;
export type TopLevelDeclaration = NamespaceMember | ExportEqualsDeclaration | ModuleDeclaration | EnumDeclaration | Import | GlobalDeclaration;

type _GenericType = any;
type _NamedType = _GenericType | any;
type _TypeParameter = any;
type _TypeReference = _NamedType | _TypeParameter;

export enum DeclarationFlags {
    None = 0,
    Private = 1 << 0,
    Protected = 1 << 1,
    Static = 1 << 2,
    Optional = 1 << 3,
    Export = 1 << 4,
    Abstract = 1 << 5,
    ExportDefault = 1 << 6,
    ReadOnly = 1 << 7,
}

export enum ParameterFlags {
    None = 0,
    Optional = 1 << 0,
    Rest = 1 << 1
}

export const config = {
    wrapJsDocComments: true,
    outputEol: "\r\n",
    chop: {
        methods: Number.MAX_SAFE_INTEGER,
    },
};

export const create = {
    interface(name: string): InterfaceDeclaration {
        return {
            name,
            baseTypes: [],
            kind: "interface",
            members: [],
            parent: undefined,
            typeParameters: [],
        };
    },

    class(name: string): ClassDeclaration {
        return {
            kind: "class",
            name,
            members: [],
            implements: [],
            typeParameters: [],
            parent: undefined,
        };
    },

    typeParameter(name: string, baseType?: ObjectTypeReference | TypeParameter): TypeParameter {
        return {
            kind: "type-parameter",
            name, baseType,
        };
    },

    enum(name: string, constant: boolean = false): EnumDeclaration {
        return {
            kind: "enum",
            name, constant,
            members: [],
        };
    },

    enumValue(name: string): EnumMemberDeclaration {
        return {
            kind: "enum-value",
            name,
        };
    },

    property(name: string, type: Type, flags = DeclarationFlags.None): PropertyDeclaration {
        return {
            kind: "property",
            name, type, flags,
        };
    },

    method(name: string, parameters: Parameter[], returnType: ReturnType, flags = DeclarationFlags.None): MethodDeclaration {
        return {
            kind: "method",
            typeParameters: [],
            name, parameters, returnType, flags,
        };
    },

    callSignature(parameters: Parameter[], returnType: ReturnType): CallSignature {
        return {
            kind: "call-signature",
            typeParameters: [],
            parameters, returnType,
        };
    },

    function(name: string, parameters: Parameter[], returnType: ReturnType): FunctionDeclaration {
        return {
            kind: "function",
            typeParameters: [],
            name, parameters, returnType,
            parent: undefined,
        };
    },

    functionType(parameters: Parameter[], returnType: ReturnType): FunctionType {
        return {
            kind: "function-type",
            parameters, returnType,
            typeParameters: [],
        };
    },

    parameter(name: string, type: Type): Parameter {
        return new Parameter(name, type);
    },

    constructor(parameters: Parameter[], flags = DeclarationFlags.None): ConstructorDeclaration {
        return {
            kind: "constructor",
            parameters,
            flags,
        };
    },

    const(name: string, type: Type): ConstDeclaration {
        return {
            kind: "const", name, type,
            parent: undefined,
        };
    },

    alias(name: string, type: Type): TypeAliasDeclaration {
        return {
            kind: "alias", name, type,
            typeParameters: [],
            parent: undefined,
        };
    },

    namespace(name: string): NamespaceDeclaration {
        return new NamespaceDeclaration(name);
    },

    global(): GlobalDeclaration {
        return new GlobalDeclaration();
    },

    objectType(members: ObjectTypeMember[]): ObjectType {
        return {
            kind: "object",
            members,
        };
    },

    indexSignature(name: string, indexType: ("string" | "number"), valueType: Type): IndexSignature {
        return {
            kind: "index-signature",
            name, indexType, valueType,
        };
    },

    array(type: TypeReference): GenericTypeReference {
        return {
            kind: "generic-type-reference",
            type: create.namedTypeReference("Array"),
            typeArguments: [type],
        };
    },

    namedTypeReference(name: string): NamedTypeReference {
        return {
            kind: "name",
            name,
            typeParameters: [],
            parent: undefined,
        };
    },

    exportEquals(target: string): ExportEqualsDeclaration {
        return {
            kind: "export=",
            target,
        };
    },

    module(name: string): ModuleDeclaration {
        return {
            kind: "module",
            name,
            members: [],
        };
    },

    importAll(name: string, from: string): ImportAllDeclaration {
        return {
            kind: "importAll",
            name,
            from,
        };
    },

    importDefault(name: string, from: string): ImportDefaultDeclaration {
        return {
            kind: "importDefault",
            name,
            from,
        };
    },

    importNamed(name: string, as: string, from?: string): ImportNamedDeclaration {
        return {
            kind: "importNamed",
            name,
            as: typeof from !== "undefined" ? as : undefined,
            from: typeof from !== "undefined" ? from : as,
        };
    },

    union(members: Type[]): UnionType {
        return {
            kind: "union",
            members,
        };
    },

    intersection(members?: Type[]): IntersectionType {
        return {
            kind: "intersection",
            members: members || [],
        };
    },

    typeof(type: NamedTypeReference): TypeofReference {
        return {
            kind: "typeof",
            type,
        };
    },

    typePredicate(parameter: Parameter, type: Type): TypePredicate {
        return {
            kind: "type-predicate",
            parameter, type,
        };
    },

    genericTypeReference(type: GenericType): GenericTypeReference {
        return new GenericTypeReference(type);
    },

    typeReference(type: Type): TypeReference {
        if (typeof type === "string") { return type; }
        switch (type.kind) {
            case "export=":
            case "global":
            case "union":
            case "intersection":
            case "object":
            case "typeof":
            case "function-type":
                throw new Error();

            case "generic-type-reference":
                return type;

            case "string-literal":
            case "number-literal":
                return type;
        }
        return create.namedTypeReference(type.name);
    },
};

export const type = {
    stringLiteral(string: string): PrimitiveType {
        return {
            kind: "string-literal",
            value: string,
        };
    },
    numberLiteral(number: number): PrimitiveType {
        return {
            kind: "number-literal",
            value: number,
        };
    },
    string: <PrimitiveType>"string",
    number: <PrimitiveType>"number",
    boolean: <PrimitiveType>"boolean",
    any: <PrimitiveType>"any",
    void: <PrimitiveType>"void",
    object: <PrimitiveType>"object",
    null: <PrimitiveType>"null",
    undefined: <PrimitiveType>"undefined",
    true: <PrimitiveType>"true",
    false: <PrimitiveType>"false",
    this: <ThisType>"this",
    is: {
        union(t: Type): t is UnionType {
            if (typeof t !== "string") { return t.kind === "union"; }
            return false;
        },
        functionType(t: Type): t is FunctionType {
            if (typeof t !== "string") { return t.kind === "function-type"; }
            return false;
        },
        array(t: GenericType): boolean {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "name":
                        const n = t as NamedTypeReference;
                        return n.name === "Array";
                }
            }
            return false;
        },
        genericTypeReference(t: Type): t is GenericTypeReference {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "generic-type-reference":
                        return true;
                }
            }
            return false;
        },
        namedTypeReference(t: Type): t is NamedTypeReference {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "name":
                        return true;
                }
            }
            return false;
        },
        primitiveType(t: Type): t is PrimitiveType {
            if (typeof t === "string") {
                switch (t) {
                    case "string":
                    case "number":
                    case "boolean":
                    case "any":
                    case "void":
                    case "object":
                    case "null":
                    case "undefined":
                    case "true":
                    case "false":
                        return true;
                }
            } else {
                switch (t.kind) {
                    case "string-literal":
                    case "number-literal":
                        return true;
                }
            }
            return false;
        },
        thisType(t: Type): t is ThisType {
            if (typeof t === "string") {
                switch (t) {
                    case "this":
                        return true;
                }
            }
            return false;
        },
        typeReference(t: Type): t is TypeReference {
            if (type.is.topLevelDeclaration(t)) { return true; }
            if (type.is.namedTypeReference(t)) { return true; }
            if (type.is.genericTypeReference(t)) { return true; }
            if (type.is.primitiveType(t)) { return true; }
            return false;
        },
        objectTypeReference(t: Type): t is ObjectTypeReference {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "class":
                    case "interface":
                        return true;
                }
            }
            return false;
        },
        objectType(t: Type): t is ObjectType {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "object":
                    case "interface":
                    case "class":
                        return true;
                }
            }
            return false;
        },
        import(t: Type): t is Import {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "importAll":
                    case "importNamed":
                    case "importDefault":
                        return true;
                }
            }
            return false;
        },
        namespaceMember(t: Type): t is NamespaceMember {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "interface":
                    case "alias":
                    case "class":
                    case "namespace":
                    case "const":
                    case "function":
                        return true;
                }
            }
            return false;
        },
        topLevelDeclaration(t: Type): t is TopLevelDeclaration {
            if (typeof t !== "string") {
                switch (t.kind) {
                    case "export=":
                    case "module":
                    case "enum":
                        return true;
                }
                if (type.is.namespaceMember(t)) { return true; }
                if (type.is.import(t)) { return true; }
            }
            return false;
        },
        staticClassMember(m: ClassMember) {
            switch (m.kind) {
                case "property":
                case "method":
                case "constructor":
                    return true;
            }
            return false;
        },
    },
};

export const reservedWords = ["abstract", "await", "boolean", "break", "byte", "case",
    "catch", "char", "class", "const", "continue", "debugger", "default",
    "delete", "do", "double", "else", "enum", "export", "extends", "false",
    "final", "finally", "float", "for", "function", "goto", "if", "implements",
    "import", "in", "instanceof", "int", "interface", "let", "long", "native",
    "new", "null", "package", "private", "protected", "public", "return", "short",
    "static", "super", "switch", "synchronized", "this", "throw", "throws",
    "transient", "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield"];

/** IdentifierName can be written as unquoted property names, but may be reserved words. */
export function isIdentifierName(s: string) {
    return /^[$A-Z_][0-9A-Z_$]*$/i.test(s);
}

/** Identifiers are e.g. legal variable names. They may not be reserved words */
export function isIdentifier(s: string) {
    return isIdentifierName(s) && reservedWords.indexOf(s) < 0;
}

function quoteIfNeeded(s: string) {
    if (isIdentifierName(s)) {
        return s;
    } else {
        // JSON.stringify handles escaping quotes for us. Handy!
        return JSON.stringify(s);
    }
}

export enum ContextFlags {
    None = 0,
    Module = 1 << 0,
    InAmbientNamespace = 1 << 1
}

export function never(x: never, err: string): never {
    throw new Error(err);
}

export function hasFlag<T extends number>(haystack: T | undefined, needle: T): boolean;
export function hasFlag(haystack: number | undefined, needle: number) {
    if (haystack === undefined) {
        return false;
    }
    return !!(needle & haystack);
}

export function emit(rootDecl: TopLevelDeclaration, rootFlags = ContextFlags.None): string {
    let output = "";
    let indentLevel = 0;
    let contextStack: ContextFlags[] = [rootFlags];

    writeDeclaration(rootDecl);
    return output;

    function getContextFlags() {
        return contextStack.reduce((a, b) => a | b, ContextFlags.None);
    }

    function tab() {
        for (let i = 0; i < indentLevel; i++) {
            output = output + "    ";
        }
    }

    function print(s: string) {
        output = output + s;
    }

    function getLastLineLength() {
        return output.length - output.lastIndexOf("\n");
    }

    function start(s: string) {
        tab();
        print(s);
    }

    function classFlagsToString(flags: DeclarationFlags | undefined = DeclarationFlags.None): string {
        let out = "";

        if (flags && flags & DeclarationFlags.Abstract) {
            out += "abstract ";
        }

        return out;
    }

    function memberFlagsToString(flags: DeclarationFlags | undefined = DeclarationFlags.None): string {
        let out = "";

        if (flags & DeclarationFlags.Private) {
            out += "private ";
        }
        else if (flags & DeclarationFlags.Protected) {
            out += "protected ";
        }

        if (flags & DeclarationFlags.Static) {
            out += "static ";
        }

        if (flags & DeclarationFlags.Abstract) {
            out += "abstract ";
        }

        if (flags & DeclarationFlags.ReadOnly) {
            out += "readonly ";
        }

        return out;
    }

    function startWithDeclareOrExport(s: string, flags: DeclarationFlags | undefined = DeclarationFlags.None) {
        if ((getContextFlags() & ContextFlags.InAmbientNamespace) ||
            s.indexOf("interface") === 0 ||
            s.indexOf("type") === 0) {
            // Already in an all-export context
            start(s);
        }
        else if (flags & DeclarationFlags.Export) {
            start(`export ${s}`);
        } else if (flags & DeclarationFlags.ExportDefault) {
            start(`export default ${s}`);
        } else {
            start(`declare ${s}`);
        }
    }

    function newline() {
        output = output + config.outputEol;
    }

    function needsParens(d: Type) {
        if (typeof d !== "string") {
            switch (d.kind) {
                case "generic-type-reference":
                    return type.is.array(d.type);
                case "alias":
                case "interface":
                case "class":
                case "union":
                case "function-type":
                    return true;
            }
        }
        return false;
    }

    function printDeclarationComments(decl: DeclarationBase) {
        if (decl.comment) {
            start(`// ${decl.comment}`);
            newline();
        }
        if (decl.jsDocComment) {
            if (config.wrapJsDocComments) {
                start("/**");
                newline();
                for (const line of decl.jsDocComment.split(/\r?\n/g)) {
                    start(` *`);
                    if (line.length) { print(` ${line}`); }
                    newline();
                }
                start(" */");
            }
            else {
                start(decl.jsDocComment);
            }

            newline();
        }
    }

    function printObjectTypeMembers(members: ObjectTypeMember[]) {
        print("{");
        if (members.length) {
            newline();
            indentLevel++;
            for (const member of members) {
                printMember(member);
            }
            indentLevel--;
            tab();
        } else {
            print("");
        }
        print("}");

        function printMember(member: ObjectTypeMember) {
            switch (member.kind) {
                case "index-signature":
                    printDeclarationComments(member);
                    tab();
                    print(`[${member.name}: `);
                    writeReference(member.indexType);
                    print("]: ");
                    writeReference(member.valueType);
                    print(";");
                    newline();
                    return;
                case "call-signature": {
                    printDeclarationComments(member);
                    tab();
                    writeTypeParameters(member.typeParameters);
                    print("(");
                    let first = true;
                    for (const param of member.parameters) {
                        if (!first) print(", ");
                        first = false;
                        print(param.name);
                        if (param.optional) print("?");
                        print(": ");
                        writeReference(param.type);
                    }
                    print("): ");
                    writeReference(member.returnType);
                    print(";");
                    newline();
                    return;
                }
                case "method":
                    writeMethodDeclaration(member);
                    return;
                case "property":
                    printDeclarationComments(member);
                    tab();
                    if (hasFlag(member.flags, DeclarationFlags.ReadOnly)) print("readonly ");
                    print(quoteIfNeeded(member.name));
                    if (hasFlag(member.flags, DeclarationFlags.Optional)) print("?");
                    print(": ");
                    writeReference(member.type);
                    print(";");
                    newline();
                    return;
            }
            never(member, `Unknown member kind ${(member as ObjectTypeMember).kind}`);
        }
    }

    function writeReference(d: ReturnType) {
        if (typeof d === "string") {
            print(d);
        } else {
            const e = d;
            switch (e.kind) {
                case "name":
                    print(e.name);
                    break;

                case "type-parameter":
                case "class":
                case "interface":
                case "alias":
                    let f: Type | undefined = e;
                    const names = [];
                    while (f) {
                        if (type.is.namespaceMember(f)) {
                            names.unshift(f.name);
                            f = f.parent;
                        } else {
                            f = undefined;
                        }
                    }
                    print(names.join("."));
                    break;

                case "generic-type-reference":
                    const typeArgument = e.typeArguments[0] || type.any;
                    if (type.is.array(e.type) && !type.is.union(typeArgument) && !type.is.functionType(typeArgument)) {
                        if (needsParens(typeArgument)) print("(");
                        writeReference(typeArgument);
                        if (needsParens(typeArgument)) print(")");
                        print("[]");
                    } else {
                        const g = e.type as Type;
                        writeReference(g);
                        if (e.typeArguments.length > 0) {
                            print("<");
                            writeDelimited(e.typeArguments, ", ", writeReference);
                            print(">");
                        }
                    }
                    break;

                case "object":
                    printObjectTypeMembers(e.members);
                    break;

                case "string-literal":
                    print(JSON.stringify(e.value));
                    break;

                case "number-literal":
                    if (isNaN(e.value)) print("typeof NaN");
                    else if (!isFinite(e.value)) print("typeof Infinity");
                    else print(e.value.toString());
                    break;

                case "function-type":
                    writeFunctionType(e);
                    break;

                case "union":
                    writeDelimited(e.members, " | ", x => {
                        if (type.is.functionType(x)) { print("("); }
                        writeReference(x);
                        if (type.is.functionType(x)) { print(")"); }
                    });
                    break;

                case "intersection":
                    writeDelimited(e.members, " & ", x => {
                        if (type.is.functionType(x)) { print("("); }
                        writeReference(x);
                        if (type.is.functionType(x)) { print(")"); }
                    });
                    break;

                case "typeof":
                    print("typeof ");
                    writeReference(e.type);
                    break;

                case "type-predicate":
                    print(e.parameter.name);
                    print(" is ");
                    writeReference(e.type);
                    break;

                default:
                    throw new Error(`Unknown kind ${d.kind}`);
            }

        }
    }

    function writeTypeParameters(params: TypeParameter[]) {
        if (params.length === 0) return;

        print("<");

        let first = true;

        for (const p of params) {
            if (!first) print(", ");

            print(p.name);

            if (p.baseType) {
                print(" extends ");

                if (p.baseType.kind === "type-parameter")
                    print(p.baseType.name);
                else
                    writeReference(p.baseType);
            }

            if (p.default) {
                print(" = ");
                writeReference(p.default);
            }

            first = false;
        }

        print(">");
    }

    function writeInterface(d: InterfaceDeclaration) {
        printDeclarationComments(d);
        startWithDeclareOrExport(`interface ${d.name}`, d.flags);
        writeTypeParameters(d.typeParameters);
        print(" ");
        if (d.baseTypes && d.baseTypes.length) {
            print(`extends `);
            let first = true;
            for (const baseType of d.baseTypes) {
                if (!first) print(", ");
                writeReference(baseType);
                first = false;
            }
            print(" ");
        }
        if (d.members.length) { printObjectTypeMembers(d.members); }
        else { print("{ }"); }
        newline();
    }

    function writeFunctionType(f: FunctionType) {
        print("(");
        writeDelimited(f.parameters, ", ", writeParameter);
        print(")");
        print(" => ");
        writeReference(f.returnType);
    }

    function writeFunction(f: FunctionDeclaration) {
        printDeclarationComments(f);
        if (!isIdentifier(f.name)) {
            start(`/* Illegal function name '${f.name}' can't be used here`);
            newline();
        }

        startWithDeclareOrExport(`function ${f.name}`, f.flags);
        writeTypeParameters(f.typeParameters);
        print("(");
        writeDelimited(f.parameters, ", ", writeParameter);
        print("): ");
        writeReference(f.returnType);
        print(";");
        newline();

        if (!isIdentifier(f.name)) {
            start(`*/`);
            newline();
        }
    }

    function writeParameter(p: Parameter) {
        print(`${p.rest ? "..." : ""}${p.name}${p.optional ? "?" : ""}: `);
        writeReference(p.type);
    }

    function writeDelimited<T>(arr: T[], sep: string, printer: (x: T) => void) {
        let first = true;
        for (const el of arr) {
            if (!first) {
                print(sep);
            }
            printer(el);
            first = false;
        }
    }

    function writeClass(c: ClassDeclaration) {
        printDeclarationComments(c);
        startWithDeclareOrExport(`${classFlagsToString(c.flags)}class ${c.name}`, c.flags);
        writeTypeParameters(c.typeParameters);
        if (c.baseType) {
            print(" extends ");
            writeReference(c.baseType);
        }
        if (c.implements && c.implements.length) {
            print(" implements ");
            let first = true;
            for (const impl of c.implements) {
                if (!first) print(", ");
                writeReference(impl);
                first = false;
            }
        }
        print(" {");
        newline();
        indentLevel++;
        for (const m of c.members) {
            writeClassMember(m);
        }
        indentLevel--;
        start("}");
        newline();
    }

    function writeClassMember(c: ClassMember) {
        switch (c.kind) {
            case "property":
                return writePropertyDeclaration(c);
            case "method":
                return writeMethodDeclaration(c);
            case "constructor":
                return writeConstructorDeclaration(c);
        }
    }

    function writeConstructorDeclaration(ctor: ConstructorDeclaration) {
        printDeclarationComments(ctor);
        start("constructor(");
        writeDelimited(ctor.parameters, ", ", writeParameter);
        print(");");
        newline();
    }

    function writePropertyDeclaration(p: PropertyDeclaration) {
        printDeclarationComments(p);
        start(`${memberFlagsToString(p.flags)}${quoteIfNeeded(p.name)}`);
        print(": ");
        writeReference(p.type);
        print(";");
        newline();
    }

    function writeMethodDeclaration(m: MethodDeclaration) {
        printDeclarationComments(m);
        start(`${memberFlagsToString(m.flags)}${quoteIfNeeded(m.name)}`);
        if (hasFlag(m.flags, DeclarationFlags.Optional)) print("?");
        writeTypeParameters(m.typeParameters);
        print("(");
        const oldLength = output.length;
        const startFrom = getLastLineLength();
        writeDelimited(m.parameters, ", ", writeParameter);
        if ((getLastLineLength() - startFrom) > config.chop.methods) {
            output = output.slice(0, oldLength);
            const sep = `,${config.outputEol}${Array(startFrom).join(" ")}`;
            writeDelimited(m.parameters, sep, writeParameter);
        }
        print("): ");
        writeReference(m.returnType);
        print(";");
        newline();
    }

    function writeNamespaceMembers(members: NamespaceMember[]) {
        indentLevel++;
        let lastKind = "";
        for (let i = 0; i < members.length; i++) {
            const member = members[i];
            switch (member.kind) {
                case "class":
                case "interface":
                case "namespace":
                    if (i !== 0) { newline(); }
                    break;
                default:
                    if (i !== 0 && lastKind !== member.kind) { newline(); }
                    break;
            }
            lastKind = member.kind;
            writeDeclaration(member);
        }
        indentLevel--;
    }

    function writeNamespace(ns: NamespaceDeclaration) {
        printDeclarationComments(ns);
        startWithDeclareOrExport(`namespace ${ns.name} {`, ns.flags);
        contextStack.push(ContextFlags.InAmbientNamespace);
        newline();
        writeNamespaceMembers(ns.members);
        start(`}`);
        contextStack.pop();
        newline();
    }

    function writeGlobal(global: GlobalDeclaration) {
        printDeclarationComments(global);
        startWithDeclareOrExport(`global {`);
        contextStack.push(ContextFlags.InAmbientNamespace);
        newline();
        writeNamespaceMembers(global.members);
        start(`}`);
        contextStack.pop();
        newline();
    }

    function writeConst(c: ConstDeclaration) {
        printDeclarationComments(c);
        startWithDeclareOrExport(`const ${c.name}: `, c.flags);
        writeReference(c.type);
        print(";");
        newline();
    }

    function writeAlias(a: TypeAliasDeclaration) {
        printDeclarationComments(a);
        startWithDeclareOrExport(`type ${a.name}`, a.flags);
        writeTypeParameters(a.typeParameters);
        print(" = ");
        writeReference(a.type);
        print(";");
        newline();
    }

    function writeExportEquals(e: ExportEqualsDeclaration) {
        start(`export = ${e.target};`);
        newline();
    }

    function writeModule(m: ModuleDeclaration) {
        printDeclarationComments(m);
        startWithDeclareOrExport(`module '${m.name}' {`, m.flags);
        contextStack.push(ContextFlags.Module);
        newline();
        indentLevel++;
        for (const member of m.members) {
            writeDeclaration(member);
            newline();
        }
        indentLevel--;
        start(`}`);
        contextStack.pop();
        newline();
    }

    function writeImportAll(i: ImportAllDeclaration) {
        start(`import * as ${i.name} from '${i.from}';`);
        newline();
    }

    function writeImportDefault(i: ImportDefaultDeclaration) {
        start(`import ${i.name} from '${i.from}';`);
        newline();
    }

    function writeImportNamed(i: ImportNamedDeclaration) {
        start(`import {${i.name}`);
        if (i.as) {
            print(` as ${i.as}`);
        }
        print(`} from '${i.from}';`);
        newline();
    }

    function writeEnum(e: EnumDeclaration) {
        printDeclarationComments(e);
        startWithDeclareOrExport(`${e.constant ? "const " : ""}enum ${e.name} {`, e.flags);
        newline();
        indentLevel++;
        for (const member of e.members) {
            writeEnumValue(member);
        }
        indentLevel--;
        start(`}`);
        newline();
    }

    function writeEnumValue(e: EnumMemberDeclaration) {
        printDeclarationComments(e);
        start(e.name);
        print(",");
        newline();
    }

    function writeDeclaration(d: TopLevelDeclaration) {
        if (typeof d === "string") {
            return print(d);
        } else {
            switch (d.kind) {
                case "interface":
                    return writeInterface(d);
                case "function":
                    return writeFunction(d);
                case "class":
                    return writeClass(d);
                case "namespace":
                    return writeNamespace(d);
                case "global":
                    return writeGlobal(d);
                case "const":
                    return writeConst(d);
                case "alias":
                    return writeAlias(d);
                case "export=":
                    return writeExportEquals(d);
                case "module":
                    return writeModule(d);
                case "importAll":
                    return writeImportAll(d);
                case "importDefault":
                    return writeImportDefault(d);
                case "importNamed":
                    return writeImportNamed(d);
                case "enum":
                    return writeEnum(d);

                default:
                    return never(d, `Unknown declaration kind ${(d as TopLevelDeclaration).kind}`);
            }
        }
    }
}
