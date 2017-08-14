
open Snick_ast
open Format
open Symbol

let bool_t = "Bool"
let int_t = "Int"
let float_t = "Float"
let string_t = "String"

(* The possible result type of a binop expression *)
let binop_result_type binop =
    match binop with
        | Op_add -> [int_t; float_t]
        | Op_sub -> [int_t; float_t]
        | Op_mul -> [int_t; float_t]
        | Op_div -> [int_t; float_t]
        | Op_eq -> [bool_t]
        | Op_noteq -> [bool_t]
        | Op_lt -> [bool_t]
        | Op_gt -> [bool_t]
        | Op_lteq -> [bool_t]
        | Op_gteq -> [bool_t]
        | Op_and -> [bool_t]
        | Op_or -> [bool_t]

(* The type of operands of a binop expression *)
let binop_operands_type binop =
    match binop with
        | Op_eq -> [bool_t; int_t; float_t]
        | Op_noteq -> [bool_t; int_t; float_t]
        | Op_lt -> [int_t; float_t]
        | Op_gt -> [int_t; float_t]
        | Op_lteq -> [int_t; float_t]
        | Op_gteq -> [int_t; float_t]
        | Op_and -> [bool_t]
        | Op_or -> [bool_t]
        | Op_add -> [int_t; float_t]
        | Op_sub -> [int_t; float_t]
        | Op_mul -> [int_t; float_t]
        | Op_div -> [int_t; float_t]

(* The type of the operand or result of a unop expression *)
let unop_type unop =
    match unop with
        | Op_minus -> [int_t; float_t]
        | Op_not -> [bool_t]

(* The label number *)
let label_num = ref 0
(* The register number *)
let reg_num = ref 0

(* Generates a primitive type declaration *)
let generate_reg_decl fmt (ident, typespec) scope =
    let symbol = Symbol.get_symbol ident scope in
    (match typespec with
        | Int   -> fprintf fmt "int_const r0, 0\n"
        | Bool  -> fprintf fmt "int_const r0, 0\n"
        | Float -> fprintf fmt "real_const r0, 0.0\n");
    let slot_num = symbol.slot in
    fprintf fmt "store %d, r0\n" slot_num

(* Stores a boolean constant at given register *)
let generate_bool_const fmt value register =
    if value then
        fprintf fmt "int_const r%d, 1\n" register
    else
        fprintf fmt "int_const r%d, 0\n" register

(* Stores a integer constant at given register *)
let generate_int_const fmt value register =
    fprintf fmt "int_const r%d, %d\n" register value

(* Stores a flost constant at given register *)
let generate_float_const fmt value register =
    fprintf fmt "real_const r%d, %f\n" register value

(* Stores a string constant at given register *)
let generate_string_const fmt value register =
    fprintf fmt "string_const r%d, %s\n" register value

(* Loads a lvalue into a register *)
let generate_lvalue_load fmt lvalue scope register =
    match lvalue with
        | LId ident ->
            let lsymbol = Symbol.get_symbol ident scope in
            let lsymbol_slot = lsymbol.slot in
            if lsymbol.pass_by_ref then
            (
                (* Load the address stored in the stack slot *)
                fprintf fmt "load r%d, %d\n" register lsymbol_slot;
                (* Dereference *)
                fprintf fmt "load_indirect r%d, r%d\n" register register
            )
            else
                (* Load the value stored in the stack slot *)
                fprintf fmt "load r%d, %d\n" register lsymbol_slot
        | LArray _ -> ()

(* Generates variable declaration *)
let generate_variable_decl fmt decl scope =
    match decl with
        | RegDecl (ident, typespec) -> generate_reg_decl fmt (ident, typespec) scope
        | ArrayDecl _ -> ()

(* Returns the type of a lvalue*)
let get_lvalue_type lvalue scope =
    match lvalue with
        | LId ident ->
            let symbol = Symbol.get_symbol ident scope in
            symbol.symbol_typespec
        | LArray _ -> ""

(* Returns the type of an expression *)
let rec get_expr_type expr scope =
    match expr with
        | Ebool _ -> bool_t
        | Eint _  -> int_t
        | Efloat _ -> float_t
        | Estring _ -> string_t
        | Elval lvalue -> get_lvalue_type lvalue scope
        | Ebinop binop -> get_binop_type binop scope
        | Eunop unop -> get_unop_type unop scope
(* Returns the type of a binop expression*)
and get_binop_type (expr1, binop, expr2) scope =
    let expr1_type = get_expr_type expr1 scope in
    let expr2_type = get_expr_type expr2 scope in
    let result_types = binop_result_type binop in
    let operands_types = binop_operands_type binop in
        (* Check types of the operands *)
        if List.mem bool_t result_types then
        (
            if ((expr1_type = bool_t || expr2_type = bool_t)
                && not (List.mem bool_t operands_types))
                || ((expr1_type = float_t || expr2_type = float_t)
                && not (List.mem float_t operands_types))
                || ((expr1_type = int_t || expr2_type = int_t)
                && not (List.mem int_t operands_types)) then
                failwith "Error: Operator type mismatch"
            else bool_t
        )
        else
        (
            if (expr1_type = int_t && expr2_type = int_t
                && List.mem int_t result_types) then
                int_t
            else if (expr1_type = float_t && expr2_type = float_t
                && List.mem float_t result_types) then
                float_t
            else if (expr1_type = float_t && expr2_type = int_t
                && List.mem float_t result_types) then
                float_t
            else if (expr1_type = int_t && expr2_type = float_t
                && List.mem float_t result_types) then
                float_t
            else
                failwith "Error: Operator type mismatch"
        )
(* Returns the type of a unop expression *)
and get_unop_type (unop, expr) scope =
    let expr_type = get_expr_type expr scope in
    (* Check the expression type matches the operand type *)
    if (expr_type = bool_t && List.mem bool_t (unop_type unop)) then
        bool_t
    else
    (
        if (expr_type = int_t && List.mem int_t (unop_type unop)) then
            int_t
        else if (expr_type = float_t && List.mem float_t (unop_type unop)) then
            float_t
        else
            failwith "Error: Operator type mismatch"
    )

(* Generates code for a unary minus *)
let generate_uminus fmt (unop, expr) reg scope =
    let next_reg = reg + 1 in
    let expr_type = get_expr_type expr scope in
    if expr_type = int_t then
    (
        fprintf fmt "int_const r%d, 0\n" next_reg;
        fprintf fmt "sub_int r%d, r%d, r%d\n" reg next_reg reg
    )
    else if expr_type = float_t then
    (
        fprintf fmt "real_const r%d, 0.0\n" next_reg;
        fprintf fmt "sub_real r%d, r%d, r%d\n" reg next_reg reg
    )
    else
        failwith "Error: Operator type mismatch"

(* Generates a logical NOT expression*)
let generate_not fmt (unop, expr) reg scope =
    let expr_type = get_expr_type expr scope in
    if expr_type = bool_t then
        fprintf fmt "not r%d, r%d\n" reg reg
    else
        failwith "Error: Operator type mismatch"

(* Generates an expression *)
let rec generate_expression fmt expr scope register =
    match expr with
        | Ebool value -> generate_bool_const fmt value register
        | Eint value -> generate_int_const fmt value register
        | Efloat value -> generate_float_const fmt value register
        | Estring value -> generate_string_const fmt value register
        | Elval value -> generate_lvalue_load fmt value scope register
        | Ebinop binop -> generate_binop fmt binop scope register
        | Eunop unop -> generate_unop fmt unop scope register
(* Generates a binop expression *)
and generate_binop fmt (expr1, binop, expr2) scope reg =
    let next_reg = reg + 1 in
    generate_expression fmt expr1 scope reg;
    generate_expression fmt expr2 scope next_reg;
    let type1  = get_expr_type expr1 scope in
    let type2 = get_expr_type expr2 scope in
        match binop with
            | Op_add ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "add_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "add_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "add_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "add_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_sub ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "sub_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "sub_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "sub_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "sub_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"

            | Op_mul ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "mul_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "mul_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "mul_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "mul_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_div ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "div_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "div_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "div_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "div_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_eq ->
                if (type1 = int_t && type2 = int_t) || (type1 = bool_t && type2 = bool_t) then
                    fprintf fmt "cmp_eq_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "cmp_eq_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "cmp_eq_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "cmp_eq_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_noteq ->
                if (type1 = int_t && type2 = int_t) || (type1 = bool_t && type2 = bool_t) then
                    fprintf fmt "cmp_ne_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "cmp_ne_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "cmp_ne_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "cmp_ne_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_lt ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "cmp_lt_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "cmp_lt_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "cmp_lt_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "cmp_lt_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_gt ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "cmp_gt_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "cmp_gt_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "cmp_gt_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "cmp_gt_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_lteq ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "cmp_le_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "cmp_le_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "cmp_le_real r%d, r%d, r%d\n" reg reg next_reg
                    )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "cmp_le_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else
                    failwith "Error: Operator type mismatch"
            | Op_gteq ->
                if type1 = int_t && type2 = int_t then
                    fprintf fmt "cmp_ge_int r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = float_t && type2 = float_t then
                    fprintf fmt "cmp_ge_real r%d, r%d, r%d\n" reg reg next_reg
                else if type1 = int_t && type2 = float_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" reg reg;
                    fprintf fmt "cmp_ge_real r%d, r%d, r%d\n" reg reg next_reg
                )
                else if type1 = float_t && type2 = int_t then
                (
                    fprintf fmt "int_to_real r%d, r%d\n" next_reg next_reg;
                    fprintf fmt "cmp_ge_real r%d, r%d, r%d\n" reg reg next_reg
                )
            else
                failwith "Error: Operator type mismatch"
            | Op_and ->
                if type1 = bool_t then
                    fprintf fmt "and r%d, r%d, r%d\n" reg reg next_reg
                else
                    failwith "Error: Operator type mismatch"
            | Op_or ->
                if type1 = bool_t then
                    fprintf fmt "or r%d, r%d, r%d\n" reg reg next_reg
                else
                    failwith "Error: Operator type mismatch"
(* Genreates a unop expression *)
and generate_unop fmt (unop, expr) scope register =
    generate_expression fmt expr scope register;
    match unop with
        | Op_minus -> generate_uminus fmt (unop, expr) register scope
        | Op_not -> generate_not fmt (unop, expr) register scope

(* Generates an assignment statement *)
let generate_assignment fmt lvalue rvalue scope =
    fprintf fmt "# assignment\n";
    match lvalue with
        | LId ident -> (
            let lsymbol = Symbol.get_symbol ident scope in
            let lsymbol_slot = lsymbol.slot in
            match rvalue with
                | Rexpr expr ->
                    generate_expression fmt expr scope 0;
                    let expr_type = get_expr_type expr scope in
                    let lsymbol_type = get_lvalue_type lvalue scope in
                    if (lsymbol_type = float_t && expr_type = int_t) then
                    (
                        fprintf fmt "int_to_real r0, r0\n"
                    );
                    if lsymbol.pass_by_ref then
                    (
                        fprintf fmt "load r1, %d\n" lsymbol_slot;   (* Load the lvalue address into r1 *)
                        fprintf fmt "store_indirect r1, r0\n"       (* Store r0 into the address pointing to *)
                    )
                    else
                        fprintf fmt "store %d, r0\n" lsymbol_slot
        )
        | LArray _ -> ()

(* Generates code for storing a lvalue *)
let generate_lvalue_store fmt lvalue scope =
    match lvalue with
        | LId ident ->
            let lsymbol = Symbol.get_symbol ident scope in
            let lsymbol_slot = lsymbol.slot in
            if lsymbol.pass_by_ref then
            (
                fprintf fmt "load r1, %d\n" lsymbol_slot;
                fprintf fmt "store_indirect r1, r0\n"
            )
            else
                fprintf fmt "store %d, r0\n" lsymbol_slot
        | LArray _ -> ()

(* Generates code for a read call *)
let generate_read_statement fmt lvalue scope =
    fprintf fmt "# read\n";
    let lvalue_type = get_lvalue_type lvalue scope in
    (if lvalue_type = bool_t then
        fprintf fmt "call_builtin read_bool\n"
    else if lvalue_type = int_t then
        fprintf fmt "call_builtin read_int\n"
    else if lvalue_type = float_t then
        fprintf fmt "call_builtin read_real\n");
    generate_lvalue_store fmt lvalue scope

(* Generate code for a write call *)
let generate_write_statement fmt expr scope =
    fprintf fmt "# write\n";
    generate_expression fmt expr scope 0;
    let expr_type = get_expr_type expr scope in
    if expr_type = bool_t then
        fprintf fmt "call_builtin print_bool\n"
    else if expr_type = int_t then
        fprintf fmt "call_builtin print_int\n"
    else if expr_type = float_t then
        fprintf fmt "call_builtin print_real\n"
    else if expr_type = string_t then
        match expr with
            | Estring str ->
                fprintf fmt "call_builtin print_string\n"
            | _ -> ()

(* Generate code for load in a actual parameter in a procedure call *)
let generate_parameter_load fmt proc_name expr scope =
    match expr with
        | Elval lvalue -> (
            match lvalue with
                | LId ident ->
                    let actual_param = Symbol.get_symbol ident scope in
                    let slot = actual_param.slot in
                    let formal_param =
                        Symbol.get_symbol_by_slot !reg_num proc_name in
                    if actual_param.pass_by_ref then
                    (
                        if (formal_param.pass_by_ref) then
                        (
                            fprintf fmt "load r%d, %d\n" !reg_num slot;
                            reg_num := !reg_num + 1
                        )
                        else
                        (
                            fprintf fmt "load r%d, %d\n" !reg_num slot;
                            fprintf fmt "load_indirect r%d, r%d\n" !reg_num !reg_num;
                            reg_num := !reg_num + 1
                        )
                    )
                    else
                    (
                        if formal_param.pass_by_ref then
                        (
                            fprintf fmt "load_address r%d, %d\n" !reg_num slot;
                            reg_num := !reg_num + 1
                        )
                        else
                        (
                            fprintf fmt "load r%d, %d\n" !reg_num slot;
                            let actual_param_type = actual_param.symbol_typespec in
                            let formal_param_type = formal_param.symbol_typespec in
                            if (actual_param_type = int_t && formal_param_type = float_t) then
                            (
                                fprintf fmt "int_to_real r%d, r%d\n" !reg_num !reg_num
                            );
                            reg_num := !reg_num + 1
                        )
                    )
                | LArray _ -> ()
        )
        | _ ->
            generate_expression fmt expr scope !reg_num;
            reg_num := !reg_num + 1

(* Genereates code for a procedure call *)
let generate_proc_call fmt (proc_name, exprs) scope =
    fprintf fmt "# call\n";
    reg_num := 0;
    List.iter (fun x -> generate_parameter_load fmt proc_name x scope) exprs;
    fprintf fmt "call proc_%s\n" proc_name

(* Generates code for a statement *)
let rec generate_statement fmt stmt scope =
    match stmt with
        | Assign (lvalue, rvalue) -> generate_assignment fmt lvalue rvalue scope
        | Read lvalue -> generate_read_statement fmt lvalue scope
        | Write expr -> generate_write_statement fmt expr scope
        | ProcCall proccall -> generate_proc_call fmt proccall scope
        | Ifthen ifthen -> generate_if_then fmt ifthen scope
        | IfthenElse ifthenelse -> generate_if_then_else fmt ifthenelse scope
        | WhileDo whiledo -> generate_while fmt whiledo scope
and generate_if_then fmt (expr, stmts) scope =
    label_num := !label_num + 1;
    let curr_label_num = !label_num in
    fprintf fmt "# if\n";
    generate_expression fmt expr scope 0;
    fprintf fmt "branch_on_false r0, label_%d\n" curr_label_num;
    List.iter (fun x -> (generate_statement fmt) x scope) stmts;
    fprintf fmt "label_%d:\n" curr_label_num
and generate_if_then_else fmt (expr, stmts1, stmts2) scope =
    label_num := !label_num + 1;
    let curr_label_num = !label_num in
    fprintf fmt "# if\n";
    (* Generate the condition *)
    generate_expression fmt expr scope 0;
    (* Goto else part if the r0 is false *)
    fprintf fmt "branch_on_false r0, label_%d_else\n" curr_label_num;
    List.iter (fun x -> (generate_statement fmt) x scope) stmts1;
    (* Goto endif *)
    fprintf fmt "branch_uncond label_%d_end\n" curr_label_num;
    (* Else part *)
    fprintf fmt "label_%d_else:\n" curr_label_num;
    List.iter (fun x -> (generate_statement fmt) x scope) stmts2;
    (* End if *)
    fprintf fmt "label_%d_end:\n" curr_label_num
and generate_while fmt (expr, stmts) scope =
    label_num := !label_num + 1;
    let curr_label_num = !label_num in
    fprintf fmt "# while\n";
    fprintf fmt "label_%d_cond:\n" curr_label_num;
    (* Generate the condition *)
    generate_expression fmt expr scope 0;
    (* Goto endwhile if r0 is false *)
    fprintf fmt "branch_on_false r0, label_%d_end\n" curr_label_num;
    List.iter (fun x -> (generate_statement fmt) x scope) stmts;
    (* Goto start of while *)
    fprintf fmt "branch_uncond label_%d_cond\n" curr_label_num;
    (* End while *)
    fprintf fmt "label_%d_end:\n" curr_label_num

(* Geneates code for a paramter in a procedure *)
let generate_parameter_store fmt (passspec, typespec, ident) procname =
    match typespec with
        | Bool ->
            let slot_num = (Symbol.get_symbol ident procname).slot in
            fprintf fmt "store %d, r%d\n" slot_num !reg_num;
            reg_num := !reg_num + 1 (* Increment the register number *)
        | Int ->
            let slot_num = (Symbol.get_symbol ident procname).slot in
            fprintf fmt "store %d, r%d\n" slot_num !reg_num;
            reg_num := !reg_num + 1
        | Float ->
            let slot_num = (Symbol.get_symbol ident procname).slot in
            fprintf fmt "store %d, r%d\n" slot_num !reg_num;
            reg_num := !reg_num + 1

let generate_proc_body fmt (decls, stmts) proc_name =
    if List.length decls > 0 then
    (
        List.iter (fun decl -> (generate_variable_decl fmt) decl proc_name) decls
    );
    List.iter (fun stmt -> (generate_statement fmt) stmt proc_name) stmts

(* Generates code for a procedure *)
let generate_proc fmt (procname, params, procbody) =
    let proc_symbol = Symbol.get_proc procname in
    let stack_frame_size = proc_symbol.proc_size in
        fprintf fmt "proc_%s:\n" procname;
        fprintf fmt "# prologue\n";
        fprintf fmt "push_stack_frame %d\n" stack_frame_size;
        if (List.length params) > 0 then
        (
            reg_num := 0; (* Reset the register. Read parameters from r0 *)
            List.iter
            (
                fun param -> (generate_parameter_store fmt) param procname
            ) params;
        );
        generate_proc_body fmt procbody procname;
        fprintf fmt "# epilogue\n";
        fprintf fmt "pop_stack_frame %d\n" stack_frame_size;
        fprintf fmt "return\n"

(* Generates code for the entire program *)
let generate_program formatter program =
    fprintf formatter "call proc_main\n";
    fprintf formatter "halt\n";
    List.iter (generate_proc formatter) program.procs
