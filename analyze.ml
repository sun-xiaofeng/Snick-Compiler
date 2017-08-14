open Symbol
open Snick_ast

let bool_t = "Bool"
let int_t = "Int"
let float_t = "Float"
let string_t = "String"

let proc_slot_num = ref 0

let increament_slot_num () =
    proc_slot_num := !proc_slot_num + 1

let reset_slot_num () =
    proc_slot_num := 0

(* Analyzes a verable. Add the verable to the symbol list *)
let analyze_variables procname is_pass_by_ref param (id, typespec) =
    match typespec with
        | Bool -> Symbol.insert_symbol
        {
            identifier = id;
            slot = !proc_slot_num;
            symbol_typespec = bool_t;
            scope = procname;
            pass_by_ref = is_pass_by_ref;
            is_param = param
        };
        increament_slot_num ()
        | Int -> Symbol.insert_symbol
        {
            identifier = id;
            slot = !proc_slot_num;
            symbol_typespec = int_t;
            scope = procname;
            pass_by_ref = is_pass_by_ref;
            is_param = param
        };
        increament_slot_num ()
        | Float -> Symbol.insert_symbol
        {
            identifier = id;
            slot = !proc_slot_num;
            symbol_typespec = float_t;
            scope = procname;
            pass_by_ref = is_pass_by_ref;
            is_param = param
        };
        increament_slot_num ()

(* Analyzes a parameter *)
let analyze_parameter procname (arg_pass_type, typespec, ident) =
    let is_pass_by_ref =
        match arg_pass_type with
            | Ref -> true
            | Val -> false
    in analyze_variables procname is_pass_by_ref true (ident, typespec)

(* Analyzes all parameters in a procedure *)
let analyze_parameter_list procname params =
    List.iter (analyze_parameter procname) params

(* Analyzes a verable declaration *)
let analyze_declaration procname declaration
    = match declaration with
        | RegDecl (ident, typespec) -> analyze_variables procname false false (ident, typespec)
        | ArrayDecl _ -> ()

(* Analyzes the body of a procedure *)
let analyze_procbody procname (decls, stmts) =
    List.iter (analyze_declaration procname) decls

(* Analyze a procedure *)
let analyze_proc (procname, params, procbody) =
    reset_slot_num ();
    analyze_parameter_list procname params;
    analyze_procbody procname procbody;
    let new_proc =
    {
        proc_name = procname;
        proc_size = (Symbol.calc_size_proc procname)
    } in Symbol.insert_proc new_proc

(* Analyzes the program *)
let analyze_program program =
    Symbol.initialise ();
    List.iter analyze_proc program.procs;
