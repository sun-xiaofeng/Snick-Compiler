(* Typedef for symbol entry *)
type symbol =
{
    identifier      : string;
    symbol_typespec : string;
    scope           : string;
    slot            : int;
    pass_by_ref     : bool;
    is_param        : bool
}

type proc =
{
    proc_name : string;
    proc_size : int
}

(* Typedef for symbol table *)
type symbol_list =
{
    mutable symbol_list : symbol list
}

type proc_list =
{
    mutable proc_list : proc list
}

(* Default symbol entry *)
let symbol_default =
{
    identifier = "";
    slot = 0;
    symbol_typespec = "";
    scope = "";
    pass_by_ref = false;
    is_param = false
}

let proc_default =
{
    proc_name = "";
    proc_size = 0
}

(* The symbol table instance *)
let symbol_table =
{
    symbol_list = []
}

let proc_table =
{
    proc_list = []
}

(* Initialises new symbol tables *)
let initialise () =
    symbol_table.symbol_list <- [];
    proc_table.proc_list <- []

(* Returns the symbol that matches the identifier *)
let get_symbol id scope =
    try (List.find (fun x -> x.identifier = id && x.scope = scope) symbol_table.symbol_list)
    with Not_found -> symbol_default

(* Returns the symbol that matches the slot number *)
let get_symbol_by_slot slot scope =
    try (List.find (fun x -> x.slot = slot && x.scope = scope) symbol_table.symbol_list)
    with Not_found -> symbol_default

(* Returns the procedure symbol that matches the identifier *)
let get_proc id =
    try (List.find (fun x -> x.proc_name = id) proc_table.proc_list)
    with Not_found -> proc_default

(* Insert a new symbol *)
let insert_symbol new_symbol =
    if not (List.exists (fun x -> x.identifier = new_symbol.identifier
        && x.scope = new_symbol.scope) symbol_table.symbol_list) then
        symbol_table.symbol_list <- symbol_table.symbol_list@[new_symbol]
    else
        failwith "Error: Symbol already exists."

(* Insert a new procedure symbol *)
let insert_proc new_proc =
    if not (List.exists (fun x -> x.proc_name = new_proc.proc_name) proc_table.proc_list) then
        proc_table.proc_list <- proc_table.proc_list@[new_proc]
    else
        failwith "Error: Precdure definition already exists."

(* Returns all symbols in a procedure *)
let get_symbols_by_proc proc_name =
    List.filter (fun x -> x.scope = proc_name) symbol_table.symbol_list

(* Returns all parameters in a procedure *)
let get_params_by_proc proc_name =
    List.filter (fun x -> x.scope = proc_name && x.is_param) symbol_table.symbol_list

(* Calculates the sum of symbol sizes of a procedure. *)
let calc_size_proc proc_name =
    let symbols_in_proc = get_symbols_by_proc proc_name in
    let total_size = ref 0 in
        List.iter
        (
            fun x -> total_size := !total_size + 1
        ) symbols_in_proc;
    !total_size

(* Prints the symbol table to stdout *)
let print_symbol_list symbol_list =
    List.iter
    (
        fun x -> print_string (
            x.identifier ^ " "
            ^ (string_of_int x.slot) ^ " "
            ^ x.symbol_typespec ^ " "
            ^ x.scope ^ " "
            ^ (string_of_bool x.pass_by_ref) ^ " "
            ^ (string_of_bool x.is_param)
            ^ "\n")
    ) symbol_list

(* Prints the procedure list to stdout *)
let print_proc_list proc_list =
    List.iter
    (
        fun x -> print_string (
            x.proc_name ^ " "
            ^ (string_of_int x.proc_size)
            ^ "\n")
    ) proc_list
