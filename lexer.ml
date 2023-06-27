open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
    let rec tok pos =
        if pos >= (String.length input) then
            []
        else if Str.string_match (Str.regexp "[ \t\n]") input pos then
            tok (pos + String.length (Str.matched_string input))
        else if Str.string_match (Str.regexp ")") input pos then
            Tok_RParen::(tok (pos + 1))
        else if Str.string_match (Str.regexp "=") input pos then
            Tok_Equal::(tok (pos + 1))
        else if Str.string_match (Str.regexp "<>") input pos then
            Tok_NotEqual::(tok (pos + 2))
        else if Str.string_match (Str.regexp ">=") input pos then
            Tok_GreaterEqual::(tok (pos + 2))
        else if Str.string_match (Str.regexp "<=") input pos then
            Tok_LessEqual::(tok (pos + 2))
        else if Str.string_match (Str.regexp ">") input pos then
            Tok_Greater::(tok (pos + 1))
        else if Str.string_match (Str.regexp "<") input pos then
            Tok_Less::(tok (pos + 1))
        else if Str.string_match (Str.regexp "||") input pos then
            Tok_Or::(tok (pos + 2))
        else if Str.string_match (Str.regexp "&&") input pos then
            Tok_And::(tok (pos + 2))
        else if Str.string_match (Str.regexp "not") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 3)) then
            Tok_Not::(tok (pos + 3))
        else if Str.string_match (Str.regexp "if") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 2)) then
            Tok_If::(tok (pos + 2))
        else if Str.string_match (Str.regexp "then") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 4)) then
            Tok_Then::(tok (pos + 4))
        else if Str.string_match (Str.regexp "else") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 4)) then
            Tok_Else::(tok (pos + 4))
        else if Str.string_match (Str.regexp "[+]") input pos then
            Tok_Add::(tok (pos + 1))
        else if Str.string_match (Str.regexp "[*]") input pos then
            Tok_Mult::(tok (pos + 1))
        else if Str.string_match (Str.regexp "[/]") input pos then
            Tok_Div::(tok (pos + 1))
        else if Str.string_match (Str.regexp "\\^") input pos then
            Tok_Concat::(tok (pos + 1))
        else if Str.string_match (Str.regexp "let") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 3)) then
            Tok_Let::(tok (pos + 3))
        else if Str.string_match (Str.regexp "def") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 3)) then
            Tok_Def::(tok (pos + 3))
        else if Str.string_match (Str.regexp "in") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 2)) then
            Tok_In::(tok (pos + 2))
        else if Str.string_match (Str.regexp "rec") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 3)) then
            Tok_Rec::(tok (pos + 3))
        else if Str.string_match (Str.regexp "fun") input pos && not (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos + 3)) then
            Tok_Fun::(tok (pos + 3))
        else if Str.string_match (Str.regexp "->") input pos then
            Tok_Arrow::(tok (pos + 2))
        else if Str.string_match (Str.regexp "[-]") input pos then
            Tok_Sub::(tok (pos + 1))
        else if Str.string_match (Str.regexp ";;") input pos then
            Tok_DoubleSemi::(tok (pos + 2))
        else if Str.string_match (Str.regexp "true\\|false") input pos then
            let token = Tok_Bool(bool_of_string (Str.matched_string input)) in
            let length = String.length (Str.matched_string input) in
            token::(tok (pos + length))
        else if Str.string_match (Str.regexp "[0-9]+") input pos then
            let token = Tok_Int(int_of_string (Str.matched_string input)) in
            let length = String.length (Str.matched_string input) in
            token::(tok (pos + length))
        else if Str.string_match (Str.regexp "(\\(-[0-9]+\\))") input pos then
            let token = Tok_Int(int_of_string (Str.matched_group 1 input)) in
            let length =  String.length (Str.matched_string input) in
            token::(tok (pos + length)) 
        else if Str.string_match (Str.regexp "(") input pos then
            Tok_LParen::(tok (pos + 1))
        else if Str.string_match (Str.regexp "\"\\([^\"]*\\)\"") input pos then
            let token = Tok_String(Str.matched_group 1 input) in
            let length = String.length (Str.matched_string input) in
            token::(tok (pos + length))
        else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
            let token = Tok_ID(Str.matched_string input) in
            let length = String.length (Str.matched_string input) in
            token::(tok (pos + length))
        else raise (InvalidInputException "No matching token")
    in
    tok 0
