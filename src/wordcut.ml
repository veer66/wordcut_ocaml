let load_dict dict_path = 
  let in_chan = open_in dict_path in
  let rec loop d acc = match (Uutf.decode d) with
    | `Uchar 0x000A  -> let w = List.hd acc in
                        let _w = List.rev w in
                        let cdr = List.tl acc in
                        let _acc = _w :: cdr in
                        loop d ([] :: _acc)
    | `Uchar ch -> let w = List.hd acc in
                   let _w = ch :: w in
                   let cdr = List.tl acc in
                   let _acc = _w :: cdr in
                   loop d _acc
    | `End -> List.rev acc
    | `Malformed _ -> loop d acc
    | `Await -> assert false in
  let nln = `Readline 0x000A in
  let d = Uutf.decoder ~nln ~encoding:`UTF_8 (`Channel in_chan) in
  loop d [[]];;

let utf8_to_uchars s =
  let rec loop d ch_list = match (Uutf.decode d) with
    | `Uchar ch -> loop d (ch :: ch_list)
    | `End -> List.rev ch_list
    | `Malformed _ -> loop d ch_list
    | `Await -> assert false in
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  loop d [];;

type policy = [ `LEFT | `RIGHT ]
                
let dict_seek dict l r offset ch policy =
  let rec seek l r a =
    let seek_ch l r a m w =
      let ch_ = List.nth w offset in
      if ch_ < ch then
        seek (m+1) r a
      else
        if ch_ > ch then
          seek l (m-1) a
        else
          let a_ = Some m in
          match policy with
          | `LEFT -> seek l (m-1) a_
          | `RIGHT -> seek (m+1) r a_ in           
    let _seek l r a =
      let m = (l + r) / 2 in
      let w = List.nth dict m in
      let wlen = List.length w in
      if wlen <= offset then
        seek (m+1) r a
      else
        seek_ch l r a m w
    in
    if l > r then
      a
    else
      _seek l r a
  in
  seek l r None;;


type acc_state = [ `FINAL | `DEFAULT ]
                  
type acceptor = {
    l: int;
    r: int;
    state: acc_state;
    offset: int;
    dict: int list list;
  } ;;

let new_acceptor dict = {l = 0;
                         r = ((List.length dict) - 1);
                         state = `DEFAULT;
                         offset = 0;
                         dict =  dict};;
  
let transit acceptor ch = 
  let l = dict_seek acceptor.dict
                      acceptor.l
                      acceptor.r
                      acceptor.offset
                      ch
                      `LEFT
  in
  let _transit_lr l r =
    let w = List.nth acceptor.dict l in
    let wlen = List.length w in
    let offset = acceptor.offset + 1 in
    let state = if wlen = offset then `FINAL else `DEFAULT in
    
    Some {l = r;
          r = r;
          state = state;
          offset = offset;
          dict = acceptor.dict} 
  in                                           
  let _transit_l l =
    let r = dict_seek acceptor.dict
                      l
                      acceptor.r
                      acceptor.offset
                      ch
                      `RIGHT
    in    
    match r with
    | None -> None
    | Some r -> _transit_lr l r
  in  
  match l with
  | None -> None
  | Some l -> _transit_l l
  ;;
    
let dict = load_dict "thai.txt" in
    let word = utf8_to_uchars "กา" in
    let ch = (List.nth word 0) in
    (*    let pos = dict_seek dict 0 r 0 ch `RIGHT in *)
    let acc = new_acceptor dict in
    transit acc ch;;
