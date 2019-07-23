open Printf      

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    exception DiviZero

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let rec string_from_intList' numberList counter = 
        match (numberList, counter) with
        | [], counter             -> ""
        | car::cdr, 69            -> ("\\\n" ^ (string_of_int car) ^ 
                                     (string_from_intList' cdr 1) )
        | car::cdr, counter       -> ((string_of_int car) ^ 
                            (string_from_intList' cdr (counter+1)))

    let string_from_intList numberList negYN =
        let counter = negYN
        in string_from_intList' numberList counter

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  ((if sign = Pos then "" else "-") ^
          string_from_intList reversed (if sign = Pos then 0 else 1))


    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = trimzeros' cdr
                 in  match car, cdr' with
                     | 0, [] -> []
                     | car, cdr' -> car::cdr'
        in trimzeros' list

    let rec cmp' list1 list2 rV =
        match (list1, list2) with
        | [], []                   -> rV
        | [], list2                -> (-1)
        | list1, []                -> 1
        | car1::cdr1, car2::cdr2   -> 
            if (car1 > car2)
            then cmp' cdr1 cdr2 1 
            else if (car1 < car2)
                 then cmp' cdr1 cdr2 (-1)
                 else cmp' cdr1 cdr2 rV
                 
    let rec add' list1 list2 carry = 
        match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 borrow =
        match (list1, list2, borrow) with
        | list1, [], 0                     -> list1
        | [], list2, borrow                -> 
          printf "Impossible Sub' Call\n%!";
          []
        | car1::cdr1, [], borrow           -> 
            if ((car1-borrow) < 0) 
            then 9 :: sub' cdr1 list2 1
            else if ((car1-borrow) = 0)
                 then 0 :: sub' cdr1 list2 0
                 else (car1-borrow) :: sub' cdr1 list2 0
        | car1::cdr1, car2::cdr2, borrow   -> 
            let val1 = car1-borrow 
            in match(val1) with
            | -1        -> (9-car2) :: sub' cdr1 cdr2 1
            | val1      -> 
                if val1 < car2 
                then ((radix+val1) - car2) :: sub' cdr1 cdr2 1
                else if val1 = car2
                     then 0 :: sub' cdr1 cdr2 0
                     else (val1-car2) :: sub' cdr1 cdr2 0


    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if neg1 = neg2
        then Bigint (neg1, 
                     add' (trimzeros value1) (trimzeros value2) 0)
        else match (cmp' value1 value2 0) with
             | -1              -> Bigint(neg2, sub' value2 value1 0)
             | 1               -> Bigint(neg1, sub' value1 value2 0)
             | 0               -> zero
             | _               -> printf "Invalid cmp'Return\n%!";
                                  zero

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if neg1 != neg2
        then match (cmp' value1 value2 0) with
             | -1              -> Bigint(neg1, 
                add' (trimzeros value1) (trimzeros value2) 0)
             | 1               -> Bigint(neg1, 
                add' (trimzeros value1) (trimzeros value2) 0)
             | 0               -> zero
             | _               -> printf "Invalid cmp'Return\n%!";
                                  zero
        else match (cmp' value1 value2 0) with
             | -1              -> 
        Bigint((if neg2=Neg then Pos else Neg),
          (trimzeros (sub' (trimzeros value2) (trimzeros value1) 0)))
             | 1               -> 
        Bigint((if neg1=Neg then Neg else Pos), 
          (trimzeros (sub' (trimzeros value1) (trimzeros value2) 0)))
             | 0               -> zero
             | _               -> printf "Invalid cmp'Return\n%!";
                                  zero

    let rec mul'' list1 nmbr carry = 
        match (list1) with
        | []                   -> [carry]
        | car1::cdr1           -> 
            let prod = car1 * nmbr + carry in
                (prod mod 10)::(mul'' cdr1 nmbr (prod / 10))

    let rec mul' list1 list2 =
        match (list1, list2) with
        | [], list2                -> []
        | list1, []                -> []
        | list1, car2::cdr2   ->
          add' (mul'' list1 (car2) 0) 
               (0::(mul' list1 (cdr2))) 0

    let mul (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2 then Bigint (Pos, trimzeros (mul' val1 val2))
        else Bigint (Neg, trimzeros (mul' val1 val2))

    let rec div' divisor dividend ov nv value = 
        let vcmp = 
            cmp' (trimzeros (mul' (add' value nv 0) dividend)) 
                 divisor 0 in
            if vcmp > 0 then ov
            else if vcmp < 0 
                then div' divisor dividend nv (mul' nv [2]) value
            else nv

    let rec divr divisor dividend value = 
        let nv = div' divisor dividend [] [1] value in
            if nv = [] then value
            else divr divisor dividend (add' value nv 0)

    let div (Bigint (neg1, val1)) (Bigint (neg2, val2)) = 
        match (val1, val2) with
            | [], val2     -> zero
            | val1, []     -> printf "Invalid div call\n%!";
                              zero
            | val1, val2 -> 
              match (cmp' val2 [0] 0) with
              | 0              ->
              raise DiviZero
              | 1              -> 
              Bigint ((if neg1 = neg2 then Pos else Neg), 
                      trimzeros (divr val1 val2 []))
              | _              -> 
              printf "Invalid compare\n%!";
              zero 
              

    let rem (Bigint (neg1, val1)) (Bigint (neg2, val2)) = 
        Bigint (neg1,  
               (trimzeros (sub' val1 (trimzeros (mul' 
                          (divr val1 val2 []) val2)) 0)))

    let rec pow' list1 list2 olist1 =
        let list2' = (trimzeros (sub' list2 [1] 0)) in
            if list2' = [] then list1
            else pow' (mul' list1 olist1) list2' olist1

    let pow (Bigint (neg1, val1)) (Bigint (neg2, val2)) = 
        if neg2 = Neg then zero
        else match (val1, val2) with
            | val1, []     -> Bigint (Pos, [1])
            | [], val2     -> zero
            | val1, [0]    -> Bigint (Pos, [1])
            | val1, val2   -> Bigint (neg1, 
                           (trimzeros (pow' val1 val2 val1)))

end
