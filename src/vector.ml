(** A 2d vector structure. *)

(** The type of a vector is a record with an x and y value. *)
type t = { x : float; y : float }

(** Basis vector in the x-direction. *)
let i = { x = 1.0; y = 0.0 }

(** Basis vector in the y-direction. *)
let j = { x = 0.0; y = 1.0 }

(** Null vector has 0 for both coordinates. *)
let null_vector = { x = 0.0; y = 0.0 }

(** See {!null_vector}. *)
let null = null_vector

(** See {!null_vector}. *)
let zero = null_vector

(** Creates a new vector given an x and y. *)
let create x y = { x = x; y = y }

(** See {!create}. *)
let vector = create

(** Set the x value of the given vector. *)
let setx a x = { x = x; y = a.y }

(** Set the y value of the given vector. *)
let sety a y = { x = a.x; y = y }

(** Vector addition. *)
let ( +| ) a b = { x = a.x +. b.x; y = a.y +. b.y }

(** Vector subtraction. *)
let ( -| ) a b = { x = a.x -. b.x; y = a.y -. b.y }

(** Multiplication between a vector and a scalar. *)
let ( *| ) b a = { x = a.x *. b; y = a.y *. b }

(** Division between a vector and a scalar. *)
let ( /| ) a b = { x = a.x /. b; y = a.y /. b }

(** Vector equality. *)
let ( =| ) a b = (a.x = b.x) && (a.y = b.y)

(** Vector dot product. *)
let ( *.| ) a b = (a.x *. b.x) +. (a.y *. b.y)

(** Returns the norm of the vector, i.e. the length or magnitude. *)
let norm a = sqrt( (a.x**2.0) +. (a.y**2.0) )

(** See {!norm}. *)
let magnitude = norm

(** See {!norm}. *)
let length = norm

(** Normalizes the given vector so that it's magnitude is 1.0. *)
let normalize a = let len = norm a in
  { x = (a.x /. len); y = (a.y /. len) }

(** Scales a vector by the given x factor and y factor. *)
let scale v x y = { x = v.x *. x; y = v.y *. y }

let dist a b = sqrt( (a.x -. b.x)**2.0 +. (a.y -. b.y)**2.0 )

let shorten v x =
  let unit = normalize v in
  let len = magnitude v in
  let scale = (len -. x) /. len in
    (scale*.len) *| unit

(** The constant PI. *)
let pi = 3.1415926535897932

let deg_to_rad x = (x *. pi) /. 180.0

let rotate v theta = {
  x = (cos theta) *. v.x -. (sin theta) *. v.y;
  y = (sin theta) *. v.x +. (cos theta) *. v.y
}

let rotate_d v theta = let theta = deg_to_rad theta in
  rotate v theta

let translate v x y = { x = v.x+.x; y = v.y+.y }

(** Returns a string representation of a vector. *)
let string_of_vector v =
  "("^(string_of_float v.x)^","^(string_of_float v.y)^")"
