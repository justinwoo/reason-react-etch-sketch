external window : Js.t {..} = "window" [@@bs.val];

module Page = {
  include ReactRe.Component.Stateful;

  type props = unit;

  type coord = Coord int int;

  module Coord = {
    type t = coord;

    let compare (Coord x1 y1) (Coord x2 y2) =>
      switch (Pervasives.compare x1 x2) {
      | 0 => Pervasives.compare y1 y2
      | x => x
      }
  };

  module Coords = Set.Make(Coord);

  type state = {
    cursor: coord,
    trail: Coords.t
  };

  type direction =
    | Left
    | Right
    | Up
    | Down;

  let increment = 10;
  let increment' = string_of_int increment;
  let width = 800;
  let width' = string_of_int width;
  let height = 600;
  let height' = string_of_int height;

  let lmap f (Coord x y) => Coord (f x) y;
  let rmap f (Coord x y) => Coord x (f y);

  let isInvalid (Coord x y) =>
    x < 0 ||
    x * increment > width ||
    y < 0 ||
    y * increment > height;

  let eval {state} direction => {
    let cursor' = switch direction {
    | Left => lmap pred state.cursor;
    | Right => lmap succ state.cursor;
    | Up => rmap pred state.cursor;
    | Down => rmap succ state.cursor;
    };
    switch (isInvalid cursor') {
    | false => Some {
      cursor: cursor',
      trail: Coords.add state.cursor state.trail
    }
    | true => None
    }
  };

  let componentDidMount {updater} => {
    window##onkeydown#=(fun (event) => {
      let direction = switch event##keyCode {
      | 37 => Some Left
      | 39 => Some Right
      | 38 => Some Up
      | 40 => Some Down
      | _ => None
      };
      switch direction {
      | Some dir => updater eval dir
      | None => ()
      };
    });
    None
  };

  let getInitialState _ => {
    {
      cursor: Coord 0 0,
      trail: Coords.empty
    }
  };
  let name = "Page";

  let point color (Coord x y) =>
    <rect
      key=(string_of_int x ^ "x" ^ string_of_int y ^ "y")
      fill=(color)
      width=(increment')
      height=(increment')
      x=(string_of_int @@ x * increment)
      y=(string_of_int @@ y * increment)
    />;

  let render {state} => {
    let trail = ReactRe.listToElement @@ List.map (point "grey") @@ Coords.elements state.trail;
    let cursor = point "black" state.cursor;
    <svg width=(width') height=(height') className="board">
      trail
      cursor
    </svg>
  };
};

include ReactRe.CreateComponent Page;

let createElement = wrapProps ();
