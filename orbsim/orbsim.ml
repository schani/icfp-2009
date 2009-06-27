(* orbsim: simulates the orbit
*)
open GMain

let space_earth_r = 6357000.0
let pi = atan 1. *. 4.;;
let two_pi = atan 1. *. 8.;;

(* 0,0 is always in middle of screen (coords of earth) *)
type space_screen = {
  mutable zoom :float; (* .. radius int space that is displayed around earth *)
  mutable screen_width :int;
  mutable screen_height :int;
  mutable screen_min :float; (* min (witdh, height) *)
  mutable screen_x_pre :float; (* ensures output is centered with x <> y *)
  mutable screen_y_pre :float;
}

let recalc_screen spasc width height =
  spasc.screen_height <- height;
  spasc.screen_width <- width;
  spasc.screen_min <- float_of_int (min height width);
  if width < height then begin
    spasc.screen_x_pre <- 0.0;
    spasc.screen_y_pre <- float_of_int (height - width) /. 2.0
  end else begin
    spasc.screen_x_pre <- float_of_int (width - height) /. 2.0;
    spasc.screen_y_pre <- 0.0
  end
 
(* convert coords
*)
let ccx spasc coord =
  ((coord +. spasc.zoom) *.
     ((spasc.screen_min -. 1.0) /. 2.0)) /. spasc.zoom +. spasc.screen_x_pre
let ccy spasc coord =
  ((coord +. spasc.zoom) *.
     ((spasc.screen_min -. 1.0) /. 2.0)) /. spasc.zoom +. spasc.screen_y_pre

(* convert a value (radius, ..)
*)
let vc spasc value =
  (value *. spasc.screen_min) /. (spasc.zoom *. 2.0)

let surface_from_gdk_pixmap gdkpixmap =
  Cairo_lablgtk.create gdkpixmap

let paint_line surface x1 y1 x2 y2 =
  Cairo.move_to surface x1 y1;
  Cairo.line_to surface x2 y2;
  Cairo.stroke surface

let paint_circle surface spasc x y r =
  Cairo.save surface;
  Cairo.new_path surface;
  Cairo.arc surface (ccx spasc x) (ccy spasc y) (vc spasc r) 0. two_pi;
  Cairo.fill surface;
  Cairo.restore surface;
  Cairo.stroke surface

let paint_earth surface spasc =
  Cairo.set_source_rgb surface 0.0 1.0 0.0;
  paint_circle surface spasc 0.0 0.0 space_earth_r

let create_space surface spasc =
  paint_earth surface spasc

let make_orbit_window () =
  let spasc = { zoom = space_earth_r *. 2.0;
		screen_height = 500;
		screen_width = 500;
		screen_min = 500.0;
		screen_x_pre = 0.0;
		screen_y_pre = 0.0
	      }
  in let w = GWindow.window ~height:500 ~width:500 ()
  in let vbox = GPack.vbox ~packing:w#add ()
  in let da = GMisc.drawing_area ~packing:vbox#add ()
  in
    da#misc#realize ();
    let d = new GDraw.drawable (da#misc#window)
    in
    let expose_event _ =
      let da_width, da_height = Gdk.Drawable.get_size (da#misc#window)
      in let pixmap = GDraw.pixmap ~width:da_width ~height:da_height ()
      in
	recalc_screen spasc da_width da_height;
	ignore (w#connect#destroy GMain.quit);
	pixmap#set_foreground (`NAME "blue");
	pixmap#rectangle ~x:0 ~y:0 ~width:da_width ~height:da_height
	  ~filled:true ();
	let surface = (surface_from_gdk_pixmap pixmap#pixmap)
	in
	  paint_earth surface spasc;
	  d#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
	    ~width:da_width ~height:da_height pixmap#pixmap;
	  false
    in
      ignore (da#event#connect#expose ~callback:expose_event);
      w#show ();
      GMain.Main.main ()

let _ =
  ignore (GMain.init ());
  make_orbit_window ()


(*
    *)
