(* orbsim: simulates the orbit
 *)

(* TODO:
   + fadenkreuz wenn erde zu klein

*)

open GMain

let earth_r = 6357000.0
let initial_zoom = 4.0
let pi = atan 1. *. 4.0;;
let two_pi = pi *. 2.0;;

let rgb_red =     1.0, 0.0, 0.0
let rgb_green =   0.0, 1.0, 0.0
let rgb_blue =    0.0, 0.0, 1.0
let rgb_yellow =  1.0, 1.0, 0.0
let rgb_cyan =    0.0, 1.0, 1.0
let rgb_magenta = 1.0, 0.0, 1.0

let our_x = ref 0.0
let our_y = ref 0.0
  
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
  Cairo.arc surface (ccx spasc x) (ccy spasc y) r 0. two_pi;
  Cairo.fill surface;
  Cairo.restore surface;
  Cairo.stroke surface

let set_color surface (r,g,b) =
  Cairo.set_source_rgb surface r g b
  
let paint_sat surface spasc x y =
  set_color surface rgb_red;
  paint_circle surface spasc x y 5.0

let paint_earth surface spasc =
  set_color surface rgb_green;
  paint_circle surface spasc 0.0 0.0 (vc spasc earth_r)

let create_space surface spasc =
  paint_earth surface spasc

let refresh_da da =
  GtkBase.Widget.queue_draw da#as_widget

let status_line = ref (GMisc.label ~text:"Statusline" ~justify:`FILL ())
    
let make_orbit_window () =
  let spasc = { zoom = earth_r *. initial_zoom;
		screen_height = 500;
		screen_width = 500;
		screen_min = 500.0;
		screen_x_pre = 0.0;
		screen_y_pre = 0.0
	      }
  in let w = GWindow.window ~height:500 ~width:500 ()
  in let vbox = GPack.vbox ~packing:w#add ~homogeneous:false ()
  in let da = GMisc.drawing_area ~packing:(vbox#pack ~expand:true) ()
  in let hbox1 = GPack.hbox ~packing:(vbox#pack ~expand:false) ()
  in let _ = GMisc.label ~text:"Zoom:"
      ~packing:(hbox1#pack ~expand:false) ()
  in let zoomer = GData.adjustment ~value:initial_zoom ~lower:1.0
      ~upper:100000.0 ~step_incr:1.0 ~page_incr:50.0 ~page_size:1.0 ()
  in let bplay = GButton.button ~label:"Play" ~packing:hbox1#pack ()
  in let playing = ref false
  in
    ignore (zoomer#connect#value_changed
	      (fun () ->
		 spasc.zoom <- zoomer#value *. earth_r; refresh_da da));
    ignore (GEdit.spin_button ~adjustment:zoomer ~rate:0. ~digits:5 ~width:75 
	      ~packing:hbox1#pack ());
    hbox1#pack !status_line#coerce;
    da#misc#realize ();
    let q = Vmbridge.setup_file "/homes/icfp/hohmann-trace";
    in let d = new GDraw.drawable (da#misc#window)
    in let redraw_all _ =
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
	  paint_sat surface spasc !our_x !our_y;
	  d#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
	    ~width:da_width ~height:da_height pixmap#pixmap;
	  false
    in let remove_timeout = ref (fun () -> ())
    in let rec timeout_handler () =
	if !playing then begin
	  let stamp, score, fuel, x, y, muh = q.Vmbridge.step ();
	  in
	    our_x := x;
	    our_y := y;
	    ignore (redraw_all ());
	    install_timeout_handler ();
	end;
	 false
       and install_timeout_handler () =
	let delta = 5
	in let toid = GMain.Timeout.add delta timeout_handler
	in
	  remove_timeout := (fun () -> GMain.Timeout.remove toid)
       and start_playing () =
	if not !playing then begin
	  playing := true;
	  bplay#set_label "Stop";
	  ignore (install_timeout_handler ())
	end
       and stop_playing () =
	if !playing then begin
	  bplay#set_label "Play";
	  playing := false;
	  !remove_timeout ()
	end
    in
      ignore (da#event#connect#expose ~callback:redraw_all);
      ignore (bplay#connect#clicked ~callback:
		(function () ->
		   if !playing then stop_playing () else start_playing ()));
      w#show ();
      GMain.Main.main ()

let _ =
  ignore (GMain.init ());
  make_orbit_window ()
