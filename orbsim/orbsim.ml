(* orbsim: simulates the orbit
 *)

(* TODO:
   + ruler
*)

open GMain

(*
let diewoed = Cairo_png.image_surface_create_from_file "/tmp/erde.png"
*)

let earth_r = 6357000.0
let moon_r =  1736000.0 (* mycrometer genau! *)
let initial_zoom = 100.0
let initial_speed = 10
let pi = atan 1. *. 4.0
let two_pi = pi *. 2.0
let border = 1000.0

let rgb_white =   1.0, 1.0, 1.0
let rgb_red =     1.0, 0.0, 0.0
let rgb_green =   0.0, 1.0, 0.0
let rgb_blue =    0.0, 0.0, 1.0
let rgb_yellow =  1.0, 1.0, 0.0
let rgb_cyan =    0.0, 1.0, 1.0
let rgb_magenta = 1.0, 0.0, 1.0
let rgb_gray =    0.1, 0.1, 0.1
let rgb_black =   0.0, 0.0, 0.0

let our_x = ref 0.0
let our_y = ref 0.0
let our_orbits = ref []
let our_sats = ref []
let our_moons = ref []
let our_massband = ref None

let limit_x1 = ref (0.0 -. earth_r *. 2.0)
let limit_y1 = ref (0.0 -. earth_r *. 2.0)
let limit_x2 = ref (earth_r *. 2.0)
let limit_y2 = ref (earth_r *. 2.0)

let our_history :(float * float) list ref = ref []
let our_sats_histories :(float * float) list array =
  [| []; []; []; []; []; []; []; []; []; []; []; []; |]

(* 0,0 is always in middle of screen (coords of earth) *)
type space_screen = {
  mutable zoom :float; (* .. radius that shall fit into spaceview *)
  mutable speed :int; (* replay speed *)
  mutable screen_width :float;  (* set by user (gtk resize) *)
  mutable screen_height :float;
  mutable spaceview_x :float; (* center of spaceview .. *)
  mutable spaceview_y :float; (* .. modified by panning *)
  mutable spaceview_width :float;  (* size of view currently displayed .. *)
  mutable spaceview_height :float; (* .. modified by zoom or gtk resize *)
}

(* convert spaceview coord into screen coord
 *)
let ccx spasc coord =
  if coord < (!limit_x1 -. border) then (* store limits for scrollers *)
    limit_x1 := coord -. border;
  if coord > (!limit_x2 +. border) then
    limit_x2 := coord +. border;
  ((coord -. spasc.spaceview_x) *. (spasc.screen_width -. 1.0)) /.
    spasc.spaceview_width +. spasc.screen_width /. 2.0
let ccy spasc coord =
  if coord < (!limit_y1 -. border) then (* store limits for scrollers *)
    limit_y1 := coord -. border;
  if coord > (!limit_y2 +. border) then
    limit_y2 := coord +. border;
  ((coord -. spasc.spaceview_y) *. (spasc.screen_height -. 1.0)) /.
    spasc.spaceview_height +. spasc.screen_height /. 2.0

(* convert a value (radius, ..)
 *)
let vc spasc value =
  (value *. spasc.screen_height) /. spasc.spaceview_height

let vc' spasc value' =
  value' *. spasc.spaceview_height /. spasc.screen_height

let spasc_dump spasc =
  Printf.fprintf stderr "SPASC: zoom=%f, speed=%i, screen=%fx%f\n     sv_x=%f, sv_x=%f, sv=%fx%f\n"
    spasc.zoom spasc.speed spasc.screen_width spasc.screen_height
    spasc.spaceview_x spasc.spaceview_y
    spasc.spaceview_width spasc.spaceview_height;
  flush stderr

(* recalculates spaceview_height and witdh
 * required after: zooming, panning, window resize
 *)
let recalculate_spaceview spasc =
  (* window size is fixed, zoom ratio user defined so we need to calculate
   * spaceview_*
   * we can't use vc' since it depends on correct spaceview_* values
   * so we calculate that zoomer
   *)
  if spasc.screen_height > spasc.screen_width then begin
    spasc.spaceview_width <- 2.0 *. spasc.zoom;
    spasc.spaceview_height <-
      spasc.spaceview_width *. (spasc.screen_height /. spasc.screen_width);
  end else begin
    spasc.spaceview_height <- 2.0 *. spasc.zoom;
    spasc.spaceview_width <-
      spasc.spaceview_height *. (spasc.screen_width /. spasc.screen_height);
  end

(* called when drawing area is resized,
 * has to adjust the space_viewport because of aspect ratio 
 *)
let resize_screen spasc new_height new_width =
  spasc.screen_width <- float_of_int new_width;
  spasc.screen_height <- float_of_int new_height;
  recalculate_spaceview spasc

let surface_from_gdk_pixmap gdkpixmap =
  Cairo_lablgtk.create gdkpixmap

let set_color surface (r,g,b) =
  Cairo.set_source_rgb surface r g b

let paint_trace ?(color=rgb_cyan) surface points =
  let rec worker = function
    [] ->
      Cairo.stroke surface
    | (x,y) :: ps ->
	Cairo.line_to surface x y;
	worker ps
  in
    match points with
	[] -> ()
      | (x,y) :: ps ->
	  set_color surface color;
	  Cairo.move_to surface x y;
	  worker ps;
	  Cairo.stroke surface

let paint_line surface x1 y1 x2 y2 =
  Cairo.move_to surface x1 y1;
  Cairo.line_to surface x2 y2;
  Cairo.stroke surface

let paint_filled_circle surface x y r =
  Cairo.save surface;
  Cairo.new_path surface;
  Cairo.arc surface x  y r 0. two_pi;
  Cairo.fill surface;
  Cairo.restore surface;
  Cairo.stroke surface

let paint_circle surface x y r =
  Cairo.arc surface x y r 0. two_pi;
  Cairo.stroke surface

let show_trace ?(color=rgb_cyan) surface spasc points =
  paint_trace ~color surface
    (List.map (fun (x,y) -> (ccx spasc x), (ccy spasc y)) points)

let show_traces ?(color=rgb_cyan) surface spasc traces =
  Array.iter (fun points -> show_trace ~color surface spasc points) traces

let show_orbit ?(color=rgb_yellow) surface spasc r =
  set_color surface color;
  paint_circle surface (ccx spasc 0.0) (ccy spasc 0.0) (vc spasc r)

let show_orbits ?(color=rgb_yellow) surface spasc rs =
  List.iter (fun r -> show_orbit ~color surface spasc r) rs

let show_sat ?(r=3.0) ?(color=rgb_white) surface spasc x y =
  set_color surface color;
  paint_circle surface (ccx spasc x) (ccy spasc y) r

let show_sats ?(color=rgb_red) surface spasc sats =
  List.iter (fun (x, y) -> show_sat  ~r:4.5 ~color surface spasc x y) sats

let show_earth surface spasc =
  set_color surface rgb_green;
  paint_filled_circle surface (ccx spasc 0.0) (ccy spasc 0.0)
    (vc spasc earth_r)

let show_moon surface spasc (x, y) =
  set_color surface rgb_yellow;
  paint_filled_circle surface (ccx spasc x) (ccy spasc y)
    (vc spasc moon_r)

let show_massband surface spasc = function
    None -> ()
  | Some (x1, y1, x2, y2) ->
      set_color surface rgb_red;
      paint_line surface x1 y1 x2 y2;
      let f = sqrt ((x1-.x2)*.(x1-.x2) +. (y1-.y2)*.(y1-.y2))
      in let dist = vc' spasc f
      in let msg = Printf.sprintf "%fkm" (dist /. 1000.0)
      in let extent = Cairo.text_extents surface msg
      in let msg_len = extent.Cairo.text_width
      in let text_adder = (f -. msg_len) /. 2.0
      in let ang1 = (atan2 (y2 -. y1) (x2 -. x1));
      in let xtext, ytext, xadder, yadder =
	  if abs_float ang1 < (pi /. 2.0) then (* right side *)
	    (x1 +. (cos (ang1 +. 0.)) *. text_adder,
	     y1 +. (sin (ang1 +. 0.)) *. text_adder,
	     (sin (ang1 +. pi)) *. (0.0 -. 10.0),
	     (cos (ang1 +. 0.)) *. (0.0 -. 10.0))
	  else (* left side *)
	    (x2 -. (cos (ang1 +. 0.)) *. text_adder, 
	     y2 -. (sin (ang1 +. 0.)) *. text_adder,
	     (sin (ang1 +. pi)) *. (0.0 +. 10.0),
	     (cos (ang1 +. 0.)) *. (0.0 +. 10.0))
      in let ang = if ang1 < (0.0 -. pi /. 2.0) or ang1 > (pi /. 2.0)
	then ang1 +. pi else ang1
      in
	Cairo.move_to surface (xadder +. xtext) (yadder +. ytext);
	Cairo.rotate surface ~angle:ang;
	Cairo.show_text surface msg;
	Cairo.stroke surface

let create_space surface spasc =
  show_earth surface spasc

let dist_human_readable x1 y1 x2 y2 =
  match max (max (abs_float x1) (abs_float y1))
    (max (abs_float x2) (abs_float y2)) with
    | v when v < 1000.0 ->
	(x1, y1, x2, y2, "m")
    | v when v < 1000000.0 ->
	(x1 /. 1000.0, y1 /. 1000.0, x2 /. 1000.0, y2 /. 1000.0, "km")
    | v when v < 1000000000.0 ->
	(x1 /. 1000000.0, y1 /. 1000000.0,
	 x2 /. 1000000.0, y2 /. 1000000.0, "Mm")
    | v ->
	(x1 /. 1000000000.0, y1 /. 1000000000.0,
	 x2 /. 1000000000.0, y2 /. 1000000000.0, "Gm")

let refresh_da da =
  GtkBase.Widget.queue_draw da#as_widget

let status_line = ref (GMisc.label ~text:"Statusline" ~justify:`FILL ())

let update_status_line = (!status_line)#set_text

let make_orbit_window () =
  let spasc = { zoom = earth_r /. 20.0 *. initial_zoom;
		speed = initial_speed;
		screen_height = 500.0;
		screen_width = 500.0;
		spaceview_x = 0.0;
		spaceview_y = 0.0;
		spaceview_width = 2.0 *. initial_zoom;
		spaceview_height = 2.0 *. initial_zoom;
	      }
  in let w = GWindow.window ~height:500 ~width:500 ()
  in let vbox = GPack.vbox ~packing:w#add ~homogeneous:false ()
  in let table = GPack.table ~rows:4 ~columns:4 ~homogeneous:false
      ~packing:(vbox#pack ~expand:true) ()
  in let da = GMisc.drawing_area
      ~packing:(table#attach ~left:1 ~right:2 ~top:1 ~bottom:2 ~expand:`BOTH) ()
  in let unitlabel = GMisc.label ~text:"[]"
      ~packing:(table#attach ~left:0 ~right:1 ~top:0 ~bottom:1) ()
  in let xruler= GRange.ruler `HORIZONTAL ~metric:`PIXELS
      ~upper:1000.0 ~lower:0.0 ~max_size:100.0 ~show:true
      ~packing:(table#attach ~left:1 ~right:2 ~top:0 ~bottom:1 ~fill:`BOTH) ()
  in let yruler = GRange.ruler `VERTICAL ~metric:`PIXELS
      ~upper:1000.0 ~lower:0.0 ~max_size:100.0 ~show:true
      ~packing:(table#attach ~left:0 ~right:1 ~top:1 ~bottom:2 ~fill:`BOTH) ()
  in let hbox1 = GPack.hbox ~packing:(vbox#pack ~expand:false) ()
  in let hbox2 = GPack.hbox ~packing:(vbox#pack ~expand:false) ()
  in let bplay = GButton.button ~label:"Play" ~packing:hbox1#pack ()
  in let _ = GMisc.label ~text:"Zoom:"
      ~packing:(hbox1#pack ~expand:false) ()
  in let scrollx = GData.adjustment ~value:0.0
      ~lower:(0.0 -. initial_zoom *. earth_r) ~upper:(initial_zoom *. earth_r)
      ~step_incr:10.0 ~page_incr:earth_r ~page_size:1.0 ()
  in let scrolly = GData.adjustment ~value:0.0
      ~lower:(0.0 -. initial_zoom *. earth_r) ~upper:(initial_zoom *. earth_r)
      ~step_incr:1.0 ~page_incr:50.0 ~page_size:1.0 ()
  in let _ = GRange.scrollbar `HORIZONTAL ~adjustment:scrollx
      ~packing:(table#attach ~left:1 ~right:2 ~top:2 ~bottom:3 ~fill:`BOTH) ()
  in let _ = GRange.scrollbar `VERTICAL ~adjustment:scrolly
      ~packing:(table#attach ~left:2 ~right:3 ~top:1 ~bottom:2 ~fill:`BOTH) ()
  in let zoomer = GData.adjustment ~value:initial_zoom ~lower:1.0
      ~upper:100000.0 ~step_incr:1.0 ~page_incr:200.0 ~page_size:1.0 ()
  in let speeder = GData.adjustment ~value:(float_of_int initial_speed)
      ~lower:1.0 ~upper:10000.0 ~step_incr:1.0 ~page_incr:50.0 ~page_size:1.0 ()
  in let playing = ref false
  in let update_scollers () = (* updates scrollers to reflect spasc *)
      (*
      scrollerx#set_bounds ~lower:!limit_x1 ~upper:!limit_x2
	~step_incr:10.0 ~page_incr:50.0 ~page_size:spasc.spaceview_width ();
       scrollery#set_bounds ~lower:!limit_y1 ~upper:!limit_y2
	 ~step_incr:10.0 ~page_incr:50.0 ~page_size:spasc.spaceview_height ();
       scrollerx#set_value 0.0;
      *)
    (*
    xruler#set_lower !limit_x1;
    xruler#set_upper !limit_x2;
    *)
       (*
	 Printf.fprintf stderr "ruler y to lower=%f upper=%f\n"
	 spasc.spaceview_y1 (spasc.spaceview_y1 +. spasc.spaceview_height);
	 flush stderr;
       *)
      let svx1, svy1, svx2, svy2, units =
	dist_human_readable
	  (spasc.spaceview_x -. (spasc.spaceview_width /. 2.0))
	  (spasc.spaceview_y -. (spasc.spaceview_height /. 2.0))
	  (spasc.spaceview_x +. (spasc.spaceview_width /. 2.0))
	  (spasc.spaceview_y +. (spasc.spaceview_height /. 2.0))
      in
	yruler#set_lower svy1;
	yruler#set_upper svy2;
	xruler#set_lower svx1;
	xruler#set_upper svx2;
	unitlabel#set_text units
  in
    ignore (zoomer#connect#value_changed
	      (fun () ->
		 spasc.zoom <- zoomer#value *. earth_r /. 20.0;
		 recalculate_spaceview spasc;
		 refresh_da da));
    ignore (speeder#connect#value_changed
	      (fun () ->
		 spasc.speed <- int_of_float speeder#value));
    ignore (GEdit.spin_button ~adjustment:zoomer ~rate:0. ~digits:5 ~width:100 
	      ~packing:hbox1#pack ());
    ignore (GMisc.label ~text:"Speed:" ~packing:(hbox1#pack ~expand:false) ());
    ignore (GEdit.spin_button ~adjustment:speeder ~rate:0. ~digits:3 ~width:100 
	      ~packing:hbox1#pack ());
    hbox2#pack ~expand:false !status_line#coerce;
    ignore (GMisc.label ~text:"" ~packing:(hbox2#pack ~expand:true) ());
    da#misc#realize ();
    let q = if Array.length Sys.argv > 1 then
      Vmbridge.setup_file Sys.argv.(1)
    else
      failwith "biely mode not yet active.."
    in let d = new GDraw.drawable (da#misc#window)
    in let redraw_all _ =
      let da_width, da_height = Gdk.Drawable.get_size (da#misc#window)
      in let pixmap = GDraw.pixmap ~width:da_width ~height:da_height ()
      in
	resize_screen spasc da_width da_height;
	ignore (w#connect#destroy GMain.quit);
	pixmap#set_foreground (`NAME "darkgray");
	pixmap#rectangle ~x:0 ~y:0 ~width:da_width ~height:da_height
	  ~filled:true ();
	let surface = (surface_from_gdk_pixmap pixmap#pixmap)
	in
	  show_earth surface spasc;
	  if !our_moons <> [] then
	    show_moon surface spasc (List.hd !our_moons);
	  show_orbits surface spasc !our_orbits;
	  show_sats surface spasc ~color:rgb_cyan !our_sats;
	  show_sat surface spasc !our_x !our_y;
	  show_trace surface ~color:rgb_white spasc !our_history;
	  show_traces surface spasc our_sats_histories;
	  show_massband surface spasc !our_massband;
	  (*
	  Cairo.set_source_surface surface diewoed 10.0 100.0;
	  Cairo.paint surface;
	  *)
	  d#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
	    ~width:da_width ~height:da_height pixmap#pixmap;
	  false
    in let remove_timeout = ref (fun () -> ())
    in let rec timeout_handler () =
	if !playing then begin
	  let stamp, score, fuel, x, y,
	    orbits, sats, moons, fusts, debugs, rem =
	    q.Vmbridge.step spasc.speed;
	  in let rec record_more_traces ?(i=0) = function
		[] -> ()
	    | (x, y) :: r ->
		let old_x, old_y =
		  try
		    List.hd our_sats_histories.(i)
		  with
		      _ -> x +. 1.0, y
		in
		  if ((int_of_float old_x) <> (int_of_float x)) or
		  ((int_of_float old_y) <> (int_of_float y)) then begin
		    if (old_x <> 0.0) && (old_y <> 0.0) then
		      our_sats_histories.(i) <-
			(x, y) :: our_sats_histories.(i)
		  end;
		  record_more_traces ~i:(i+1) r
	  in
	    if ((int_of_float !our_x) <> (int_of_float x)) or
	      ((int_of_float !our_y) <> (int_of_float y)) then begin
		if (!our_x <> 0.0) && (!our_y <> 0.0) then
		  our_history := (!our_x, !our_y) :: !our_history;
	      end;
	    record_more_traces sats;
	    update_status_line
	      (Printf.sprintf "[%i] Score=%f Fuel=%f x=%f y=%f | %s"
		 stamp score fuel x y rem);
	    our_x := x;
	    our_y := y;
	    our_orbits := orbits;
	    our_sats := sats;
	    our_moons := moons;
	    ignore (redraw_all ());
	    install_timeout_handler ();
	end;
	 false
       and install_timeout_handler () =
	let delta = 25
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
    in let left_pressed = ref false
       and right_pressed = ref false
       and mouse_coords = ref (0.0, 0.0)
       and massband_start = ref (0.0, 0.0)
    in let mbutton_callback ev =
	match GdkEvent.get_type ev with
	    `BUTTON_PRESS when GdkEvent.Button.button ev = 1 ->
	      let mx, my = GdkEvent.Button.x ev, GdkEvent.Button.y ev
	      in
		mouse_coords := mx, my;
		left_pressed := true;
		true
	  | `BUTTON_PRESS when GdkEvent.Button.button ev = 3 ->
	      let mx, my = GdkEvent.Button.x ev, GdkEvent.Button.y ev
	      in
		massband_start := mx, my;
		right_pressed := true;
		true;
	  | `BUTTON_RELEASE when GdkEvent.Button.button ev = 1 ->
	      left_pressed := false;
	      true
	  | `BUTTON_RELEASE when GdkEvent.Button.button ev = 3 ->
	      right_pressed := false;
	      our_massband := None;
	      refresh_da da;
	      true
	  | _ ->
	      false
       and mmove_callback ev =
	let mx = GdkEvent.Motion.x ev
	and my = GdkEvent.Motion.y ev
	in
	  ignore (xruler#event#send (ev :> GdkEvent.any));
	  ignore (yruler#event#send (ev :> GdkEvent.any));
	  if !left_pressed then begin
	    let oldx, oldy = !mouse_coords
	    in
	      spasc.spaceview_x <-
		spasc.spaceview_x +. (oldx -. mx) *.
		(spasc.zoom /. spasc.screen_width);
	      spasc.spaceview_y <-
		    spasc.spaceview_y +. (oldy -. my) *.
		(spasc.zoom /. spasc.screen_height);
	      mouse_coords := mx, my;
	      recalculate_spaceview spasc;
	      update_scollers ();
	      refresh_da da
	  end;
	  if !right_pressed then begin
	    let sx, sy = !massband_start
	    in
	      our_massband := Some (sx, sy, mx, my);
	      refresh_da da
	  end;
	  false
       and scroll_callback ev =
	match GdkEvent.get_type ev with
	  | `SCROLL ->
	      if GdkEvent.Scroll.direction ev = `UP then begin
		if zoomer#value <= 20.0 then
		  zoomer#set_value (zoomer#value +. 3.0)
		else
		  zoomer#set_value (zoomer#value +. 20.0);
		recalculate_spaceview spasc;
		refresh_da da
	      end;
	      if GdkEvent.Scroll.direction ev = `DOWN then begin
		if zoomer#value <= 20.0 then 
		  zoomer#set_value (zoomer#value -. 3.0)
		else
		  zoomer#set_value (zoomer#value -. 20.0);
		recalculate_spaceview spasc;
		refresh_da da
	      end;
	      true
    in
      ignore (da#event#connect#expose ~callback:redraw_all);
      ignore (da#event#connect#button_press mbutton_callback);
      ignore (da#event#connect#button_release mbutton_callback);
      ignore (da#event#connect#scroll scroll_callback);
      ignore (da#event#connect#motion_notify mmove_callback);
      da#event#add [`BUTTON_PRESS; `BUTTON_RELEASE; `BUTTON_MOTION;
		    `POINTER_MOTION; `POINTER_MOTION_HINT];
      ignore (bplay#connect#clicked ~callback:
		(function () ->
		   if !playing then stop_playing () else start_playing ()));
      w#show ();
      GMain.Main.main ()

let _ =
  ignore (GMain.init ());
  make_orbit_window ()
