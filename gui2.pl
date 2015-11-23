:- use_module(library(pce)).
:- use_module(library(scaledbitmap)).


go :-
	free(@main),
	free(@myBitmap),
	new(@main, dialog('')),
	new(@myBitmap, bitmap('./lowres/1.jpg')),
	send(@myBitmap, size, size(350, 350)),
	%send(@myBitmap, layout),
	send(@main, append, @myBitmap),
	%resize(@myBitmap, 100, 100),
	send(@main, open),
	true.

resize(Bitmap, W, H) :-
	get(Bitmap, file, File),
	new(I2, image(@nil, W, H)),
	send(I2, draw_in, Bitmap, point(0,0)),
	send(Bitmap, image, I2).


test :-
  new(@sd,dialog('Dialog Test')),
  send(@sd,append,button(quit,message(@prolog,doquit))),
  send(@sd,append,new(@pic,picture(box))),
  send(@pic,width(450)),
  send(@pic,height(130)),
  send(@sd, open).

redrawpic :-    
  get(@sd,width,W),
  W1 is W - 30,
  get(@pic,height,H),
  H1 is H + 6,
  send(@pic,free),
  send(@sd,append,new(@pic,picture(box,size(W1,H1)))),
  %send(new(P, picture), below, @sd),
  send(@sd,layout).


 mert :-
	(free(@frame);true),
	(free(@testBitmap);true),
	new(@frame, dialog('Frame Title')),

	new(@testBitmap, scaled_bitmap(image('./resources/206.jpg'))),
	X = 'frame',
	send(@X, append, @testBitmap),
	send(@X, open).

mert2 :-
	send(@testBitmap, scale, size(350,350)).