:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).

colour(white).
colour(red).
colour(green).
colour(blue).
colour(black).

go :-

	new(P, auto_sized_picture('Table with merged cells')),
	%new(P, dialog),
	send(P, display, new(T, tabular)),
	send(T, border, 0),
	send(T, cell_spacing, 3),
	send(T, rules, all),
	send_list(T,
		[ 
			append('01       ', background := 'GreenYellow'),
			append('02       ', background := 'Yellow'),
			append('03       ', background := 'White'),
			append('04       ', background := 'Wheat'),
			append('05       ', background := 'BlueViolet'),
			append('06       ', background := 'Violet'),
			append('07       ', background := 'MediumTurquoise'),
			append('08       ', background := 'DarkTurquoise'),
			append('09       ', background := 'Turquoise'),
			append('10       ', background := 'Thistle'),
			append('11       ', background := 'Tan'),
			append('12       ', background := 'Sienna'),
			append('13       ', background := 'Salmon'),
			append('14       ', background := 'VioletRed'),
			append('15       ', background := 'OrangeRed'),
			append('16       ', background := 'MediumVioletRed'),
			append('17       ', background := 'IndianRed'),
			append('18       ', background := 'Red'),
			append('19       ', background := 'Plum'),
			append('20       ', background := 'Pink'),
			append('21       ', background := 'MediumOrchid'),
			append('22       ', background := 'DarkOrchid'),
			append('23       ', background := 'Orchid'),
			append('24       ', background := 'Orange'),
			append('25       ', background := 'Maroon'),
			append('26       ', background := 'Magenta'),
			append('27       ', background := 'Khaki'),
			append('28       ', background := 'Grey'),
			append('29       ', background := 'LightGray'),
			append('30       ', background := 'DimGray'),
			append('31       ', background := 'DarkSlateGray'),
			append('32       ', background := 'YellowGreen'),
			append('33       ', background := 'SpringGreen'),
			append('34       ', background := 'SeaGreen'),
			append('35       ', background := 'PaleGreen'),
			append('36       ', background := 'MediumSpringGreen'),
			append('37       ', background := 'MediumSeaGreen'),
			append('38       ', background := 'LimeGreen'),
			append('39       ', background := 'ForestGreen'),
			append('40       ', background := 'DarkOliveGreen'),
			append('41       ', background := 'DarkGreen'),
			append('42       ', background := 'Green'),
			append('43       ', background := 'Goldenrod'),
			append('44       ', background := 'Gold'),
			append('45       ', background := 'Brown'),
			append('46       ', background := 'Firebrick'),
			append('47       ', background := 'Cyan'),
			append('48       ', background := 'Coral'),
			append('49       ', background := 'SteelBlue'),
			append('50       ', background := 'SlateBlue'),
			append('51       ', background := 'SkyBlue'),
			append('52       ', background := 'Navy'),
			append('53       ', background := 'MidnightBlue'),
			append('54       ', background := 'MediumSlateBlue'),
			append('55       ', background := 'MediumBlue'),
			append('56       ', background := 'LightSteelBlue'),
			append('57       ', background := 'LightBlue'),
			append('58       ', background := 'DarkSlateBlue'),
			append('59       ', background := 'CornflowerBlue'),
			append('60       ', background := 'CadetBlue'),
			append('61       ', background := 'Blue'),
			append('62       ', background := 'Black'),
			append('63       ', background := 'MediumAquamarine'),
			append('64       ', background := 'Aquamarine')


		]),
	send(P, open).

		
