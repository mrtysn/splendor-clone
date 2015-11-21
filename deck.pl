%cards:[id,point,color,white#,blue#,green#,red#,black#]
%ID's correspond to images of the card, so you can use this to fetch the related card.
cards(deck1, [
[1,0,white,0,3,0,0,0],
[2,0,white,0,0,0,2,1],
[3,0,white,0,1,1,1,1],
[4,0,white,0,2,0,0,2],
[5,1,white,0,0,4,0,0],
[6,0,white,0,1,2,1,1],
[7,0,white,0,2,2,0,1],
[8,0,white,3,1,0,0,1],
[9,0,blue,1,0,0,0,2],
[10,0,blue,0,0,0,0,3],
[11,0,blue,1,0,1,1,1],
[12,0,blue,0,0,2,0,2],
[13,1,blue,0,0,0,4,0],
[14,0,blue,1,0,1,2,1],
[15,0,blue,1,0,2,2,0],
[16,0,blue,0,1,3,1,0],
[17,0,green,2,1,0,0,0],
[18,0,green,0,0,0,3,0],
[19,0,green,1,1,0,1,1],
[20,0,green,0,2,0,1,0],
[21,1,green,0,0,0,0,4],
[22,0,green,1,1,0,1,2],
[23,0,green,0,1,0,2,2],
[24,0,green,1,3,1,0,0],
[25,0,red,0,2,1,0,0],
[26,0,red,3,0,0,0,0],
[27,0,red,1,1,1,0,1],
[28,0,red,2,0,0,2,0],
[29,1,red,4,0,0,0,0],
[30,0,red,2,1,1,0,1],
[31,0,red,2,0,1,0,2],
[32,0,red,1,0,0,1,3],
[33,0,black,0,0,2,1,0],
[34,0,black,0,0,3,0,0],
[35,0,black,1,1,1,1,0],
[36,0,black,2,0,2,0,0],
[37,1,black,0,4,0,0,0],
[38,0,black,1,2,1,1,0],
[39,0,black,2,2,0,1,0],
[40,0,black,0,0,1,3,1]
]).

cards(deck2, [
[101,2,white,0,0,0,5,0],
[102,3,white,6,0,0,0,0],
[103,1,white,0,0,3,2,2],
[104,2,white,0,0,1,4,2],
[105,1,white,2,3,0,3,0],
[106,2,white,0,0,0,5,3],
[107,2,blue,0,5,0,0,0],
[108,3,blue,0,6,0,0,0],
[109,1,blue,0,2,2,3,0],
[110,2,blue,2,0,0,1,4],
[111,1,blue,0,2,3,0,3],
[112,2,blue,5,3,0,0,0],
[113,2,green,0,0,5,0,0],
[114,3,green,0,0,6,0,0],
[115,1,green,2,3,0,0,2],
[116,2,green,3,0,2,3,0],
[117,1,green,4,2,0,0,1],
[118,2,green,0,5,3,0,0],
[119,2,red,0,0,0,0,5],
[120,3,red,0,0,0,6,0],
[121,1,red,2,0,0,2,3],
[122,2,red,1,4,2,0,0],
[123,1,red,0,3,0,2,3],
[124,2,red,3,0,0,0,5],
[125,2,black,5,0,0,0,0],
[126,3,black,0,0,0,0,6],
[127,1,black,3,2,2,0,0],
[128,2,black,0,1,4,2,0],
[129,1,black,3,0,3,0,2],
[130,2,black,0,0,5,3,0]
]).

cards(deck3, [
[201,4,white,0,0,0,0,7],
[202,5,white,3,0,0,0,7],
[203,4,white,3,0,0,3,6],
[204,3,white,0,3,3,5,3],
[205,4,blue,7,0,0,0,0],
[206,5,blue,7,3,0,0,0],
[207,4,blue,6,3,0,0,3],
[208,3,blue,3,0,3,3,5],
[209,4,green,0,7,0,0,0],
[210,5,green,0,7,3,0,0],
[211,4,green,3,6,3,0,0],
[212,3,green,5,3,0,3,3],
[213,4,red,0,0,7,0,0],
[214,5,red,0,0,7,3,0],
[215,4,red,0,3,6,3,0],
[216,3,red,3,5,3,0,3],
[217,4,black,0,0,0,7,0],
[218,5,black,0,0,0,7,3],
[219,4,black,0,0,3,6,3],
[220,3,black,3,3,5,3,0]
]).

%nobles:[id,white#,blue#,green#,red#,black#], always 3 points
nobles(board, [
[501,3,3,0,0,3],
[502,0,3,3,3,0],
[503,3,0,0,3,3],
[504,0,0,4,4,0],
[505,0,4,4,0,0],
[506,0,0,0,4,4],
[507,4,0,0,0,4],
[508,3,3,3,0,0],
[509,0,0,3,3,3],
[510,4,4,0,0,0]
]).