function(eO){"use strict";var
aE=".",bK="  [",bG=" : ",bR=915186972,A=246,bF=141,bJ=3901498,bP=121,aF="congruence",bQ="[",bO=-431191102,ao=104,bE="A",an=120,aV=1000,bT="with",aH="]",bN=113,aG=15500,bI=-318868643,bM=" and ",bH=888453194,aW=-912009552,bS="Heq",bD="f_equal",bL=100,V=250,y=eO.jsoo_runtime,q=y.caml_check_bound,al=y.caml_int_compare,aB=y.caml_make_vect,d=y.caml_new_string,U=y.caml_obj_tag,ak=y.caml_register_global,r=y.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):y.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):y.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):y.caml_call_gen(a,[b,c,d])}function
am(a,b,c,d,e){return a.length==4?a(b,c,d,e):y.caml_call_gen(a,[b,c,d,e])}function
aC(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):y.caml_call_gen(a,[b,c,d,e,f])}function
aD(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):y.caml_call_gen(a,[b,c,d,e,f,g,h])}function
eN(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):y.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
g=y.caml_get_global_data(),ay=d("cc_plugin"),n=g.Constr,m=g.Names,_=g.Hashset,aY=g.Sorts,i=g.Util,t=g.Not_found,a8=g.Term,e=g.EConstr,Q=g.Typing,f=g.Int,w=g.Stdlib__queue,c=g.Pp,aO=g.Control,F=g.CErrors,bc=g.Environ,L=g.Context,ba=g.Assert_failure,ag=g.Printer,o=g.Tacmach,ac=g.Stdlib__hashtbl,ae=g.Stdlib,P=g.Termops,T=g.CamlinternalLazy,p=g.Tacticals,j=g.Proofview,H=g.Tactics,bw=g.Evarsolve,bz=g.DAst,O=g.Coqlib,bu=g.Equality,bq=g.CClosure,br=g.Reductionops,az=g.Stdarg,aA=g.Genarg,bC=g.Ltac_plugin__Tacentries,dd=g.Vars,db=g.Namegen,bU=g.Feedback,bZ=g.Goptions,dO=g.Global,dP=g.Inductiveops,dQ=g.Retyping,dV=g.Evarutil,eq=g.Pretype_errors,er=g.Type_errors,ec=g.Detyping,dZ=g.Evd,dW=g.Refine,es=g.Mltop;ak(an,[0,0,0,0,0],"Cc_plugin");var
aI=[0,0],dj=d("Out of depth ... "),di=d("Out of instances ... "),dk=d("First run was incomplete, completing ... "),dh=d("Executing ... "),dg=d("Running E-matching algorithm ... "),df=d("paf_of_patt: pattern is trivial"),dc=d("wrong incomplete class."),c7=d(" ... "),c8=d(" = "),c9=d("Checking if "),c6=d("Yes"),c_=d("No"),c3=d(aE),c4=d("Processing mark for term "),c0=d("weird error in injection subterms merge."),c1=[0,d("add_pacs")],cY=d(aE),cZ=d("Updating term "),cV=d(aE),cW=d(bM),cX=d("Merging "),cR=d(aE),cS=d(bM),cT=d("Linking "),cQ=[0,d("_vendor+v8.10+32bit/coq/plugins/cc/ccalgo.ml"),635,2],cK=d(aH),cL=d(" <> "),cM=d(bG),cN=d(bK),cO=d("Adding new disequality, depth="),cF=d(aH),cG=d(" == "),cH=d(bG),cI=d(bK),cJ=d("Adding new equality, depth="),cE=d("discarding redundant (dis)equality"),cB=d(aH),cC=d(bQ),cy=d(aH),cz=d(":="),cA=d(bQ),cx=d("incomplete matching."),cm=d("not a node."),cn=[0,d("subterms")],ce=d("not a constructor."),cf=[0,d("get_constructor")],cc=d("not a representative."),cd=[0,d("get_representative")],b5=d("signature already entered."),b6=[0,d("enter")],bW=[0,d("Congruence"),[0,d("Verbose"),0]],bX=d("Congruence Verbose"),ck=d("Cc_plugin.Ccalgo.Discriminable"),cp=d(bE),cr=d(bE),c$=d("_eps_"),dl=d("invalid cc transitivity."),dm=d("not enough args."),dn=[0,d("nth_arg")],dp=[0,1,20],dq=d("equal_proof "),dr=[0,1,20],ds=d("edge_proof "),dt=[0,1,20],du=d("constr_proof "),dw=d(","),dv=d("}"),dx=d("{"),dy=[0,1,20],dz=d("path_proof "),dA=[0,1,20],dB=d("congr_proof "),dC=[0,1,20],dD=d("ind_proof "),dN=[0,0],dU=[0,d("_vendor+v8.10+32bit/coq/plugins/cc/cctac.ml"),251,9],d0=d("f"),d1=d("I don't know how to handle dependent equality"),eo=[0,0],ef=d("("),eg=d(")"),eb=[0,1],d$=d("Goal solved, generating proof ..."),d_=d("Computation completed."),d9=d("Problem built, solving ..."),d8=d("Reading subgoal ..."),ea=[13,0,0,0],ed=d("  replacing metavariables by arbitrary terms."),ee=d(')",'),eh=d('"congruence with ('),ei=d("  Try "),ej=d("Goal is solvable by congruence but some arguments are missing."),ek=d("congruence failed"),d7=d(bS),d6=d("H"),d4=d("e"),d5=d("X"),d2=d(bS),dX=[0,0],dY=[0,1],dT=d("t"),dM=d("core.False.type"),dL=d("core.eq.type"),dK=d("core.eq.trans"),dI=d("core.eq.sym"),dH=d("core.eq.refl"),dF=d("core.eq.rect"),dE=d("core.eq.congr"),el=d("congruence failed."),ev=d(bT),ex=d(aF),eA=d(bT),eB=d(aF),eE=d(aF),eG=[0,d(aF),0],eI=d("cc"),eK=[0,d(bD),0],eM=d(bD),W=5;function
s(d){var
c=aI[1];if(c){var
e=a(d,0);return b(bU[9],0,e)}return c}function
bV(a){aI[1]=a;return 0}var
bY=[0,0,bX,bW,function(a){return aI[1]},bV];b(bZ[4],0,bY);var
b0=f[1],b1=[0,function(b,a){return b===a?1:0},b0],ap=a(ac[25],b1);function
b2(b,a){var
c=b[1]===a[1]?1:0,d=a[2],e=b[2],f=c?e===d?1:0:c;return f}var
b3=[0,b2,function(c){var
d=c[1],e=a(f[1],c[2]),g=a(f[1],d);return b(_[2][1],g,e)}],ad=a(ac[25],b3);function
b4(f,e,d){if(b(ad[11],d[1],e)){var
g=a(c[3],b5);h(F[3],0,b6,g)}else
h(ad[10],d[1],e,f);return h(ap[10],d[2],f,e)}function
b7(c,a){return b(ad[7],a[1],c)}function
b8(a,c){try{var
d=b(ap[7],a[2],c);b(ad[6],a[1],d);var
e=b(ap[6],a[2],c);return e}catch(a){a=r(a);if(a===t)return 0;throw a}}function
b9(c,a){function
d(a){return b8(c,a)}return b(f[2][13],d,a)}var
b_=[0,function(b,a){var
c=al(b[1],a[1]),e=a[3],f=a[2],g=b[3],j=b[2];if(0===c){var
d=al(j,f);return 0===d?h(i[17][46],al,g,e):d}return c}],b$=[0,function(b,a){var
c=al(b[1],a[1]),d=a[2],e=b[2];return 0===c?al(e,d):c}],l=a(i[21][1],b_),k=a(i[21][1],b$);function
aX(c,a){if(typeof
c==="number")switch(c){case
1:var
b=typeof
a==="number"?1===a?1:0:0;break;case
2:var
b=typeof
a==="number"?2<=a?1:0:0;break;default:var
b=0}else
var
b=typeof
a==="number"?0:1;return b?1:0}function
aq(l,k){var
c=l,a=k;for(;;){switch(c[0]){case
0:if(0===a[0])return b(n[85],c[1],a[1]);break;case
1:if(1===a[0]){var
o=a[2],p=c[2],f=aX(c[1],a[1]);return f?aX(p,o):f}break;case
2:if(2===a[0])return b(m[1][1],c[1],a[1]);break;case
3:if(3===a[0]){var
q=a[2],r=c[2],g=aq(c[1],a[1]);if(g){var
c=r,a=q;continue}return g}break;default:if(4===a[0]){var
d=a[1],e=c[1],h=e[2]===d[2]?1:0,s=d[3],t=d[1][1],u=e[3],v=e[1][1];if(h){var
i=u===s?1:0;if(i)return b(m[46],v,t);var
j=i}else
var
j=h;return j}}return 0}}function
aJ(c){switch(c[0]){case
0:var
e=a(n[bN],c[1]);return b(_[2][1],1,e);case
1:var
f=c[1],g=a(aY[7],c[2]),i=a(aY[7],f);return h(_[2][3],2,i,g);case
2:var
j=a(m[1][3],c[1]);return b(_[2][1],3,j);case
3:var
k=c[1],l=aJ(c[2]),o=aJ(k);return h(_[2][3],4,o,l);default:var
d=c[1],p=d[3],q=d[2],r=a(m[50],d[1][1]);return am(_[2][4],5,r,q,p)}}var
C=a(ac[25],[0,n[85],n[bN]]),X=a(ac[25],[0,aq,aJ]),aK=a(ac[25],[0,m[1][1],m[1][3]]),ca=[0,a(n[1],ae[9])],aZ=[0,[1,ae[9],[0,ae[9],ae[9],0]],ae[9],l[1],0,ca];function
a0(c){var
b=a(X[1],W);return[0,W,0,aB(5,aZ),a(C[1],W),0,b]}function
a1(e,b){var
g=a(o[2],b),h=a(o[5],b),i=a(C[1],W),j=a(aK[1],W),k=f[2][1],l=a(w[2],0),m=a(w[2],0),n=f[2][1],c=a(ap[1],W),d=[0,a(ad[1],W),c];return[0,a0(0),d,n,m,l,0,0,k,j,e,0,i,h,g]}function
cb(a){return a[1]}function
z(e,g){var
c=0,a=g;for(;;){var
d=q(e[3],a)[1+a][2];if(0<=d){var
c=[0,a,c],a=d;continue}var
f=function(b){q(e[3],b)[1+b][2]=a;return 0};b(i[17][11],f,c);return a}}function
K(e,b){var
d=q(e[3],b)[1+b][1];if(0===d[0])return d[1];var
f=a(c[3],cc);return h(F[3],0,cd,f)}function
a2(b,a){return q(b[3],a)[1+a][3]}function
a3(c,f,e){var
a=f;for(;;)try{var
g=a2(c,a),h=b(l[23],e,g);return h}catch(b){b=r(b);if(b===t){var
d=q(c[3],a)[1+a][1];if(0===d[0])throw t;var
a=d[1];continue}throw b}}function
af(e,b){var
d=q(e[3],b)[1+b][5];if(4===d[0])return d[1];var
f=a(c[3],ce);return h(F[3],0,cf,f)}function
a4(b,a){return K(b,a)[1]}function
cg(a){return a[4]}function
ch(a){return a[5]}function
ci(e,d,c){var
a=K(e,d);a[1]=a[1]+1|0;a[2]=b(f[2][4],c,a[2]);a[3]=b(f[2][4],c,a[3]);return 0}function
cj(e,d,c){var
a=K(e,d);a[1]=a[1]+1|0;a[3]=b(f[2][4],c,a[3]);return 0}var
a5=[248,ck,y.caml_fresh_oo_id(0)];function
a6(b){var
c=a(i[17][6],b[3]);return[0,b[1],b[2]+1|0,c]}function
cl(a,c,e){try{var
j=b(k[23],c,a[6]),d=j}catch(a){a=r(a);if(a!==t)throw a;var
d=f[2][1]}var
g=a[6],i=b(f[2][4],e,d);a[6]=h(k[4],c,i,g);return 0}function
D(b,a){return q(b[3],a)[1+a][5]}function
$(f,b){var
d=q(f[3],b)[1+b][4];if(d){var
e=d[1];return[0,e[1],e[2]]}var
g=a(c[3],cm);return h(F[3],0,cn,g)}function
a7(a,c){var
b=$(a,c),d=b[1],e=z(a,b[2]);return[0,z(a,d),e]}function
co(a){var
b=a[2],c=b+1|0;if(c===a[1]){var
d=((a[1]*3|0)/2|0)+1|0,e=aB(d,aZ);a[1]=d;aC(i[19][10],a[3],0,e,0,b);a[3]=e}a[2]=c;return b}function
ar(a){return[0,0,f[2][1],f[2][1],0,a,k[1]]}var
cq=[0,a(m[1][6],cp)],cs=[0,a(m[1][6],cr)],ct=a(n[1],2),cu=a(n[1],2),cv=[0,b(L[4],0,0),cu,ct],cw=a(n[12],cv);function
G(c){switch(c[0]){case
0:return c[1];case
1:var
k=c[1],f=a(n[6],c[2]),g=[0,b(L[4],cs,0),f,cw],h=a(n[13],g),i=a(n[6],k),j=[0,b(L[4],cq,0),i,h];return a(n[13],j);case
2:return a(n[2],c[1]);case
3:var
l=c[1],e=[0,G(c[2]),0],d=l;for(;;){if(3===d[0]){var
o=d[1],e=[0,G(d[2]),e],d=o;continue}var
m=[0,G(d),e];return a(a8[12],m)}default:return a(n[23],c[1][1])}}function
a9(q,p){var
f=a(e[bF][1],p);function
d(b){return a9(q,a(e[9],b))}var
c=a(n[29],f);switch(c[0]){case
6:var
r=c[2],s=c[1],t=d(c[3]),u=[0,s,d(r),t];return a(n[12],u);case
7:var
v=c[2],w=c[1],x=d(c[3]),y=[0,w,d(v),x];return a(n[13],y);case
8:var
z=c[3],A=c[2],B=c[1],C=d(c[4]),D=d(z),E=[0,B,d(A),D,C];return a(n[14],E);case
9:var
F=c[1],G=b(i[19][73][1],d,c[2]),H=[0,d(F),G];return a(n[15],H);case
10:var
g=c[1],I=g[2],J=a(m[17][5],g[1]),K=[0,a(m[17][2],J),I];return a(n[18],K);case
11:var
h=c[1],j=h[1],L=h[2],M=j[2],N=a(m[23][5],j[1]),O=[0,[0,a(m[23][2],N),M],L];return a(n[21],O);case
12:var
k=c[1],l=k[1],o=l[1],P=k[2],Q=l[2],R=o[2],S=a(m[23][5],o[1]),T=[0,[0,[0,a(m[23][2],S),R],Q],P];return a(n[23],T);case
16:var
U=c[2],V=c[1],W=function(b){var
c=a(m[23][5],b);return a(m[23][2],c)},X=b(m[62][19],W,V),Y=[0,X,d(U)];return a(n[19],Y);default:return f}}function
aL(b,a){if(0===a[0]){var
d=a[2],e=a[1],f=function(c,a){return[3,a,aL(b,c)]};return h(i[17][16],f,d,e)}var
c=a[1]-1|0;return q(b,c)[1+c]}function
v(i,g,f,d){var
j=a(c[3],cy),k=G(D(f,d)),l=a(e[9],k),m=h(ag[11],i,g,l),n=a(c[3],cz),o=a(c[16],d),p=a(c[3],cA),q=b(c[12],p,o),r=b(c[12],q,n),s=b(c[12],r,m);return b(c[12],s,j)}function
as(g,f,d){var
i=a(c[3],cB),j=G(d),k=a(e[9],j),l=h(ag[11],g,f,k),m=a(c[3],cC),n=b(c[12],m,l);return b(c[12],n,i)}function
M(d,g){var
i=d[1];try{var
k=b(X[7],i[6],g);return k}catch(k){k=r(k);if(k===t){var
c=co(i),s=G(g),u=a(e[9],s),v=h(Q[1],d[13],d[14],u),j=a9(d[14],v);switch(g[0]){case
2:var
B=l[1],m=[0,[0,ar(j)],-1,B,0,g];break;case
3:var
D=g[2],o=M(d,g[1]),p=M(d,D);ci(i,z(i,o),c);cj(i,z(i,p),c);d[3]=b(f[2][4],c,d[3]);var
E=l[1],m=[0,[0,ar(j)],-1,E,[0,[0,o,p]],g];break;case
4:var
F=g[1];b(w[3],[0,c,[0,[0,c,0]]],d[5]);b(w[3],[0,c,[1,[0,c,F[2],0]]],d[5]);var
H=l[1],m=[0,[0,ar(j)],-1,H,0,g];break;default:b(w[3],[0,c,[0,[0,c,0]]],d[5]);var
x=l[1],m=[0,[0,ar(j)],-1,x,0,g]}q(i[3],c)[1+c]=m;h(X[5],i[6],g,c);try{var
A=b(C[7],d[12],j),n=A}catch(a){a=r(a);if(a!==t)throw a;var
n=f[2][1]}var
y=b(f[2][4],c,n);h(C[10],d[12],j,y);return c}throw k}}function
aM(a,e,d,c){var
f=M(a,d),g=M(a,c);b(w[3],[0,f,g,[0,e,0]],a[4]);return h(C[5],a[1][4],e,[0,d,c])}function
Y(a,d,c,b){var
e=M(a,c),f=M(a,b);a[6]=[0,[0,e,f,d],a[6]];return 0}function
aN(b,d,c,a){b[7]=[0,[0,d,c,a[1],a[3],a[2],a[5],a[4]],b[7]];return 0}function
cD(a,d,c){try{var
e=a[1],f=function(a){return z(e,a)},g=b(i[19][15],f,c),j=b(aK[9],a[9],d),k=function(b){function
c(c,b){return c===z(a[1],b)?1:0}return h(i[19][37],c,g,b)},l=b(i[17][22],k,j);return l}catch(a){a=r(a);if(a===t)return 0;throw a}}function
cP(e,b,a,d){var
c=q(e[3],b)[1+b];c[1]=[1,a,d];c[2]=a;return 0}function
a_(g,f,e){var
a=f,b=e;for(;;){var
c=q(g[3],a)[1+a][1];if(0===c[0])return b;var
d=c[1],h=[0,[0,[0,a,d],c[2]],b],a=d,b=h;continue}}function
a$(c,i,h){var
o=z(c,h);if(z(c,i)===o){var
p=a_(c,h,0),a=[0,a_(c,i,0),p];for(;;){var
b=a[1];if(b){var
d=a[2];if(d){var
f=d[1][1],g=b[1][1],e=g[1]===f[1]?1:0,m=d[2],n=b[2],j=f[2],k=g[2],l=e?k===j?1:0:e;if(l){var
a=[0,n,m];continue}return a}return[0,b,0]}return[0,0,a[2]]}}throw[0,ba,cQ]}function
bb(d,i,m,y){s(function(o){var
e=a(c[3],cR),f=v(d[13],d[14],d[1],m),g=a(c[3],cS),h=v(d[13],d[14],d[1],i),j=a(c[3],cT),k=b(c[12],j,h),l=b(c[12],k,g),n=b(c[12],l,f);return b(c[12],n,e)});var
j=K(d[1],i),e=K(d[1],m);cP(d[1],i,m,y);try{var
I=b(C[7],d[12],j[5]),p=I}catch(a){a=r(a);if(a!==t)throw a;var
p=f[2][1]}var
z=b(f[2][6],i,p);h(C[10],d[12],j[5],z);var
u=b(f[2][7],j[3],e[3]);e[1]=a(f[2][20],u);e[3]=u;e[2]=b(f[2][7],j[2],e[2]);b9(d[2],j[3]);d[3]=b(f[2][7],d[3],j[3]);var
A=q(d[1][3],i)[1+i][3];function
B(c,a){return b(w[3],[0,a,[1,c]],d[5])}b(l[11],B,A);var
D=j[6];function
E(c){function
e(a){return b(w[3],[0,a,[0,c]],d[5])}return a(f[2][13],e)}b(k[11],E,D);var
n=j[4],g=e[4];if(typeof
n==="number"){if(0===n)return 0;if(typeof
g==="number"){if(0===g){e[4]=1;return 0}}else
if(0===g[0]){d[8]=b(f[2][6],m,d[8]);e[4]=1;return 0}}else
if(0===n[0]){var
F=n[1];if(typeof
g==="number"){if(0===g){e[4]=[0,F];d[8]=b(f[2][6],i,d[8]);d[8]=b(f[2][4],m,d[8]);return 0}var
x=0}else
var
x=1===g[0]?1:0;if(!x){d[8]=b(f[2][6],i,d[8]);return 0}}else{var
o=n[1],G=o[2],H=o[1];if(typeof
g==="number"){if(0===g){e[4]=[1,o];return 0}}else
if(0!==g[0])return b(w[3],[0,H,[1,G]],d[5])}return 0}function
cU(f,e){s(function(n){var
d=a(c[3],cV),g=v(e[13],e[14],e[1],f[2]),h=a(c[3],cW),i=v(e[13],e[14],e[1],f[1]),j=a(c[3],cX),k=b(c[12],j,i),l=b(c[12],k,h),m=b(c[12],l,g);return b(c[12],m,d)});var
g=e[1],h=z(g,f[1]),i=z(g,f[2]),j=1-(h===i?1:0);if(j){var
l=a4(g,i);if(a4(g,h)<l)return bb(e,h,i,f);var
d=f[3],k=typeof
d==="number"?0:0===d[0]?[0,d[1],1-d[2]]:[1,d[3],d[4],d[1],d[2],d[5]];return bb(e,i,h,[0,f[2],f[1],k])}return j}function
c2(g,t,d){s(function(j){var
e=a(c[3],c3),f=v(d[13],d[14],d[1],g),h=a(c[3],c4),i=b(c[12],h,f);return b(c[12],i,e)});var
p=z(d[1],g),i=K(d[1],p);if(0===t[0]){cl(i,t[1],g);d[3]=b(f[2][7],i[2],d[3]);return 0}var
e=t[1],r=q(d[1][3],p)[1+p];if(1-b(l[3],e,r[3]))r[3]=h(l[4],e,g,r[3]);var
j=i[4];if(typeof
j==="number"){if(0===j)return 0===e[2]?(i[4]=[1,[0,g,e]],0):(d[3]=b(f[2][7],i[2],d[3]),i[4]=[0,e],d[8]=b(f[2][4],p,d[8]),0)}else
if(1===j[0]){var
u=j[1],k=u[2],x=u[1];if(e[1]===k[1]){var
A=af(d[1],e[1]),o=A[3],n=k[3],m=e[3];for(;;){var
y=0<o?1:0;if(y){if(n)if(m){var
B=m[2],C=n[2];b(w[3],[0,n[1],m[1],[1,x,k,g,e,o]],d[4]);var
o=o-1|0,n=C,m=B;continue}var
D=a(c[3],c0);return h(F[3],0,c1,D)}return y}}throw[0,a5,x,k,g,e]}d[3]=b(f[2][7],i[2],d[3]);return 0}function
c5(d){var
g=d[1];function
h(f){if(f){var
e=f[1],k=f[2],l=z(g,e[2]);if(z(g,e[1])===l)var
j=a(c[3],c6),i=[0,e];else
var
m=h(k),j=a(c[3],c_),i=m;s(function(p){var
f=a(c[3],c7),g=v(d[13],d[14],d[1],e[2]),h=a(c[3],c8),i=v(d[13],d[14],d[1],e[1]),k=a(c[3],c9),l=b(c[12],k,i),m=b(c[12],l,h),n=b(c[12],m,g),o=b(c[12],n,f);return b(c[12],o,j)});return i}return 0}return h(d[6])}var
da=a(m[1][6],c$);function
de(d){var
g=d[8];function
j(p){var
j=K(d[1],p)[4];if(typeof
j!=="number"&&0===j[0]){var
f=j[1],y=G(D(d[1],f[1])),z=a(e[9],y),A=h(Q[1],d[13],d[14],z),B=a(e[bF][1],A),C=f[3],E=function(a){return G(D(d[1],a))},H=b(i[17][68],E,C),I=a(i[17][9],H),J=b(a8[29],B,I),N=f[2],l=D(d[1],p),m=J,k=N;for(;;){if(0<k){var
o=a(n[65],m),v=o[3],w=a(e[9],o[2]),q=a(bc[13],d[13]),r=a(bc[38],q),g=b(db[26],da,r),s=d[13],t=[0,b(L[4],g,0),w];d[13]=b(e[124],t,s);var
x=[0,a(n[2],g),0],l=[3,l,[2,g]],m=b(dd[13],x,v),k=k-1|0;continue}d[1][5]=[0,f,d[1][5]];M(d,l);return 0}}var
u=a(c[3],dc);return h(F[3],0,0,u)}return b(f[2][13],j,g)}function
bd(c){var
a=[0,k[1]],g=c[1],d=c[1][3];function
e(c,i){var
d=c<g[2]?1:0;if(d){var
e=i[1];if(0===e[0]){var
j=e[1][6],l=function(d,l){try{var
j=b(k[23],d,a[1]),e=j}catch(a){a=r(a);if(a!==t)throw a;var
e=f[2][1]}var
g=a[1],i=b(f[2][4],c,e);a[1]=h(k[4],d,i,g);return 0};return b(k[11],l,j)}return 0}return d}b(i[19][14],e,d);return a[1]}function
be(s,p,d){var
c=a(i[22][9],d),m=c[3];if(m){var
e=m[2],u=m[1],g=u[2],h=u[1],j=s[1];if(0===h[0]){var
l=h[2],n=h[1];if(l){var
A=l[2],B=l[1];try{var
C=b(X[7],j[6],n),D=[0,C,a(i[17][1],l)],E=K(j,g)[6],F=b(k[23],D,E),G=function(g){var
f=a7(s[1],g),h=[0,[0,[0,n,A],f[1]],[0,[0,B,f[2]],e]],j=c[2],k=[0,a(i[19][8],c[1]),j,h];return b(i[22][3],k,d)},H=b(f[2][13],G,F);return H}catch(a){a=r(a);if(a===t)return 0;throw a}}try{var
v=z(j,b(X[7],j[6],n))===g?1:0,I=v?b(i[22][3],[0,c[1],c[2],e],d):v;return I}catch(a){a=r(a);if(a===t)return 0;throw a}}var
o=h[1],w=o-1|0;if(0<=q(c[1],w)[1+w]){var
x=o-1|0;return q(c[1],x)[1+x]===g?b(i[22][3],[0,c[1],c[2],e],d):0}var
y=o-1|0;q(c[1],y)[1+y]=g;return b(i[22][3],[0,c[1],c[2],e],d)}p[1]=[0,[0,c[2],c[1]],p[1]];return 0}function
aP(d,c){if(0===c[0]){var
e=c[1],f=a(i[17][1],c[2]);return[0,b(X[7],d,e),f]}return b(F[9],0,df)}function
bf(c){var
l=c[1][6],g=a(i[22][2],0),m=bd(c);function
d(a){var
h=a[5];if(typeof
h==="number")if(0===h)try{var
x=aP(l,a[4]),y=b(k[23],x,m),d=y}catch(a){a=r(a);if(a!==t)throw a;var
d=f[2][1]}else
var
d=f[2][1];else{var
z=h[1];try{var
A=b(C[7],c[12],z),o=A}catch(a){a=r(a);if(a!==t)throw a;var
o=f[2][1]}var
d=o}function
p(c){return b(i[22][3],[0,aB(a[3],-1),a,[0,[0,a[4],c],0]],g)}b(f[2][13],p,d);var
j=a[7];if(typeof
j==="number")if(0===j)try{var
s=aP(l,a[6]),u=b(k[23],s,m),e=u}catch(a){a=r(a);if(a!==t)throw a;var
e=f[2][1]}else
var
e=f[2][1];else{var
v=j[1];try{var
w=b(C[7],c[12],v),n=w}catch(a){a=r(a);if(a!==t)throw a;var
n=f[2][1]}var
e=n}function
q(c){return b(i[22][3],[0,aB(a[3],-1),a,[0,[0,a[6],c],0]],g)}return b(f[2][13],q,e)}b(i[17][11],d,c[7]);return g}function
bg(b){var
d=[0,0],e=bf(b);s(function(b){return a(c[3],dg)});try{for(;;){a(aO[3],0);be(b,d,e);continue}}catch(a){a=r(a);if(a===i[22][1])return d[1];throw a}}function
at(y,d){s(function(b){return a(c[3],dh)});try{for(;;){a(aO[3],0);try{cU(a(w[5],d[4]),d);var
M=1,j=M}catch(e){e=r(e);if(e!==w[1])throw e;try{var
x=a(w[5],d[5]);c2(x[1],x[2],d);var
L=1,j=L}catch(e){e=r(e);if(e!==w[1])throw e;try{var
g=a(f[2][26],d[3]);d[3]=b(f[2][6],g,d[3]);s(function(i){return function(j){var
e=a(c[3],cY),f=v(d[13],d[14],d[1],i),g=a(c[3],cZ),h=b(c[12],g,f);return b(c[12],h,e)}}(g));var
o=a7(d[1],g),p=o[1],A=$(d[1],g)[2],q=K(d[1],p),u=q[4],X=typeof
u==="number"?0:0===u[0]?(q[4]=1,d[8]=b(f[2][6],p,d[8]),1):0,B=a2(d[1],p),C=function(c,e){return function(a,f){return b(w[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,c,a[3]]]]],d[5])}}(A,g);b(l[11],C,B);var
E=q[6],H=function(c){return function(a,e){return b(w[3],[0,c,[0,[0,a[1],a[2]+1|0]]],d[5])}}(g);b(k[11],H,E);try{var
I=b7(o,d[2]);b(w[3],[0,g,I,0],d[4])}catch(a){a=r(a);if(a!==t)throw a;b4(g,o,d[2]);var
Z=a}var
J=1,j=J}catch(a){a=r(a);if(a!==t)throw a;var
j=0,_=a}var
aa=e}var
ab=e}if(j)continue;var
z=c5(d);if(z)var
S=z[1],T=y?[1,S]:0,m=[0,T];else
if(a(f[2][2],d[8]))if(0<d[10]){var
U=bg(d),V=function(q){var
m=q[2],f=q[1];a(aO[3],0);var
o=0<d[10]?1:0;if(o){if(cD(d,f[1],m))return s(function(b){return a(c[3],cE)});h(aK[5],d[9],f[1],m);var
u=d[1],t=function(b){try{var
e=D(u,b);return e}catch(b){b=r(b);if(a(F[18],b)){var
d=a(c[3],cx);return h(F[3],0,0,d)}throw b}},l=b(i[19][15],t,m),v=a(n[2],f[1]),p=b(i[19][15],G,l);a(i[19][46],p);var
g=a(n[15],[0,v,p]),j=aL(l,f[4]),k=aL(l,f[6]);d[11]=1;d[10]=d[10]-1|0;return f[2]?(s(function(C){var
f=a(c[3],cF),i=as(d[13],d[14],k),l=a(c[3],cG),m=as(d[13],d[14],j),n=a(c[3],cH),o=a(e[9],g),p=h(ag[11],d[13],d[14],o),q=a(c[3],cI),r=b(c[12],q,p),s=b(c[12],r,n),t=b(c[12],s,m),u=b(c[12],t,l),v=b(c[12],u,i),w=b(c[12],v,f),x=a(c[5],0),y=a(c[16],d[10]),z=a(c[3],cJ),A=b(c[12],z,y),B=b(c[12],A,x);return b(c[12],B,w)}),aM(d,g,j,k)):(s(function(C){var
f=a(c[3],cK),i=as(d[13],d[14],k),l=a(c[3],cL),m=as(d[13],d[14],j),n=a(c[3],cM),o=a(e[9],g),p=h(ag[11],d[13],d[14],o),q=a(c[3],cN),r=b(c[12],q,p),s=b(c[12],r,n),t=b(c[12],s,m),u=b(c[12],t,l),v=b(c[12],u,i),w=b(c[12],v,f),x=a(c[5],0),y=a(c[16],d[10]),z=a(c[3],cO),A=b(c[12],z,y),B=b(c[12],A,x);return b(c[12],B,w)}),Y(d,[0,g],j,k))}return o};b(i[17][11],V,U);var
W=d[11]?(d[11]=0,at(1,d)):(s(function(b){return a(c[3],di)}),0),m=W}else{s(function(b){return a(c[3],dj)});var
m=0}else{s(function(b){return a(c[3],dk)});de(d);var
m=at(0,d)}return m}}catch(a){a=r(a);if(a[1]===a5){var
N=a[5],O=a[4],P=a[3],Q=a[2],R=y?[0,[0,Q,P,O,N]]:0;return[0,R]}throw a}}ak(146,[0,[0,k[1],k[2],k[3],k[4],k[5],k[6],k[7],k[8],k[9],k[10],k[11],k[12],k[13],k[14],k[15],k[16],k[17],k[18],k[19],k[20],k[21],k[22],k[23],k[24],k[25],k[26]],[0,l[1],l[2],l[3],l[4],l[5],l[6],l[7],l[8],l[9],l[10],l[11],l[12],l[13],l[14],l[15],l[16],l[17],l[18],l[19],l[20],l[21],l[22],l[23],l[24],l[25],l[26]],C,X,aq,G,s,cb,cg,ch,a1,M,aM,Y,aN,a6,z,a3,D,af,$,a$,bd,be,bf,aP,bg,at,v,a0],"Cc_plugin__Ccalgo");function
ah(a){return[0,a,a,[2,a]]}function
aa(b,a){var
c=b[3],d=a[3];if(2===c[0])if(2===d[0])return ah([3,c[1],d[1]]);return[0,[3,b[1],a[1]],[3,b[2],a[2]],[4,b,a]]}function
B(m,l){var
d=m,b=l;for(;;){var
e=d[3],f=b[3];switch(e[0]){case
2:return b;case
4:var
j=e[2],k=e[1];switch(f[0]){case
2:var
g=0;break;case
3:var
i=f[1][3];if(4===i[0]){var
p=f[2],q=i[1],r=B(j,i[2]),d=aa(B(k,q),r),b=p;continue}var
g=1;break;case
4:var
s=f[1],t=B(j,f[2]);return aa(B(k,s),t);default:var
g=1}break;default:var
g=0}if(!g){if(2===f[0])return d;if(3===e[0]){var
o=e[1],d=o,b=B(e[2],b);continue}}if(aq(d[2],b[1]))return[0,d[1],b[2],[3,d,b]];var
n=a(c[3],dl);return h(F[3],0,0,n)}}function
N(b){var
a=b[3];switch(a[0]){case
0:return[0,b[2],b[1],[1,a[1]]];case
1:return[0,b[2],b[1],[0,a[1]]];case
2:return b;case
3:var
c=a[2],d=N(a[1]);return B(N(c),d);case
4:var
e=a[1],f=N(a[2]);return aa(N(e),f);default:var
g=a[4],h=a[3],i=a[2],j=[5,N(a[1]),i,h,g];return[0,b[2],b[1],j]}}function
bh(d,a){var
c=b(C[7],d,a);return[0,c[1],c[2],[0,a]]}function
bi(d,a){var
c=b(C[7],d,a);return[0,c[2],c[1],[1,a]]}function
bj(f,e){var
b=f,d=e;for(;;){if(3===b[0]){var
i=b[2],j=b[1];if(0<d){var
b=j,d=d-1|0;continue}return i}var
g=a(c[3],dm);return h(F[3],0,dn,g)}}function
bk(c,d,b,a){var
e=bj(c[2],b-a|0);return[0,bj(c[1],b-a|0),e,[5,c,d,b,a]]}function
R(h,g,d,e,f){s(function(o){var
i=v(h,g,d,f),j=a(c[4],dp),k=v(h,g,d,e),l=a(c[3],dq),m=b(c[12],l,k),n=b(c[12],m,j);return b(c[12],n,i)});if(e===f)return ah(D(d,e));var
i=a$(d,e,f),j=i[1],k=N(au(h,g,d,f,i[2]));return B(au(h,g,d,e,j),k)}function
bl(g,f,d,j){var
h=j[2],k=j[1],l=k[2],m=k[1];s(function(o){var
e=v(g,f,d,l),h=a(c[4],dr),i=v(g,f,d,m),j=a(c[3],ds),k=b(c[12],j,i),n=b(c[12],k,h);return b(c[12],n,e)});var
q=R(g,f,d,m,h[1]),r=N(R(g,f,d,l,h[2])),e=h[3];if(typeof
e==="number")var
i=bm(g,f,d,h[1],h[2]);else
if(0===e[0])var
n=e[1],t=e[2]?bi(d[4],n):bh(d[4],n),i=t;else
var
o=e[2],u=e[5],w=aR(g,f,d,e[1],o,e[3],e[4]),p=af(d,o[1]),i=bk(w,p[1],p[3],u);return B(B(q,i),r)}function
aQ(h,g,d,f,e){s(function(l){var
e=a(c[4],dt),i=v(h,g,d,f),j=a(c[3],du),k=b(c[12],j,i);return b(c[12],k,e)});var
i=a3(d,f,e),j=R(h,g,d,f,i);if(0===e[3])return j;var
l=a6(e),k=$(d,i),m=k[1],n=D(d,k[2]),o=aQ(h,g,d,m,l);return B(j,aa(o,ah(n)))}function
au(g,f,e,i,d){s(function(w){var
j=a(c[3],dv);function
k(b){return a(c[16],b[1][2])}function
l(b){return a(c[3],dw)}var
m=h(c[39],l,k,d),n=a(c[3],dx),o=a(c[4],dy),p=v(g,f,e,i),q=a(c[3],dz),r=b(c[12],q,p),s=b(c[12],r,o),t=b(c[12],s,n),u=b(c[12],t,m);return b(c[12],u,j)});if(d){var
j=d[1],k=d[2],l=bl(g,f,e,j);return B(au(g,f,e,j[1][2],k),l)}return ah(D(e,i))}function
bm(f,e,d,h,g){s(function(o){var
i=v(f,e,d,g),j=a(c[4],dA),k=v(f,e,d,h),l=a(c[3],dB),m=b(c[12],l,k),n=b(c[12],m,j);return b(c[12],n,i)});var
i=$(d,h),k=i[2],l=i[1],j=$(d,g),m=j[1],n=R(f,e,d,k,j[2]);return aa(R(f,e,d,l,m),n)}function
aR(f,e,d,h,j,g,i){s(function(o){var
i=v(f,e,d,g),j=a(c[4],dC),k=v(f,e,d,h),l=a(c[3],dD),m=b(c[12],l,k),n=b(c[12],m,j);return b(c[12],n,i)});var
k=R(f,e,d,h,g),l=aQ(f,e,d,h,j),m=B(k,aQ(f,e,d,g,i));return B(N(l),m)}function
aS(e,d,c,b){if(bI<=b[1]){var
a=b[2];return aR(e,d,c,a[1],a[2],a[3],a[4])}var
f=b[2];return R(e,d,c,f[1],f[2])}ak(147,[0,ah,aa,B,N,bh,bi,bk,R,bl,au,bm,aR,aS],"Cc_plugin__Ccproof");var
aT=[A,function(b){return a(O[2],dE)}],dG=[A,function(b){return a(O[2],dF)}],bn=[A,function(b){return a(O[2],dH)}],dJ=[A,function(b){return a(O[2],dI)}],bo=[A,function(b){return a(O[2],dK)}],x=[A,function(b){return a(O[2],dL)}],S=[A,function(b){return a(O[2],dM)}];function
bp(c,b,a){return am(br[17],bq[9],c,b,a)}function
av(c,b,a){return am(br[17],bq[4],c,b,a)}function
aw(c,b,a){return h(Q[3],c,b,a)[2]}function
E(f,c,z){var
g=z;for(;;){var
A=bp(f,c,g),d=b(e[3],c,A);switch(d[0]){case
6:var
l=d[3],o=d[2];if(h(e[an][13],c,1,l)){var
p=a(P[47],l),B=aw(f,c,p),C=aw(f,c,o),D=E(f,c,p);return[3,[3,[1,C,B],E(f,c,o)],D]}break;case
9:var
F=d[2],G=E(f,c,d[1]),H=function(a){return E(f,c,a)},I=b(i[19][15],H,F),J=function(b,a){return[3,b,a]};return h(i[19][17],J,G,I);case
10:var
q=d[1],K=q[1],L=b(e[2][2],c,q[2]),M=a(m[17][5],K),N=[0,a(m[17][2],M),L];return[0,a(n[18],N)];case
11:var
r=d[1],s=r[1],O=s[2],Q=s[1],R=b(e[2][2],c,r[2]),S=a(m[23][5],Q),T=[0,[0,a(m[23][2],S),O],R];return[0,a(n[21],T)];case
12:var
u=d[1],v=u[1],w=v[2],x=v[1],U=x[2],V=x[1],W=b(e[2][2],c,u[2]),X=a(m[23][5],V),j=[0,a(m[23][2],X),U],Y=a(dO[32],j)[1],y=b(dP[46],f,[0,j,w]);return[4,[0,[0,[0,j,w],W],y,y-Y[6]|0]];case
16:var
Z=d[2],_=d[1],$=function(b){var
c=a(m[23][5],b);return a(m[23][2],c)},aa=b(m[62][19],$,_),g=aC(dQ[9],f,c,aa,Z,0);continue}var
k=b(P[60],c,g);if(b(e[an][16],c,k))return[0,h(e[5],dN,c,k)];throw t}}function
aU(d,c,g){var
k=av(d,c,g),i=b(e[3],c,k);if(9===i[0]){var
f=i[2],l=i[1],j=U(x),m=V===j?x[1]:A===j?a(T[2],x):x;if(h(P[ao],c,m,l))if(3===f.length-1){var
n=E(d,c,q(f,2)[3]),o=E(d,c,q(f,1)[2]);return[0,aG,[0,q(f,0)[1],o,n]]}return[0,aW,E(d,c,g)]}return[0,aW,E(d,c,g)]}function
ai(d,c,j){var
r=bp(d,c,j),g=b(e[3],c,r);switch(g[0]){case
0:var
k=g[1];return[0,[1,k],a(f[2][5],k)];case
6:var
l=g[3],m=g[2];if(h(e[an][13],c,1,l)){var
n=a(P[47],l),o=ai(d,c,m),t=o[2],u=o[1],p=ai(d,c,n),v=p[2],w=p[1],x=aw(d,c,n),y=aw(d,c,m);return[0,[0,[1,y,x],[0,u,[0,w,0]]],b(f[2][7],t,v)]}break;case
9:var
z=g[2],A=E(d,c,g[1]),B=function(a){return ai(d,c,a)},C=b(i[19][56],B,z),q=a(i[17][119],C),D=q[1],F=h(i[17][15],f[2][7],f[2][1],q[2]);return[0,[0,A,a(i[17][9],D)],F]}var
s=E(d,c,j);return[0,[0,s,0],f[2][1]]}function
bs(a){return 0===a[0]?1:0}function
bt(k,c,j,v){try{var
w=av(k,c,v),l=b(e[81],c,w)}catch(a){a=r(a);if(a===n[59])throw t;throw a}var
d=l[2],y=l[1],m=U(x),z=V===m?x[1]:A===m?a(T[2],x):x;if(h(P[ao],c,z,y))if(3===d.length-1){var
o=ai(k,c,q(d,1)[2]),p=o[1],B=o[2],s=ai(k,c,q(d,2)[3]),u=s[1],C=s[2];if(a(f[2][20],B)===j)if(bs(p))var
g=0;else
var
E=q(d,0)[1],g=[0,h(e[5],0,c,E)];else
var
g=1;if(a(f[2][20],C)===j)if(bs(u))var
i=0;else
var
D=q(d,0)[1],i=[0,h(e[5],0,c,D)];else
var
i=1;if(1===g)if(1===i)throw t;return[0,j,g,p,i,u]}throw t}function
dR(o,c,n,m){var
d=o,f=n,i=m;for(;;){var
p=av(d,c,i),g=b(e[3],c,p);if(6===g[0]){var
j=g[3],k=g[2],q=g[1],l=U(S),r=V===l?S[1]:A===l?a(T[2],S):S;if(h(P[ao],c,r,j))return[0,bH,bt(d,c,f,k)];var
d=b(e[bP],[0,q,k],d),f=f+1|0,i=j;continue}return[0,bR,bt(d,c,f,i)]}}function
dS(d,c,g){var
n=av(d,c,g),f=b(e[3],c,n);if(6===f[0]){var
k=f[3],l=f[2],o=f[1],m=U(S),p=V===m?S[1]:A===m?a(T[2],S):S;if(h(P[ao],c,p,k)){var
i=aU(d,c,l);if(aG<=i[1]){var
j=i[2];return[0,bJ,[0,j[1],j[2],j[3]]]}return[0,bO,i[2]]}try{var
q=dR(b(e[bP],[0,o,l],d),c,1,k);return q}catch(a){a=r(a);if(a===t)return[0,aW,E(d,c,g)];throw a}}return aU(d,c,g)}function
Z(c,g,f){function
h(b){return a(f,a(e[23],[0,b,g]))}var
d=U(c),i=V===d?c[1]:A===d?a(T[2],c):c,k=a(p[65][61],i);return b(j[73][1],k,h)}function
aj(c,n,v){function
d(k){function
f(p){var
q=a(j[68][4],k),w=a(j[68][2],k);function
c(r){var
x=b(o[35][8],k,p),y=a(i[19][11],n),z=h(P[58],r,x,y),c=r,j=z,f=v,d=0,A=a(e[23],[0,p,n]);for(;;){if(0===f){var
B=[0,A,a(i[17][9],d)],s=a(e[40],B);return[0,am(Q[4],q,c,s,w),s]}var
g=b(e[3],c,j);if(6===g[0]){var
t=g[3],l=eN(dV[4],0,0,0,0,0,0,0,0,q,c,g[2]),m=l[2],u=l[1],c=u,j=b(e[an][5],m,t),f=f-1|0,d=[0,m,d];continue}throw[0,ba,dU]}}return b(dW[1],0,c)}var
d=U(c),g=V===d?c[1]:A===d?a(T[2],c):c,l=a(p[65][61],g);return b(j[73][1],l,f)}return a(j[68][8],d)}function
ax(d,c){function
e(e){var
f=Q[2];function
g(a){return b(f,0,a)}var
i=h(o[35][1],g,e,c)[1],k=b(H[137],d,c),l=a(j[66][1],i);return b(j[18],l,k)}return a(j[68][8],e)}function
bv(c,b,a){return aD(bw[6],[0,dZ[125]],0,dY,dX,c,b,a)}function
I(f,e){function
c(c){var
g=a(j[68][4],c),d=bv(g,a(o[35][4],c),f),h=d[1],i=a(e,d[2]),k=a(j[66][1],h);return b(j[18],k,i)}return a(j[68][8],c)}function
u(b){var
c=G(b);return a(e[9],c)}function
J(k){function
d(i){function
f(a){return b(o[35][7],i,a)}try{var
d=k[3];switch(d[0]){case
0:var
D=a(e[9],d[1]),g=a(H[46],D);break;case
1:var
E=a(e[9],d[1]),w=u(k[1]),F=u(k[2]),G=function(a){return Z(dJ,[0,a,F,w,E],H[46])},g=I(f(w),G);break;case
2:var
x=d[1],K=u(x),M=function(a){var
b=H[46];return Z(bn,[0,a,u(x)],b)},g=I(f(K),M);break;case
3:var
y=d[2],q=d[1],N=u(q[1]),z=u(q[2]),O=u(y[2]),P=function(a){var
c=aj(bo,[0,a,N,z,O],2),d=[0,J(y),0],e=[0,J(q),d];return b(p[65][21],c,e)},g=I(f(z),P);break;case
4:var
s=d[2],t=d[1],l=u(t[1]),h=u(s[1]),n=u(t[2]),A=u(s[2]),Q=function(g){function
d(j){function
d(d){var
f=a(m[1][6],d0),k=b(o[35][11],f,i),q=[0,a(e[10],1),[0,h]],r=a(e[23],q),u=[0,b(L[4],[0,k],0),g,r],v=aj(aT,[0,g,d,a(e[21],u),l,n],1),w=aj(aT,[0,j,d,n,h,A],1),x=a(e[23],[0,n,[0,A]]),y=a(e[23],[0,n,[0,h]]),z=aj(bo,[0,d,a(e[23],[0,l,[0,h]]),y,x],2),B=a(c[3],d1),C=[0,b(p[65][5],0,B),0],D=[0,H[123],C],E=J(s),F=[0,b(p[65][3],w,E),D],G=[0,a(p[65][26],F),0],I=J(t),K=[0,b(p[65][3],v,I),G];return b(p[65][21],z,K)}return I(f(a(e[23],[0,l,[0,h]])),d)}return I(f(h),d)},g=I(f(l),Q);break;default:var
v=d[1],R=d[4],S=d[3],T=d[2],B=u(v[1]),U=u(v[2]),C=u(k[1]),V=a(e[10],(1+S|0)-R|0),W=function(c){function
d(r){var
f=T[1][2],d=a(o[35][4],i),g=a(e[10],1),h=a(o[35][5],i),k=aD(bu[38],h,d,f,g,c,V,C),l=a(m[1][6],dT),n=[0,b(o[35][11],l,i)],q=[0,b(L[4],n,0),c,k],s=aj(aT,[0,c,r,a(e[21],q),B,U],1),t=J(v),u=b(p[65][3],s,t),w=a(j[66][1],d);return b(p[65][3],w,u)}return I(f(C),d)},g=I(f(B),W)}return g}catch(c){c=r(c);if(a(j[72][9],c))return b(j[21],0,c);throw c}}return a(j[68][8],d)}function
d3(c){function
d(d){var
e=Q[2];function
f(a){return b(e,0,a)}var
g=h(o[35][1],f,d,c)[1],i=a(H[46],c),k=a(j[66][1],g);return b(j[18],k,i)}return a(j[68][8],d)}function
bx(k,h,f,i){function
c(c){var
g=u(h),d=u(f);function
j(f){var
l=a(m[1][6],d4),h=b(o[35][11],l,c),n=a(m[1][6],d5),q=b(o[35][11],n,c),r=a(e[10],1),s=[0,b(L[4],[0,q],0),f,r],t=a(e[21],s),u=[0,Z(dG,[0,f,g,t,k,d,a(e[11],h)],d3),0],j=[0,f,g,d],v=[0,J(i),u],w=[0,h],y=Z(x,j,function(a){return ax(w,a)});return b(p[65][21],y,v)}return I(b(o[35][7],c,d),j)}return a(j[68][8],c)}function
by(ab,aa){function
d(f){var
C=a(o[35][4],f);a(O[12],O[14]);s(function(b){return a(c[3],d8)});var
z=a(o[35][5],f),q=a(o[35][4],f),d=a1(ab,[0,a(j[68][12],f),q]),r=[0,0],A=[0,0];function
R(a){M(d,E(z,q,a));return 0}b(i[17][11],R,aa);var
S=a(j[68][3],f);function
T(h){var
g=a(L[11][1][2],h),e=a(n[2],g),c=dS(z,q,a(L[11][1][4],h)),f=c[1];if(aG<=f){if(bH<=f)return bR<=f?aN(d,g,1,c[2]):aN(d,g,0,c[2]);if(bJ<=f){var
j=c[2];return Y(d,[0,e],j[2],j[3])}var
k=c[2];return aM(d,e,k[2],k[3])}if(bO<=f){var
l=c[2],o=r[1],p=function(a){return Y(d,[2,a[1],e],a[2],l)};b(i[17][11],p,o);A[1]=[0,[0,e,l],A[1]];return 0}var
m=c[2],s=A[1];function
t(a){return Y(d,[2,e,a[1]],m,a[2])}b(i[17][11],t,s);r[1]=[0,[0,e,m],r[1]];return 0}b(i[17][11],T,S);var
B=aU(z,q,a(o[35][6],f));if(aG<=B[1]){var
K=B[2];Y(d,0,K[2],K[3])}else{var
U=B[2],V=r[1],W=function(a){return Y(d,[1,a[1]],a[2],U)};b(i[17][11],W,V)}s(function(b){return a(c[3],d9)});var
N=at(1,d);s(function(b){return a(c[3],d_)});var
g=d[1];if(N){var
t=N[1];s(function(b){return a(c[3],d$)});if(typeof
t==="number"){var
P=a(j[68][4],f),ac=g[5],ad=function(c){var
f=af(g,c[1]),h=c[3];function
j(a){return u(D(g,a))}var
k=b(i[17][14],j,h),d=f[1],l=d[1],m=c[2],n=[0,l,a(e[2][1],d[2])],o=[0,a(e[30],n),k];return[0,a(e[40],o),m]},ae=b(i[17][68],ad,ac),ah=b(bz[3],0,ea),ai=function(a){var
c=a[2],d=aD(ec[9],0,eb,0,m[1][10][1],P,C,a[1]);function
e(a){return ah}var
f=[4,d,b(i[17][56],c,e)],g=b(bz[3],0,f);return b(ag[27],P,g)},aj=a(c[3],ed),ak=a(c[3],ee),al=function(h){var
d=a(c[3],ef),e=a(c[13],0),f=a(c[3],eg),g=b(c[12],f,e);return b(c[12],g,d)},am=h(c[39],al,ai,ae),an=a(c[3],eh),ao=b(c[12],an,am),ap=b(c[12],ao,ak),aq=b(c[26],8,ap),ar=a(c[3],ei),as=a(c[5],0),au=a(c[3],ej),av=b(c[12],au,as),aw=b(c[12],av,ar),ay=b(c[12],aw,aq),az=b(c[12],ay,aj);return b(p[65][4],0,az)}else{if(0===t[0]){var
v=t[1],Q=v[2],aA=[0,bI,[0,v[1],Q,v[3],v[4]]],F=aS(a(o[35][5],f),C,g,aA);af(g,Q[1]);var
$=function(c){var
d=u(F[1]),g=u(F[2]),h=a(j[68][4],c),i=a(o[35][4],c),e=bv(h,i,b(o[35][7],c,d)),k=e[2],l=e[1],n=a(m[1][6],d7),f=b(o[35][11],n,c),r=[0,a(bu[16],f),0],q=[0,k,d,g],s=[0,J(F),r],t=[0,f],y=Z(x,q,function(a){return ax(t,a)}),v=b(p[65][21],y,s),w=a(j[66][1],l);return b(p[65][3],w,v)};return a(j[68][8],$)}var
l=t[1],aB=a(j[68][4],f),w=aS(aB,C,g,[0,-608347012,[0,l[1],l[2]]]),G=D(g,l[1]),y=D(g,l[2]),k=l[3];if(typeof
k==="number")return J(w);else
switch(k[0]){case
0:var
aC=a(e[9],k[1]),X=function(c){var
d=u(G),g=u(y),h=a(m[1][6],d2),f=b(o[35][11],h,c),i=[0,aC,[0,a(e[11],f)]],j=a(e[23],i);function
k(c){var
h=[0,a(H[bL],j),0],e=[0,c,d,g],i=[0,J(w),h],k=[0,f],l=Z(x,e,function(a){return ax(k,a)});return b(p[65][21],l,i)}return I(b(o[35][7],c,d),k)};return a(j[68][8],X);case
1:return bx(a(e[9],k[1]),G,y,w);default:var
aE=k[2],aF=a(e[9],k[1]),aH=a(e[9],aE),_=function(d){var
f=u(y),g=a(m[1][6],d6),c=b(o[35][11],g,d),h=[0,aH,[0,a(e[11],c)]],i=a(e[23],h),j=[0,a(H[bL],i),0],k=[0,bx(aF,G,y,w),j],l=ax([0,c],f);return b(p[65][21],l,k)};return a(j[68][8],_)}}}var
aI=a(c[3],ek);return b(p[65][4],0,aI)}return a(j[68][8],d)}var
em=a(c[3],el),bA=b(p[65][5],0,em);function
ab(d,c){var
e=by(d,c),f=a(p[65][32],H[17]),g=b(p[65][3],f,e);return b(p[65][12],g,bA)}function
en(c,d,l,k){function
g(m){function
c(c){var
n=Q[2];function
p(a){return b(n,0,a)}var
f=h(o[35][1],p,c,d),q=f[2],r=f[1],s=a(o[35][5],c),g=aD(bw[6],0,0,0,eo,s,r,q),t=g[1],i=a(e[23],[0,m,[0,g[2],d,l]]),u=a(o[35][5],c),v=am(Q[2],0,u,t,i)[1],w=a(k,i),x=a(j[66][1],v);return b(j[18],x,w)}return a(j[68][8],c)}var
f=U(c),i=V===f?c[1]:A===f?a(T[2],c):c,m=a(p[65][61],i);return b(j[73][1],m,g)}function
ep(l){var
y=a(j[68][2],l),c=a(o[35][4],l);function
z(d,c){try{var
e=0,f=H[87],g=[0],h=function(a){return Z(bn,g,a)}(f),i=[0,a(p[65][24],h),e],k=[0,a(j[16],0),i],l=en(x,d,c,H[144]),m=b(p[65][21],l,k);return m}catch(c){c=r(c);if(a(j[72][9],c))return b(j[21],0,c);throw c}}function
B(d){var
c=d[1],e=d[2];if(c[1]!==eq[1])if(c[1]!==er[1])return b(j[21],[0,e],c);return a(j[16],0)}var
g=b(e[3],c,y);if(9===g[0]){var
i=g[2];if(3===i.length-1){var
C=g[1],D=i[2],E=i[3],n=U(x),F=V===n?x[1]:A===n?a(T[2],x):x;if(h(P[ao],c,F,C)){var
s=b(e[3],c,D),t=b(e[3],c,E);if(9===s[0])if(9===t[0]){var
v=t[2],k=s[2];if(k.length-1===v.length-1)var
w=function(c){if(0<=c){var
d=w(c-1|0),e=q(v,c)[1+c],f=z(q(k,c)[1+c],e);return b(p[65][16],f,d)}var
g=ab(aV,0);return a(p[65][24],g)},u=w(k.length-1-1|0),f=1;else
var
f=0}else
var
f=0;else
var
f=0;if(!f)var
u=a(j[16],0);var
m=u,d=1}else
var
d=0}else
var
d=0}else
var
d=0;if(!d)var
m=a(j[16],0);return b(j[23],m,B)}var
bB=a(j[68][8],ep);ak(168,[0,J,by,bA,ab,bB],"Cc_plugin__Cctac");a(es[9],ay);var
et=0;function
eu(b,a,c){return ab(b,a)}var
ew=[0,ev,[1,[0,[5,a(aA[16],az[11])]],0]],ey=[0,[0,[0,ex,[1,[5,a(aA[16],az[15])],ew]],eu],et];function
ez(a,b){return ab(aV,a)}var
eC=[0,[0,[0,eB,[0,eA,[1,[0,[5,a(aA[16],az[11])]],0]]],ez],ey];function
eD(a,b){return ab(a,0)}var
eF=[0,[0,[0,eE,[1,[5,a(aA[16],az[15])],0]],eD],eC],eH=[0,[0,eG,function(a){return ab(aV,0)}],eF];aC(bC[8],ay,eI,0,0,eH);var
eJ=0,eL=[0,[0,eK,function(a){return bB}],eJ];aC(bC[8],ay,eM,0,0,eL);ak(173,[0,ay],"Cc_plugin__G_congruence");return}
