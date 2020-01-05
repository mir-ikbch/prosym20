function(jy){"use strict";var
b9="closed",a4="x",b8="ring_lookup",cr=",",J='"',a2="ring",k=246,b7=141,cq="(",cp="constants",b6="preprocess tactic",aL="Field_tac",cc="with carrier ",co="postprocess tactic",aK="field",cn="InitialRing",cb="decidable",A="[",cf='and morphisms "',ck="Pphi_pow",cl='Using setoid "',cm="tactic recognizing constants",X=104,cj="postprocess",a_="gen_phiZ",aN="setoid",b5="and equivalence relation ",b4=")",aM=103,B="]",b2=":",b3="t",ca="preprocess",a9="_vendor+v8.10+32bit/coq/plugins/setoid_ring/newring.ml",a3="power_tac",ci="Print",a8="power",b1="abstract",ch="Ring_polynom",aJ="sign",a7="protect_fv",b0="PEeval",a6="Ring",aI="div",b$='and "',a1="newring_plugin",cd="Add",ce="completeness",a0="IDphi",b_="morphism",cg="field_lookup",bZ="Pphi_dev",n=250,a5="Coq",H=jy.jsoo_runtime,E=H.caml_check_bound,c=H.caml_new_string,p=H.caml_obj_tag,aH=H.caml_register_global,z=H.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):H.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):H.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):H.caml_call_gen(a,[b,c,d])}function
ad(a,b,c,d,e){return a.length==4?a(b,c,d,e):H.caml_call_gen(a,[b,c,d,e])}function
D(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):H.caml_call_gen(a,[b,c,d,e,f])}function
jx(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):H.caml_call_gen(a,[b,c,d,e,f,g,h])}var
f=H.caml_get_global_data(),be=[0,c(a1),c("closed_term")],bk=[0,c(a1),c("get_res")],ap=c("setoid_ring"),al=c(a1),Z=f.Constr,g=f.Util,Q=f.CClosure,ah=f.Termops,j=f.Names,F=f.Not_found,h=f.EConstr,bL=f.Tacmach,q=f.Proofview,v=f.Ltac_plugin__Tacinterp,P=f.Coqlib,L=f.Global,w=f.Stdlib,ag=f.Ltac_plugin__Tacintern,bF=f.Lib,o=f.CamlinternalLazy,aU=f.UnivGen,ac=f.Ltac_plugin__Rewrite,x=f.Mod_subst,V=f.Ltac_plugin__Tacsubst,ab=f.Retyping,bt=f.Reductionops,d=f.Pp,Y=f.CErrors,C=f.Printer,aS=f.Assert_failure,$=f.Evd,af=f.Feedback,ax=f.Typing,am=f.Option,_=f.Evarutil,bz=f.Smartlocate,an=f.Loc,O=f.CAst,bv=f.Flags,G=f.Ppconstr,t=f.Stdarg,l=f.Genarg,aT=f.Declare,bi=f.Constrintern,bc=f.Tactics,bf=f.Ltac_plugin__Tacenv,br=f.Summary,aw=f.Libobject,N=f.Vernacextend,aG=f.Attributes,aE=f.Ltac_plugin__Pptactic,aX=f.Ltac_plugin__Tacentries,I=f.Pcoq,i=f.CLexer,aF=f.Ltac_plugin__Pltac,bT=f.Ltac_plugin__Tacarg,cx=f.Esubst,cX=f.Refiner,cP=f.Environ,cQ=f.Goal,cJ=f.Vars,cK=f.UState,cL=f.Typeops,cM=f.Univ,cH=f.DAst,cD=f.Tacticals,cw=f.Globnames,fB=f.Redexpr,gt=f.Mltop;aH(249,[0,0,0,0],"Newring_plugin");aH(n,[0],"Newring_plugin__Newring_ast");var
gr=[0,0,0],gs=[0,0,0],gh=c("field kind"),gi=c(cm),gj=c(b6),gk=c(co),gl=c(aN),gm=c(a8),gn=c(aJ),go=c(aI),gp=c("infinite property"),f9=[0,c(aL),0],f_=[0,0,0],f$=c("field_lemmas"),ga=c("_field_lemma1"),gb=c("_field_lemma2"),gc=c("_field_lemma3"),gd=c("_field_lemma4"),ge=c("_lemma5"),gg=[22,0],gf=[22,0],f8=c("core.eq.congr"),f7=c("field inverse should be declared as a morphism"),fU=c("arguments of field_simplify do not have all the same type"),fV=[0,c(aK)],fT=[0,c(aL),0],fW=c(J),fX=c(J),fY=c("cannot find a declared field structure over"),fZ=[0,c(aK)],f0=[0,c(a9),816,12],fQ=c(b5),fR=c(cc),fP=c("The following field structures have been declared:"),fM=[0,1],fN=[0,0],fL=c("bad field structure"),eP=[0,0,0],eQ=[0,0,0],eG=c("ring kind"),eH=c(cm),eI=c(b6),eJ=c(co),eK=c(aN),eL=c(a8),eM=c(aJ),eN=c(aI),eF=c(" cannot be set twice"),ez=[0,c("Ring_base"),0],eA=c("ring_lemmas"),eB=c("_ring_lemma1"),eC=c("_ring_lemma2"),eE=[22,0],eD=[22,0],ex=[0,1],ey=[0,0],ew=c("bad ring structure"),ef=c("ring addition should be declared as a morphism"),eg=c("ring multiplication should be declared as a morphism"),eh=c("ring opposite should be declared as a morphism"),ei=c(J),ej=c(b$),ek=c(J),el=c(J),em=c('",'),en=c(cf),eo=c(J),ep=c(cl),eq=c(J),er=c(b$),es=c(J),et=c(cf),eu=c(J),ev=c(cl),eb=c("cannot find setoid relation"),d1=c("arguments of ring_simplify do not have all the same type"),d2=[0,c(a2)],d3=c(J),d4=c(J),d5=c("cannot find a declared ring structure over"),d6=[0,c(a2)],d7=[0,c(a9),360,12],dY=c(b5),dZ=c(cc),dX=c("The following ring structures have been declared:"),df=[0,c(ch),0],de=c("newring"),c$=c(a6),c_=c(a6),c6=c("ring: cannot find relation (not closed)"),c5=c("ring: cannot find relation"),cW=c(a4),cV=c(a4),cT=c(a4),cR=c("Ring.exec_tactic: anomaly"),cN=[2,1],cO=[0,1],cG=c(b3),cI=c(b3),cC=[0,c(a9),116,7],cz=c("not found"),cA=c("map "),cB=[0,c("lookup_map")],cv=c("dummy"),ct=c("global_head_of_constr."),cY=c("plugins.setoid_ring.Build_Setoid_Theory"),c0=c("core.option.None"),c1=c("core.option.Some"),c2=c("core.eq.type"),c3=c("core.list.cons"),c4=c("core.list.nil"),c7=c(a5),c8=[0,[0,c("Ring_theory"),0],[0,[0,c(ch),0],[0,[0,c("Ring_tac"),0],[0,[0,c(cn),0],[0,[0,c(aL),0],[0,[0,c("Field_theory"),0],0]]]]]],da=[0,c(a5),0],db=c(cn),dg=c("almost_ring_theory"),dh=c("Eqsth"),dj=c("Eq_ext"),dl=c("Eq_s_ext"),dn=c("ring_theory"),dp=c("mk_reqe"),dr=c("semi_ring_theory"),ds=c("mk_seqe"),du=c("Abstract"),dv=c("Computational"),dx=c("Morphism"),dz=c("inv_morph_nothing"),dA=c("mkhypo"),dC=c("hypo"),dF=c(b0),dI=c(ck),dL=c(bZ),dO=c(a_),dR=c(a0),dV=c(a2),dW=c("ring-tac-carrier-table"),d_=c("tactic-new-ring-theory"),eR=[0,c(a5),0],eS=c(aL),eX=c("FEeval"),e0=c(b0),e3=c(ck),e6=c(bZ),e9=c("display_pow_linear"),fa=c("display_linear"),fd=c(a_),fg=c(a0),fk=c(aK),fn=c("PCond"),fq=c(a_),ft=c(a0),fx=c("field_cond"),fy=c(aK),fA=c("simpl_field_expr"),fC=c("almost_field_theory"),fD=c("field_theory"),fE=c("semi_field_theory"),fF=c("AF_AR"),fH=c("F_R"),fJ=c("SF_SR"),fO=c("field-tac-carrier-table"),f3=c("tactic-new-field-theory"),iQ=c(ce),g8=[0,0],gE=c(b1),gF=c(cb),gG=c(b_),gH=c(B),gI=c(A),gJ=c(cp),gK=c(B),gL=c(A),gM=c(b9),gN=c(B),gO=c(A),gP=c(ca),gQ=c(B),gR=c(A),gS=c(cj),gT=c(aN),gU=c(B),gV=c(A),gW=c(a3),gX=c(B),gY=c(A),gZ=c(a3),g0=c(aJ),g1=c(aI),gw=c(a7),gz=c("in"),gB=c(a7),gD=c(a7),g5=c(cb),g9=c(b1),hb=c(b_),he=c(B),hh=c(A),hj=c(cp),hm=c(B),hp=c(A),hr=c(b9),hu=c(B),hx=c(A),hz=c(ca),hC=c(B),hF=c(A),hH=c(cj),hM=c(aN),hQ=c(aJ),hT=c(B),hW=c(A),hZ=c(a8),h2=c(B),h5=c(A),h8=c(a3),ia=c(aI),id=c("ring_mod"),ii=c(b4),ik=c(cr),im=c(cq),iq=c("ring_mods"),iu=[0,c(ci),[0,c("Rings"),0]],iz=c(b2),iB=c(a6),iC=c(cd),iG=c("AddSetoidRing"),iJ=c(B),iL=c(A),iN=c(b8),iP=c(b8),iV=c(ce),iY=c("field_mod"),i2=c(b4),i4=c(cr),i6=c(cq),i9=c("field_mods"),jb=[0,c(ci),[0,c("Fields"),0]],jg=c(b2),ji=c("Field"),jj=c(cd),jn=c("AddSetoidField"),jq=c(B),js=c(A),ju=c(cg),jw=c(cg);function
K(b){var
c=a(d[3],b);return e(Y[6],0,0,c)}function
cs(c,f){var
g=b(h[91],c,f)[1];try{var
j=b(ah[aM],c,g)[1];return j}catch(b){b=z(b);if(b===F){var
i=a(d[3],ct);return e(Y[3],0,0,i)}throw b}}function
cu(b){try{var
c=a(cw[16],b);return c}catch(b){b=z(b);if(b===F)return[0,a(j[1][6],cv)];throw b}}function
a$(h,f,e){var
i=a(Z[70],e),j=i[2],c=i[1],k=a(h,cu(c));if(k){var
l=k[1],d=function(d,c){switch(a(l,d)){case
0:var
e=a(cx[1],f);return b(Q[45],e,c);case
1:return a(Q[24],c);default:return a$(h,f,c)}};if(a(g[19][35],j))return d(-1,c);var
m=b(g[19][16],d,j),n=[5,d(-1,c),m];return a(Q[25],n)}return a(Q[24],e)}function
ba(b,a){try{var
c=[0,e(g[17][115],j[63][1],a,b)];return c}catch(a){a=z(a);if(a===F)return 0;throw a}}var
aO=[0,g[15][53][1]];function
aP(b,a){aO[1]=e(g[15][53][4],b,a,aO[1]);return 0}function
cy(f){try{var
c=b(g[15][53][23],f,aO[1]);return c}catch(c){c=z(c);if(c===F){var
h=a(d[3],cz),i=a(d[20],f),j=a(d[3],cA),k=b(d[12],j,i),l=b(d[12],k,h);return e(Y[6],0,cB,l)}throw c}}function
aQ(i,g,f,d){function
j(a){return b(_[12],f,a)}var
k=a(h[b7][1],d),l=a(Q[33],0),m=e(Q[31],[0,j],Q[4],g),n=b(cy(i),f,d);function
c(d,f){var
b=a(Z[29],f);if(6===b[0]){var
h=b[2],i=b[1],j=c(d+1|0,b[3]),k=[0,i,c(d,h),j];return a(Z[12],k)}var
g=a$(n,d,f);return e(Q[49],m,l,g)}var
o=c(0,k);return a(h[9],o)}function
bb(a){var
b=0,c=2,d=[0,function(b,c,d){return aQ(a,b,c,d)},c];return e(bc[50],0,d,b)}function
bd(b,a){var
c=[0,[0,a,0]],d=2,f=[0,function(a,c,d){return aQ(b,a,c,d)},d];return e(bc[50],0,f,c)}function
aR(a,e,c){var
f=c;for(;;)try{var
m=b(ah[aM],a,f)[1],n=b(j[63][4][3],m,e);return n}catch(c){c=z(c);if(c===F){var
d=b(h[3],a,f);switch(d[0]){case
5:var
f=d[1];continue;case
9:var
k=d[2],i=aR(a,e,d[1]);if(i){var
l=function(b){return aR(a,e,b)};return b(g[19][21],l,k)}return i;default:return 0}}throw c}}var
cE=[0,function(c,s){if(c){var
f=c[2];if(f)if(!f[2]){var
h=f[1],i=a(v[2][2],c[1]),k=a(am[7],i),m=a(v[2][5],h),n=a(am[7],m),o=function(c){var
d=a(l[6],t[9]);return b(v[2][7],d,c)},p=b(g[17][68],o,n),r=function(c){if(aR(c,e(g[17][16],j[63][4][4],p,j[63][4][1]),k))return a(q[16],0);var
f=a(d[7],0);return b(cD[65][4],0,f)};return b(q[73][1],q[54],r)}}throw[0,aS,cC]}];e(bf[16],0,be,cE);var
cF=[0,be,0];function
bg(c){function
d(a){return[0,b(an[12],0,a)]}var
e=b(g[17][68],d,c),f=a(l[18],t[9]),h=a(l[5],f),i=[0,[0,b(l[7],h,e)],0],k=[1,a(j[1][6],cG)],m=[0,b(cH[3],0,k),0],n=a(l[5],t[11]),o=[0,cF,[0,[0,b(l[7],n,m)],i]],p=[31,b(O[1],0,o)];return[28,[0,[0,[0,a(j[1][6],cI)],0],p]]}function
bh(d){var
b=a(L[2],0),e=a($[17],b),c=ad(bi[10],b,e,0,d),f=c[1];return[0,a($[18],c[2]),f]}function
M(c){var
b=a(L[2],0),d=a($[17],b);return ad(bi[10],b,d,0,c)[1]}function
aa(f,d,c){var
g=a(cJ[27],c),h=a(L[5],0),i=e(cK[23],h,d,g);b(aT[14],0,i);var
k=a(L[2],0),l=b(cL[1],k,c)[2],m=[0,[0,jx(aT[2],0,cO,0,[0,l],[0,[0,cM[39][1]]],0,c)],cN],n=a(j[1][6],f),o=D(aT[3],0,0,n,0,m);return a(Z[17],o)}var
bj=[0,[0]],cS=[0,bk,0],cU=[0,function(d,c){var
e=a(g[17][5],d),f=a(l[6],t[3]),h=b(v[2][7],f,e);function
i(d){var
e=c[1],f=a(w[22],d),g=b(w[17],cT,f),h=a(j[1][6],g);return b(j[1][11][23],h,e)}bj[1]=b(g[19][2],h,i);return a(q[16],0)}];e(bf[16],0,bk,cU);function
bl(F,E,i,c,D){function
G(g,c){var
d=c[1],h=c[3],i=c[2],k=a(w[22],d),l=b(w[17],cV,k),f=a(j[1][6],l),m=[2,[1,b(O[1],0,f)]],n=a(v[2][1],g);return[0,d+1|0,[0,m,i],e(j[1][11][4],f,n,h)]}var
m=e(g[17][16],G,D,[0,0,0,j[1][11][1]]),H=m[3],I=m[2],r=a(v[31],0),J=[0,H,r[2],r[3]];function
K(c){var
d=a(w[22],c),e=b(w[17],cW,d);return a(j[1][6],e)}var
L=b(g[17][56],i,K),M=a(l[5],t[3]),N=[0,cS,[0,[0,b(l[7],M,i)],0]],P=[31,b(O[1],0,N)];function
Q(a){return[0,a]}var
R=[5,[28,[0,b(g[17][68],Q,L),P]]],A=h[16],B=a(cP[13],F),f=e(cQ[3][5],E,B,A),C=[0,f[1],f[3]],S=b(g[18],I,[0,R,0]),d=p(c),u=n===d?c[1]:k===d?a(o[2],c):c,x=[0,[0,b(an[12],0,u)],S],y=[3,b(O[1],0,x)],z=[29,b(O[1],0,y)],T=b(v[23],J,z),U=b(q[72][7],T,C),V=a(cX[2],U),s=a($[165],V);function
W(c){var
b=a(v[2][2],c);return b?e(h[5],0,s,b[1]):a(w[3],cR)}var
X=a($[150],s);return[0,b(g[19][15],W,bj[1]),X]}function
bm(b){return[k,function(e){var
c=a(P[2],b),d=a(aU[22],c);return a(h[9],d)}]}function
ao(b){return[k,function(c){return a(P[2],b)}]}var
cZ=bm(cY),aV=ao(c0),aW=ao(c1),R=bm(c2),ai=ao(c3),aj=ao(c4);function
ak(b,d){var
c=p(b),e=n===c?b[1]:k===c?a(o[2],b):b;return a(h[23],[0,e,d])}function
r(d,c,g){var
e=p(c),i=n===e?c[1]:k===e?a(o[2],c):c,f=b(_[9],d[1],i),j=f[2];d[1]=f[1];return a(h[23],[0,j,g])}function
bn(f,l){var
d=b(h[3],f,l);if(9===d[0]){var
c=d[2],m=d[1];if(2<=c.length-1){var
n=[0,m,e(g[19][7],c,0,c.length-1-2|0)],i=a(h[23],n);if(b(h[120][16],f,i)){var
j=c.length-1-1|0,k=c.length-1-2|0,o=E(c,j)[1+j];return[0,i,E(c,k)[1+k],o]}return K(c6)}}return K(c5)}var
aq=[0,c7,[0,ap,0]];function
c9(a){return b(g[18],aq,a)}var
bo=b(g[17][68],c9,c8);function
S(b){return[k,function(f){var
c=e(P[18],c_,bo,b),d=a(aU[22],c);return a(h[9],d)}]}function
m(a){return[k,function(b){return e(P[18],c$,bo,a)}]}var
dc=b(g[17][68],j[1][6],[0,db,[0,ap,da]]),dd=a(j[5][4],dc);function
bp(c){return[k,function(e){var
d=a(j[6][4],c);return b(j[13][1],[0,dd],d)}]}function
ae(a){var
b=[0,ap,df];return[k,function(c){return e(P[17],de,b,a)}]}var
ar=S(dg),di=m(dh),dk=m(dj),dm=m(dl),as=S(dn),dq=S(dp),at=S(dr),dt=S(ds),au=S(du),dw=S(dv),dy=S(dx),T=bp(dz),dB=m(dA),u=m(dC);function
bq(i,d,f){var
c=f;for(;;){var
e=b(h[3],d,c);if(6===e[0]){var
c=e[3];continue}var
j=bn(d,c)[1],l=function(c){var
b=c[1],e=c[2],d=p(b),f=n===d?b[1]:k===d?a(o[2],b):b;return[0,f,e]},m=b(g[17][68],l,i),q=function(a){return-1===a?1:2},r=[0,[0,cs(d,j),q],m];return function(a){return ba(r,a)}}}var
dD=0;function
dE(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
dG=[0,[0,ae(dF),dE],dD];function
dH(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
12:case
17:return 2;case
0:case
9:case
10:case
11:case
14:case
16:case
18:return 0}return 1}var
dJ=[0,[0,ae(dI),dH],dG];function
dK(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
dM=[0,[0,ae(dL),dK],dJ];function
dN(a){return 0}var
dP=[0,[0,m(dO),dN],dM];function
dQ(a){return 0}var
dS=[0,[0,m(dR),dQ],dP],dT=[0,[0,aj,function(a){return-1===a?0:1}],dS],dU=[0,[0,ai,function(a){return-1===a?0:2===a?2:1}],dT];aP(dV,function(a,b){return bq(dU,a,b)});var
U=a(g[21][1],[0,Z[86]]),av=e(br[4],0,dW,U[1]);function
bs(h){var
c=a(d[22],dX);b(af[7],0,c);var
f=av[1];function
g(v,c){var
f=a(L[2],0),g=a($[17],f),h=e(C[6],f,g,c[3]),i=a(d[3],dY),j=a(d[13],0),k=e(C[6],f,g,c[2]),l=a(d[3],dZ),m=a(d[13],0),n=a(G[6],c[1]),o=b(d[12],n,m),p=b(d[12],o,l),q=b(d[12],p,k),r=b(d[12],q,j),s=b(d[12],r,i),t=b(d[12],s,h),u=b(d[26],2,t);return b(af[7],0,u)}return b(U[11],g,f)}function
d0(a){return b(U[23],a,av[1])}function
d8(d){var
a=d[2],c=d[1],e=b(x[46],c,a[2]),f=b(x[46],c,a[3]),g=b(x[46],c,a[4]),h=b(x[46],c,a[5]),i=b(x[46],c,a[6]),j=b(x[46],c,a[7]),k=b(x[46],c,a[10]),l=b(x[46],c,a[11]),m=b(V[1],c,a[8]),n=b(V[1],c,a[9]),o=b(V[1],c,a[12]),p=b(V[1],c,a[13]);if(e===a[2])if(f===a[3])if(b(Z[80],g,a[4]))if(h===a[5])if(i===a[6])if(j===a[7])if(k===a[10])if(l===a[11])if(m===a[8])if(n===a[9])if(o===a[12])if(p===a[13])return a;return[0,a[1],e,f,g,h,i,j,m,n,k,l,o,p]}function
d9(b){var
a=b[2];av[1]=e(U[4],a[2],a,av[1]);return 0}var
d$=e(aw[16],d_,d9,[0,d8]),ea=a(aw[4],d$);function
bu(c,d,b,a){try{var
e=ad(ac[12],c,d[1],b,a),h=e[2],f=ad(ac[13],c,e[1],b,a),i=f[2],g=ad(ac[14],c,f[1],b,a),j=g[2];d[1]=g[1];var
k=ak(cZ,[0,b,a,h,i,j]);return k}catch(a){a=z(a);if(a===F)return K(eb);throw a}}function
ec(h,g,f,e,d,c,b,a){return ak(dq,[0,h,g,f,e,d,c,b,a])}function
ed(f,e,d,c,b,a){return ak(dt,[0,f,e,d,c,b,a])}function
ee(i,c,j){var
f=j[5],q=j[4],l=j[3],m=j[2],g=j[1],u=b(h[3],c[1],f);if(9===u[0])if(1===u[2].length-1){var
aU=u[1],E=p(R),aV=n===E?R[1]:k===E?a(o[2],R):R;if(e(h[X],c[1],aU,aV)){var
aW=r(c,di,[0,g]),aX=q?r(c,dk,[0,g,m,l,q[1]]):r(c,dm,[0,g,m,l]),G=e(ax[6],i,c[1],aW),aY=G[2],H=e(ax[6],i,G[1],aX),aZ=H[2];c[1]=H[1];return[0,aY,aZ]}}var
v=[0,[0,[0,[0,g,[0,f]]],[0,[0,[0,g,[0,f]]],0]],[0,[0,g,[0,f]]]],I=bu(a(L[2],0),c,g,f);try{var
aT=b(ac[15],v,m),w=aT}catch(a){a=z(a);if(a!==F)throw a;var
w=K(ef)}var
s=w[2];try{var
aS=b(ac[15],v,l),x=aS}catch(a){a=z(a);if(a!==F)throw a;var
x=K(eg)}var
t=x[2];if(q){var
y=q[1];try{var
at=b(ac[15],[0,[0,[0,[0,g,[0,f]]],0],[0,[0,g,[0,f]]]],y),A=at}catch(a){a=z(a);if(a!==F)throw a;var
A=K(eh)}var
B=A[2],J=ec(g,m,l,y,f,s,t,B),M=a(d[3],ei),N=e(C[11],i,c[1],B),O=a(d[3],ej),P=a(d[13],0),Q=a(d[3],ek),S=e(C[11],i,c[1],t),T=a(d[3],el),U=a(d[13],0),V=a(d[3],em),W=e(C[11],i,c[1],s),Y=a(d[3],en),Z=a(d[13],0),_=a(d[3],eo),$=e(C[11],i,c[1],f),aa=a(d[3],ep),ab=b(d[12],aa,$),ad=b(d[12],ab,_),ae=b(d[12],ad,Z),ag=b(d[12],ae,Y),ah=b(d[12],ag,W),ai=b(d[12],ah,V),aj=b(d[12],ai,U),ak=b(d[12],aj,T),al=b(d[12],ak,S),am=b(d[12],al,Q),an=b(d[12],am,P),ao=b(d[12],an,O),ap=b(d[12],ao,N),aq=b(d[12],ap,M),ar=af[6],as=function(a){return b(ar,0,a)};b(bv[20],as,aq);var
D=J}else{var
au=a(d[3],eq),av=e(C[11],i,c[1],t),aw=a(d[3],er),ay=a(d[13],0),az=a(d[3],es),aA=e(C[11],i,c[1],s),aB=a(d[3],et),aC=a(d[13],0),aD=a(d[3],eu),aE=e(C[11],i,c[1],f),aF=a(d[3],ev),aG=b(d[12],aF,aE),aH=b(d[12],aG,aD),aI=b(d[12],aH,aC),aJ=b(d[12],aI,aB),aK=b(d[12],aJ,aA),aL=b(d[12],aK,az),aM=b(d[12],aL,ay),aN=b(d[12],aM,aw),aO=b(d[12],aN,av),aP=b(d[12],aO,au),aQ=af[6],aR=function(a){return b(aQ,0,a)};b(bv[20],aR,aP);var
D=ed(g,m,l,f,s,t)}return[0,I,D]}function
bw(h,g,f,e,d,c,b,a){return a?a[1]:ee(h,g,[0,f,e,d,c,b])}function
bx(b){if(typeof
b==="number"){var
c=p(au);return n===c?au[1]:k===c?a(o[2],au):au}else
return 0===b[0]?ak(dw,[0,b[1]]):ak(dy,[0,b[1]])}function
by(u,t,s,r,q,d){if(d){var
c=d[1];if(0===c[0])return a(ag[2],c[1]);var
f=c[1],h=bz[3],i=function(a){return b(h,0,a)};return bg(b(g[17][68],i,f))}var
e=p(T),j=n===e?T[1]:k===e?a(o[2],T):T,l=[0,[0,b(an[12],0,j)],0],m=[3,b(O[1],0,l)];return[29,b(O[1],0,m)]}function
ay(c,b,a){return r(b,dB,[0,D(ab[2],0,0,c,b[1],a),a])}function
bA(d,c,m){var
f=p(u),q=n===f?u[1]:k===f?a(o[2],u):u,i=b(_[9],c[1],q),j=i[2];c[1]=i[1];var
s=r(c,aj,[0,j]);function
t(b,a){return r(c,ai,[0,j,ay(d,c,b),a])}var
v=e(g[17][16],t,m,s),l=e(ax[6],d,c[1],v),w=l[2];c[1]=l[1];var
x=a(h[b7][1],w);return b(_[41],c[1],x)}function
bB(q,c,e){var
f=p(u),s=n===f?u[1]:k===f?a(o[2],u):u,h=b(_[9],c[1],s),i=h[2];c[1]=h[1];if(e){var
j=e[1],d=j[1],t=j[2];if(0===d[0])var
l=a(ag[2],d[1]);else
var
v=d[1],w=bz[3],x=function(a){return b(w,0,a)},l=bg(b(g[17][68],x,v));return[0,l,r(c,aW,[0,i,ay(q,c,M(t))])]}var
m=p(T),y=n===m?T[1]:k===m?a(o[2],T):T,z=[0,b(an[12],0,y)],A=r(c,aV,[0,i]),B=[3,b(O[1],0,[0,z,0])];return[0,[29,b(O[1],0,B)],A]}function
bC(h,c,d){var
e=p(u),i=n===e?u[1]:k===e?a(o[2],u):u,f=b(_[9],c[1],i),g=f[2];c[1]=f[1];return d?r(c,aW,[0,g,ay(h,c,M(d[1]))]):r(c,aV,[0,g])}function
bD(h,c,d){var
e=p(u),i=n===e?u[1]:k===e?a(o[2],u):u,f=b(_[9],c[1],i),g=f[2];c[1]=f[1];return d?r(c,aW,[0,g,ay(h,c,M(d[1]))]):r(c,aV,[0,g])}function
bE(r,N,aw,M,av,J,au,ap,ao){var
O=J[2],Q=J[1],R=N[2],f=N[1],ax=b(g[18],aq,ez);a(P[12],ax);var
l=a(L[2],0),ad=D(ab[2],0,0,l,f,R),s=b(h[3],f,ad);if(9===s[0]){var
c=s[2],u=c.length-1-6|0;if(2<u>>>0)var
i=0;else{var
t=s[1];switch(u){case
0:var
ae=c[1],af=c[2],ah=c[3],ai=c[4],aj=c[5],ak=c[6],v=p(at),al=n===v?at[1]:k===v?a(o[2],at):at;if(e(h[X],f,t,al))var
d=[0,ex,ae,af,ah,ai,aj,0,0,ak],i=1;else
var
i=0;break;case
1:var
i=0;break;default:var
x=c[1],y=c[2],z=c[3],A=c[4],B=c[5],C=c[6],F=c[7],G=c[8],H=p(ar),am=n===H?ar[1]:k===H?a(o[2],ar):ar;if(e(h[X],f,t,am))var
d=[0,0,x,y,z,A,B,[0,C],[0,F],G],i=1;else{var
I=p(as),an=n===I?as[1]:k===I?a(o[2],as):as;if(e(h[X],f,t,an))var
d=[0,ey,x,y,z,A,B,[0,C],[0,F],G],i=1;else
var
i=0}}}}else
var
i=0;if(!i)var
d=K(ew);var
S=d[9],T=d[8],U=d[6],V=d[5],W=d[2],m=[0,f],ay=d[4],az=d[3],aA=d[1],Y=bw(l,m,W,V,U,T,S,aw),Z=Y[1],aB=Y[2],_=bB(l,m,au),aC=_[2],aD=_[1],aE=bC(l,m,ap),aF=bD(l,m,ao),aG=[0,Z,[0,aB,[0,R,[0,aC,[0,aE,[0,aF,[0,bx(M),0]]]]]]],aH=bp(eA),$=bl(l,m[1],5,aH,aG),ac=$[2],q=$[1],aI=E(q,3)[4],aJ=E(q,4)[5],aK=a(j[1][8],r),aL=aa(b(w[17],aK,eB),ac,aI),aM=a(j[1][8],r),aN=aa(b(w[17],aM,eC),ac,aJ),aO=by(l,f,M,aA,[0,az,ay,V,U,T],av),aP=Q?a(ag[2],Q[1]):eE,aQ=O?a(ag[2],O[1]):eD,aR=e(h[5],0,f,W),aS=e(h[5],0,f,S),aT=e(h[5],0,f,Z),aU=E(q,0)[1],aV=E(q,2)[3],aW=a(ea,[0,r,aR,aS,aT,E(q,1)[2],aV,aU,aO,aD,aL,aN,aP,aQ]);b(bF[7],r,aW);return 0}function
bG(a){return typeof
a==="number"?0:0===a[0]?[0,M(a[1])]:[1,M(a[1])]}function
s(e,c,d){return a(am[3],c[1])?(c[1]=[0,d],0):K(b(w[17],e,eF))}function
bH(q,p,o){var
l=bh(p),c=[0,0],d=[0,0],e=[0,0],f=[0,0],h=[0,0],i=[0,0],j=[0,0],k=[0,0],r=l[2],t=l[1];function
m(a){switch(a[0]){case
0:return s(eG,c,bG(a[1]));case
1:return s(eH,e,a[1]);case
2:return s(eI,f,a[1]);case
3:return s(eJ,h,a[1]);case
4:var
b=a[1],g=M(a[2]);return s(eK,d,[0,M(b),g]);case
5:return s(eL,j,[0,a[1],a[2]]);case
6:return s(eM,i,a[1]);default:return s(eN,k,a[1])}}b(g[17][11],m,o);var
a=c[1],n=a?a[1]:0;return bE(q,[0,t,r],d[1],n,e[1],[0,f[1],h[1]],j[1],i[1],k[1])}function
bI(d,a,c){if(a)return a;var
b=bn(d,c);return[0,b[2],[0,b[3],0]]}function
bJ(f,a,b,d){var
h=r(a,aj,[0,b]);function
i(d,c){return r(a,ai,[0,b,d,c])}var
j=e(g[17][16],i,d,h),c=e(ax[6],f,a[1],j),k=c[2];a[1]=c[1];return k}function
y(b){var
c=a(h[9],b);return a(v[2][1],c)}function
W(c){var
d=a(v[31],0);return b(v[2][6],d,c)}function
eO(a){var
b=y(a[3]),c=y(a[4]),d=y(a[5]),e=y(a[6]),f=y(a[7]),g=W(a[8]),h=W(a[9]),i=y(a[10]),j=y(a[11]),k=W([28,[0,eP,a[12]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,W([28,[0,eQ,a[13]]]),0]]]]]]]]]]]}function
bK(J,I,H,G){function
c(m){var
c=a(bL[35][4],m),f=a(q[68][4],m);try{var
i=bI(c,H,G),k=[0,c];if(i){var
n=i[2],j=D(ab[2],0,0,f,c,i[1]),o=function(g){var
h=D(ab[2],0,0,f,c,g),b=1-D(bt[81],0,f,c,j,h);if(b){var
i=a(d[3],d1);return e(Y[6],0,d2,i)}return b};b(g[17][11],o,n);try{var
E=d0(e(h[5],0,c,j)),l=E}catch(g){g=z(g);if(g!==F)throw g;var
p=a(d[3],d3),r=e(C[11],f,c,j),s=a(d[3],d4),t=a(d[13],0),u=a(d[3],d5),w=b(d[12],u,t),x=b(d[12],w,s),A=b(d[12],x,r),B=b(d[12],A,p),l=e(Y[6],0,d6,B)}var
K=bJ(f,k,a(h[9],l[2]),i),L=a(v[2][1],K),M=y(bA(f,k,I)),N=eO(l),O=b(g[18],N,[0,M,[0,L,0]]),P=b(v[2][8],J,O),Q=a(q[66][1],k[1]),R=b(q[18],Q,P);return R}throw[0,aS,d7]}catch(c){c=z(c);if(a(q[72][9],c))return b(q[21],0,c);throw c}}return a(q[68][8],c)}var
eT=b(g[17][68],j[1][6],[0,eS,[0,ap,eR]]),eU=a(j[5][4],eT),eV=0;function
eW(b){var
a=b+1|0;if(!(16<a>>>0))switch(a){case
11:case
15:return 2;case
0:case
13:case
16:return 0}return 1}var
eY=[0,[0,m(eX),eW],eV];function
eZ(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
e1=[0,[0,ae(e0),eZ],eY];function
e2(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
12:case
17:return 2;case
0:case
9:case
10:case
11:case
14:case
16:case
18:return 0}return 1}var
e4=[0,[0,ae(e3),e2],e1];function
e5(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
e7=[0,[0,ae(e6),e5],e4];function
e8(b){var
a=b+1|0;if(!(20<a>>>0))switch(a){case
13:case
18:return 2;case
0:case
10:case
11:case
12:case
15:case
17:case
19:case
20:return 0}return 1}var
e_=[0,[0,m(e9),e8],e7];function
e$(a){if(12<=a)if(15<=a)var
b=17<=a?0:1;else{if(13!==a)return 2;var
b=1}else
var
b=-1===a?1:9<=a?1:0;return b?0:1}var
fb=[0,[0,m(fa),e$],e_];function
fc(a){return 0}var
fe=[0,[0,m(fd),fc],fb];function
ff(a){return 0}var
fh=[0,[0,m(fg),ff],fe],fi=[0,[0,aj,function(a){return-1===a?0:1}],fh],fj=[0,[0,ai,function(a){return-1===a?0:2===a?2:1}],fi];aP(fk,function(a,b){return bq(fj,a,b)});var
fl=0;function
fm(b){var
a=b+1|0;if(!(15<a>>>0))switch(a){case
10:case
14:return 2;case
0:case
12:case
15:return 0}return 1}var
fo=[0,[0,m(fn),fm],fl];function
fp(a){return 0}var
fr=[0,[0,m(fq),fp],fo];function
fs(a){return 0}var
fu=[0,[0,m(ft),fs],fr],fv=[0,[0,aj,function(a){return-1===a?0:1}],fu],fw=[0,[0,ai,function(a){return-1===a?0:2===a?2:1}],fv];aP(fx,function(e,f){function
c(c){var
b=c[1],e=c[2],d=p(b),f=n===d?b[1]:k===d?a(o[2],b):b;return[0,f,e]}var
d=b(g[17][68],c,fw);return function(a){return ba(d,a)}});function
fz(a,b,c){return aQ(fy,a,b,c)}b(fB[3],fA,fz);var
az=m(fC),aA=m(fD),aB=m(fE),fG=m(fF),fI=m(fH),fK=m(fJ),aC=e(br[4],0,fO,U[1]);function
bM(h){var
c=a(d[22],fP);b(af[7],0,c);var
f=aC[1];function
g(w,c){var
f=a(L[2],0),g=a($[17],f),h=e(C[6],f,g,c[3]),i=a(d[3],fQ),k=a(d[13],0),l=e(C[6],f,g,c[2]),m=a(d[3],fR),n=a(d[13],0),o=a(j[1][9],c[1]),p=b(d[12],o,n),q=b(d[12],p,m),r=b(d[12],q,l),s=b(d[12],r,k),t=b(d[12],s,i),u=b(d[12],t,h),v=b(d[26],2,u);return b(af[7],0,v)}return b(U[11],g,f)}function
fS(a){return b(U[23],a,aC[1])}function
f1(d){var
a=d[2],c=d[1],e=b(x[46],c,a[2]),f=b(x[46],c,a[3]),g=b(x[46],c,a[6]),h=b(x[46],c,a[7]),i=b(x[46],c,a[8]),j=b(x[46],c,a[9]),k=b(x[46],c,a[10]),l=b(V[1],c,a[4]),m=b(V[1],c,a[5]),n=b(V[1],c,a[11]),o=b(V[1],c,a[12]);if(e===a[2])if(f===a[3])if(g===a[6])if(h===a[7])if(i===a[8])if(j===a[9])if(k===a[10])if(l===a[4])if(m===a[5])if(n===a[11])if(o===a[12])return a;return[0,a[1],e,f,l,m,g,h,i,j,k,n,o]}function
f2(b){var
a=b[2];aC[1]=e(U[4],a[2],a,aC[1]);return 0}var
f4=e(aw[16],f3,f2,[0,f1]),f5=a(aw[4],f4);function
f6(f,c,i,d){var
g=b(h[3],f[1],d);if(9===g[0])if(1===g[2].length-1){var
r=g[1],l=p(R),s=n===l?R[1]:k===l?a(o[2],R):R;if(e(h[X],f[1],r,s)){var
t=a(P[2],f8),u=a(aU[22],t),v=[0,a(h[9],u),[0,c,c,i]];return a(h[23],v)}}bu(a(L[2],0),f,c,d);var
m=[0,[0,[0,[0,c,[0,d]]],0],[0,[0,c,[0,d]]]];try{var
q=b(ac[15],m,i),j=q}catch(a){a=z(a);if(a!==F)throw a;var
j=K(f7)}return j[2]}function
bN(i,bs,br){var
at=[0,0],au=[0,0],av=[0,0],aw=[0,0],ax=[0,0],ay=[0,0],aC=[0,0],aD=[0,0],aE=[0,0];function
bp(b){if(0===b[0]){var
a=b[1];switch(a[0]){case
0:return s(gh,at,bG(a[1]));case
1:return s(gi,av,a[1]);case
2:return s(gj,aw,a[1]);case
3:return s(gk,ax,a[1]);case
4:var
c=a[1],d=M(a[2]);return s(gl,au,[0,M(c),d]);case
5:return s(gm,aD,[0,a[1],a[2]]);case
6:return s(gn,aC,a[1]);default:return s(go,aE,a[1])}}return s(gp,ay,M(b[1]))}b(g[17][11],bp,br);var
aF=at[1],R=aF?aF[1]:0,aG=aE[1],aH=aC[1],aI=aD[1],aJ=ax[1],aK=aw[1],aL=av[1],aM=ay[1],bq=au[1],aS=b(g[18],aq,f9);a(P[12],aS);var
ai=bh(bs),t=ai[2],u=ai[1],m=a(L[2],0),d=[0,u],aO=D(ab[2],0,0,m,d[1],t),J=b(h[3],d[1],aO);if(9===J[0]){var
c=J[2],S=c.length-1-8|0;if(2<S>>>0)var
l=0;else{var
N=J[1];switch(S){case
0:var
T=c[1],U=c[2],V=c[3],W=c[4],Y=c[5],_=c[6],$=c[7],ac=c[8],ad=p(aB),aP=n===ad?aB[1]:k===ad?a(o[2],aB):aB;if(e(ah[X],d[1],aP,N))var
f=[0,fM,T,U,V,W,Y,0,0,_,$,ac,r(d,fK,[0,T,U,V,W,Y,_,$,ac,t])],l=1;else
var
l=0;break;case
1:var
l=0;break;default:var
x=c[1],y=c[2],z=c[3],A=c[4],B=c[5],C=c[6],F=c[7],G=c[8],H=c[9],I=c[10],ae=p(az),aQ=n===ae?az[1]:k===ae?a(o[2],az):az;if(e(ah[X],d[1],aQ,N))var
f=[0,0,x,y,z,A,B,[0,C],[0,F],G,H,I,r(d,fG,[0,x,y,z,A,B,C,F,G,H,I,t])],l=1;else{var
af=p(aA),aR=n===af?aA[1]:k===af?a(o[2],aA):aA;if(e(ah[X],d[1],aR,N))var
f=[0,fN,x,y,z,A,B,[0,C],[0,F],G,H,I,r(d,fI,[0,x,y,z,A,B,C,F,G,H,I,t])],l=1;else
var
l=0}}}}else
var
l=0;if(!l)var
f=K(fL);var
O=f[11],aj=f[8],ak=f[6],al=f[5],Q=f[2],aT=f[12],aU=f[10],aV=f[4],aW=f[3],aX=f[1],am=bw(m,d,Q,al,ak,aj,O,bq),an=am[2],ao=am[1];bE(i,[0,d[1],aT],[0,[0,ao,an]],R,aL,f_,aI,aH,aG);var
ap=bB(m,d,aI),aY=ap[2],aZ=ap[1],a0=bC(m,d,aH),a1=bD(m,d,aG),a2=f6(d,Q,aU,O),a3=[0,ao,[0,an,[0,a2,[0,t,[0,aY,[0,a0,[0,a1,[0,bx(R),0]]]]]]]],aN=[k,function(d){var
c=a(j[6][4],f$);return b(j[13][1],[0,eU],c)}],ar=bl(m,d[1],9,aN,a3),v=ar[2],q=ar[1],a4=E(q,3)[4],a5=E(q,4)[5],a6=E(q,5)[6],a7=E(q,6)[7];if(aM)var
a8=[0,e(h[5],0,u,aM[1])],a9=[0,E(q,8)[9],a8],as=a(Z[15],a9);else
var
as=E(q,7)[8];var
a_=a(j[1][8],i),a$=aa(b(w[17],a_,ga),v,a4),ba=a(j[1][8],i),bb=aa(b(w[17],ba,gb),v,a5),bc=a(j[1][8],i),bd=aa(b(w[17],bc,gc),v,a6),be=a(j[1][8],i),bf=aa(b(w[17],be,gd),v,a7),bg=a(j[1][8],i),bi=aa(b(w[17],bg,ge),v,as),bj=by(m,u,R,aX,[0,aW,aV,al,ak,aj],aL),bk=aK?a(ag[2],aK[1]):gg,bm=aJ?a(ag[2],aJ[1]):gf,bn=e(h[5],0,u,Q),bo=a(f5,[0,i,bn,e(h[5],0,u,O),bj,aZ,a$,bb,bd,bf,bi,bk,bm]);b(bF[7],i,bo);return 0}function
gq(a){var
b=y(a[3]),c=W(a[4]),d=W(a[5]),e=y(a[6]),f=y(a[8]),g=y(a[7]),h=y(a[9]),i=y(a[10]),j=W([28,[0,gr,a[11]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,W([28,[0,gs,a[12]]]),0]]]]]]]]]]}function
bO(K,J,I,H){function
c(m){var
c=a(bL[35][4],m),f=a(q[68][4],m);try{var
i=bI(c,I,H),k=[0,c],n=b(g[18],aq,fT);a(P[12],n);if(i){var
o=i[2],j=D(ab[2],0,0,f,c,i[1]),p=function(g){var
h=D(ab[2],0,0,f,c,g),b=1-D(bt[81],0,f,c,j,h);if(b){var
i=a(d[3],fU);return e(Y[6],0,fV,i)}return b};b(g[17][11],p,o);try{var
G=fS(e(h[5],0,c,j)),l=G}catch(g){g=z(g);if(g!==F)throw g;var
r=a(d[3],fW),s=e(C[11],f,c,j),t=a(d[3],fX),u=a(d[13],0),w=a(d[3],fY),x=b(d[12],w,u),A=b(d[12],x,t),B=b(d[12],A,s),E=b(d[12],B,r),l=e(Y[6],0,fZ,E)}var
L=bJ(f,k,a(h[9],l[2]),i),M=a(v[2][1],L),N=y(bA(f,k,J)),O=gq(l),Q=b(g[18],O,[0,N,[0,M,0]]),R=b(v[2][8],K,Q),S=a(q[66][1],k[1]),T=b(q[18],S,R);return T}throw[0,aS,f0]}catch(c){c=z(c);if(a(q[72][9],c))return b(q[21],0,c);throw c}}return a(q[68][8],c)}aH(307,[0,bd,bb,bH,bs,bK,bN,bM,bO],"Newring_plugin__Newring");a(gt[9],al);var
gu=0;function
gv(a,b){return bb(a)}var
gx=[0,[0,[0,gw,[1,[5,a(l[16],t[4])],0]],gv],gu];function
gy(b,a,c){return bd(b,a)}var
gA=[0,gz,[1,[5,a(l[16],t[7])],0]],gC=[0,[0,[0,gB,[1,[5,a(l[16],t[4])],gA]],gy],gx];D(aX[8],al,gD,0,0,gC);function
aD(g,f,c){switch(c[0]){case
0:var
h=c[1];if(typeof
h==="number")return a(d[3],gE);else{if(0===h[0]){var
k=h[1],l=b(G[16],g,f),m=b(d[32],l,k),n=a(d[3],gF);return b(d[12],n,m)}var
o=h[1],p=b(G[16],g,f),q=b(d[32],p,o),r=a(d[3],gG);return b(d[12],r,q)}case
1:var
i=c[1];if(0===i[0]){var
s=i[1],t=a(d[3],gH),u=e(aE[22],g,f,s),v=a(d[3],gI),w=a(d[13],0),x=a(d[3],gJ),y=b(d[12],x,w),z=b(d[12],y,v),A=b(d[12],z,u);return b(d[12],A,t)}var
B=i[1],C=a(d[3],gK),D=e(d[39],d[13],G[7],B),E=a(d[3],gL),F=a(d[13],0),H=a(d[3],gM),I=b(d[12],H,F),J=b(d[12],I,E),K=b(d[12],J,D);return b(d[12],K,C);case
2:var
L=c[1],M=a(d[3],gN),N=e(aE[22],g,f,L),O=a(d[3],gO),P=a(d[13],0),Q=a(d[3],gP),R=b(d[12],Q,P),S=b(d[12],R,O),T=b(d[12],S,N);return b(d[12],T,M);case
3:var
U=c[1],V=a(d[3],gQ),W=e(aE[22],g,f,U),X=a(d[3],gR),Y=a(d[13],0),Z=a(d[3],gS),_=b(d[12],Z,Y),$=b(d[12],_,X),aa=b(d[12],$,W);return b(d[12],aa,V);case
4:var
ab=c[2],ac=c[1],ad=b(G[16],g,f),ae=b(d[32],ad,ab),af=b(G[16],g,f),ag=b(d[32],af,ac),ah=a(d[3],gT),ai=b(d[12],ah,ag);return b(d[12],ai,ae);case
5:var
j=c[1];if(0===j[0]){var
aj=c[2],ak=j[1],al=a(d[3],gU),am=e(aE[22],g,f,ak),an=a(d[3],gV),ao=a(d[13],0),ap=b(G[16],g,f),aq=b(d[32],ap,aj),ar=a(d[3],gW),as=b(d[12],ar,aq),at=b(d[12],as,ao),au=b(d[12],at,an),av=b(d[12],au,am);return b(d[12],av,al)}var
aw=c[2],ax=j[1],ay=a(d[3],gX),az=e(d[39],d[13],G[7],ax),aA=a(d[3],gY),aB=a(d[13],0),aC=b(G[16],g,f),aD=b(d[32],aC,aw),aF=a(d[3],gZ),aG=b(d[12],aF,aD),aH=b(d[12],aG,aB),aI=b(d[12],aH,aA),aJ=b(d[12],aI,az);return b(d[12],aJ,ay);case
6:var
aK=c[1],aL=b(G[16],g,f),aM=b(d[32],aL,aK),aN=a(d[3],g0);return b(d[12],aN,aM);default:var
aO=c[1],aP=b(G[16],g,f),aQ=b(d[32],aP,aO),aR=a(d[3],g1);return b(d[12],aR,aQ)}}var
g2=0;function
g3(a,c,b){return[0,[0,a]]}var
g4=[6,I[16][1]],g6=[0,[0,[0,[0,0,[0,a(i[10],g5)]],g4],g3],g2];function
g7(b,a){return g8}var
g_=[0,[0,[0,0,[0,a(i[10],g9)]],g7],g6];function
g$(a,c,b){return[0,[1,a]]}var
ha=[6,I[16][1]],hc=[0,[0,[0,[0,0,[0,a(i[10],hb)]],ha],g$],g_];function
hd(e,a,d,c,b){return[1,[0,a]]}var
hf=[0,a(i[10],he)],hg=[6,aF[18]],hi=[0,a(i[10],hh)],hk=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hj)]],hi],hg],hf],hd],hc];function
hl(e,a,d,c,b){return[1,[1,a]]}var
hn=[0,a(i[10],hm)],ho=[1,[6,I[16][7]]],hq=[0,a(i[10],hp)],hs=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hr)]],hq],ho],hn],hl],hk];function
ht(e,a,d,c,b){return[2,a]}var
hv=[0,a(i[10],hu)],hw=[6,aF[18]],hy=[0,a(i[10],hx)],hA=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hz)]],hy],hw],hv],ht],hs];function
hB(e,a,d,c,b){return[3,a]}var
hD=[0,a(i[10],hC)],hE=[6,aF[18]],hG=[0,a(i[10],hF)],hI=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hH)]],hG],hE],hD],hB],hA];function
hJ(b,a,d,c){return[4,a,b]}var
hK=[6,I[16][1]],hL=[6,I[16][1]],hN=[0,[0,[0,[0,[0,0,[0,a(i[10],hM)]],hL],hK],hJ],hI];function
hO(a,c,b){return[6,a]}var
hP=[6,I[16][1]],hR=[0,[0,[0,[0,0,[0,a(i[10],hQ)]],hP],hO],hN];function
hS(f,b,e,a,d,c){return[5,[1,b],a]}var
hU=[0,a(i[10],hT)],hV=[1,[6,I[16][7]]],hX=[0,a(i[10],hW)],hY=[6,I[16][1]],h0=[0,[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hZ)]],hY],hX],hV],hU],hS],hR];function
h1(f,b,e,a,d,c){return[5,[0,b],a]}var
h3=[0,a(i[10],h2)],h4=[6,aF[18]],h6=[0,a(i[10],h5)],h7=[6,I[16][1]],h9=[0,[0,[0,[0,[0,[0,[0,0,[0,a(i[10],h8)]],h7],h6],h4],h3],h1],h0];function
h_(a,c,b){return[7,a]}var
h$=[6,I[16][1]],ib=[1,[0,[0,[0,[0,0,[0,a(i[10],ia)]],h$],h_],h9]],ic=[0,function(b,a){return function(c){return aD(b,a,c)}},ib],bP=b(N[3],id,ic),aY=bP[2],ie=bP[1];function
bQ(f,c,b){function
g(a){return aD(f,c,a)}var
h=e(d[39],d[28],g,b);return a(d[46],h)}var
ig=0;function
ih(d,a,c,b){return a}var
ij=[0,a(i[10],ii)],il=[2,[6,aY],[0,a(i[10],ik)]],io=[1,[0,[0,[0,[0,[0,0,[0,a(i[10],im)]],il],ij],ih],ig]],ip=[0,function(b,a){return function(c){return bQ(b,a,c)}},io],bR=b(N[3],iq,ip),bS=bR[1],ir=bR[2],is=0,it=[0,N[5]],iv=[0,[0,0,iu,function(c,b){a(aG[2],c);bs(0);return b},it],is],iw=0;function
ix(g,f,e,d,c){a(aG[2],d);bH(g,f,b(am[23],0,e));return c}var
iy=[1,[4,[5,a(l[16],bS)]],0],iA=[0,iz,[1,[5,a(l[16],t[11])],iy]],iD=[0,[0,0,[0,iC,[0,iB,[1,[5,a(l[16],t[7])],iA]]],ix,iw],iv],iE=0,iF=[0,function(a){return N[6]}];ad(N[2],iG,iF,iE,iD);var
iH=0;function
iI(e,d,c,f){var
b=a(g[17][aM],c);return bK(e,d,b[2],b[1])}var
iK=[0,iJ,[1,[0,[5,a(l[16],t[11])]],0]],iM=[0,iL,[1,[2,[5,a(l[16],t[11])]],iK]],iO=[0,[0,[0,iN,[1,[6,a(l[16],bT[9]),0],iM]],iI],iH];D(aX[8],al,iP,0,0,iO);function
aZ(f,e,c){if(0===c[0])return aD(f,e,c[1]);var
g=c[1],h=b(G[16],f,e),i=b(d[32],h,g),j=a(d[3],iQ);return b(d[12],j,i)}var
iR=0,iS=[0,[0,[0,0,[6,aY]],function(a,b){return[0,a]}],iR];function
iT(a,c,b){return[1,a]}var
iU=[6,I[16][1]],iW=[1,[0,[0,[0,[0,0,[0,a(i[10],iV)]],iU],iT],iS]],iX=[0,function(b,a){return function(c){return aZ(b,a,c)}},iW],bU=b(N[3],iY,iX),bV=bU[2],iZ=bU[1];function
bW(f,c,b){function
g(a){return aZ(f,c,a)}var
h=e(d[39],d[28],g,b);return a(d[46],h)}var
i0=0;function
i1(d,a,c,b){return a}var
i3=[0,a(i[10],i2)],i5=[2,[6,bV],[0,a(i[10],i4)]],i7=[1,[0,[0,[0,[0,[0,0,[0,a(i[10],i6)]],i5],i3],i1],i0]],i8=[0,function(b,a){return function(c){return bW(b,a,c)}},i7],bX=b(N[3],i9,i8),bY=bX[1],i_=bX[2],i$=0,ja=[0,N[5]],jc=[0,[0,0,jb,function(c,b){a(aG[2],c);bM(0);return b},ja],i$],jd=0;function
je(g,f,b,e,d){a(aG[2],e);var
c=b?b[1]:0;bN(g,f,c);return d}var
jf=[1,[4,[5,a(l[16],bY)]],0],jh=[0,jg,[1,[5,a(l[16],t[11])],jf]],jk=[0,[0,0,[0,jj,[0,ji,[1,[5,a(l[16],t[7])],jh]]],je,jd],jc],jl=0,jm=[0,function(a){return N[6]}];ad(N[2],jn,jm,jl,jk);var
jo=0;function
jp(e,d,c,f){var
b=a(g[17][aM],c);return bO(e,d,b[2],b[1])}var
jr=[0,jq,[1,[0,[5,a(l[16],t[11])]],0]],jt=[0,js,[1,[2,[5,a(l[16],t[11])]],jr]],jv=[0,[0,[0,ju,[1,[5,a(l[16],bT[9])],jt]],jp],jo];D(aX[8],al,jw,0,0,jv);aH(317,[0,al,aD,ie,aY,bQ,bS,ir,aZ,iZ,bV,bW,bY,i_],"Newring_plugin__G_newring");return}
