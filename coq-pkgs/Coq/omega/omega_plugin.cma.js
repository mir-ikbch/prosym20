function(li){"use strict";var
dL="*",cP=-16,dV=" 0\n",cO="Equation E",cN=" and E",dd=": ",aV=".\n",d=246,dR="(",dU="not a number",H=123,dK="+ ",dO="- ",dH="------------------------\n\n",aW="omega",dN="Inequation E",dT="Z",db="_vendor+v8.10+32bit/coq/plugins/omega/coq_omega.ml",dc=103,dS=")",dZ="N",dQ="with",dY="X%d",dM=" subsumes E",df=" E",aN="Omega",dX=" states ",de=-18,dJ="positive",dI="Equations E",Z=144,dW="nat",d1="Omega: Can't solve a goal with non-linear products",aO=248,aM=100,dP="Coq",g=250,d0="E%d subsumes E%d.\n",E=li.jsoo_runtime,aU=E.caml_check_bound,dF=E.caml_equal,aL=E.caml_fresh_oo_id,b=E.caml_new_string,cJ=E.caml_notequal,f=E.caml_obj_tag,cH=E.caml_register_global,cM=E.caml_string_notequal,cL=E.caml_trampoline,cK=E.caml_trampoline_return,t=E.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):E.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):E.caml_call_gen(a,[b,c])}function
n(a,b,c,d){return a.length==3?a(b,c,d):E.caml_call_gen(a,[b,c,d])}function
cI(a,b,c,d,e){return a.length==4?a(b,c,d,e):E.caml_call_gen(a,[b,c,d,e])}function
dG(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):E.caml_call_gen(a,[b,c,d,e,f])}function
lg(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
lh(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
r=E.caml_get_global_data(),lf=[11,b(" + "),[2,0,[12,32,[2,0,[11,b(df),[4,0,0,0,[11,b(aV),0]]]]]]],dB=[0,[0,2],[0,0,0]],cF=b("omega_plugin"),o=r.Stdlib__list,w=r.Stdlib__printf,m=r.Stdlib,p=r.Util,I=r.Not_found,ae=r.Int,v=r.Stdlib__hashtbl,ba=r.Assert_failure,D=r.Names,e=r.CamlinternalLazy,h=r.EConstr,C=r.Bigint,ac=r.Pp,_=r.CErrors,aD=r.Logic,dq=r.Coqlib,j=r.Tactics,k=r.Tacticals,q=r.Proofview,z=r.Context,a0=r.Nameops,F=r.Tacmach,c4=r.Reductionops,dw=r.Refine,c2=r.Nametab,c3=r.Libnames,cY=r.Tacred,aY=r.Goptions,dE=r.Ltac_plugin__Tacentries,kH=r.Termops,j0=r.Contradiction,j5=r.Equality,ij=r.Evarutil,hO=r.Global,hP=r.Evd,fm=r.UnivGen,kS=r.Stdlib__string,kQ=r.Ltac_plugin__Tacenv,kR=r.Ltac_plugin__Tacinterp,kO=r.Mltop,k$=r.Stdarg,la=r.Genarg;cH(389,[0,0,0,0],"Omega_plugin");var
aE=[0,0],el=[0,[11,b(dN),[4,0,0,0,[11,b(" is divided by "),[2,0,[11,b(" and the constant coefficient is rounded by subtracting "),[2,0,[11,b(aV),0]]]]]]],b("Inequation E%d is divided by %s and the constant coefficient is rounded by subtracting %s.\n")],em=[0,[11,b("Constant in equation E"),[4,0,0,0,[11,b(" is not divisible by the pgcd "),[2,0,[11,b(" of its other coefficients.\n"),0]]]]],b("Constant in equation E%d is not divisible by the pgcd %s of its other coefficients.\n")],en=[0,[12,69,[4,0,0,0,[11,b(" is trivially satisfiable.\n"),0]]],b("E%d is trivially satisfiable.\n")],eo=[0,[11,b(cO),[4,0,0,0,[11,b(" is divided by the pgcd "),[2,0,[11,b(" of its coefficients.\n"),0]]]]],b("Equation E%d is divided by the pgcd %s of its coefficients.\n")],ep=[0,[11,b("We state "),[2,0,[11,b(df),[4,0,0,0,[11,b(" = "),[2,0,[12,32,[2,0,[11,b(df),[4,0,0,0,lf]]]]]]]]]],b("We state %s E%d = %s %s E%d + %s %s E%d.\n")],eq=[0,[11,b("We define a new equation E"),[4,0,0,0,[11,b(dd),0]]],b("We define a new equation E%d: ")],er=b(" 0"),es=[0,[11,b("We define E"),[4,0,0,0,[11,b(dd),0]]],b("We define E%d: ")],et=b(dV),eu=[0,[12,69,[4,0,0,0,[11,b(dM),[4,0,0,0,[11,b(aV),0]]]]],b(d0)],ev=[0,[12,69,[4,0,0,0,[11,b(dM),[4,0,0,0,[11,b(aV),0]]]]],b(d0)],ew=[0,[11,b(dI),[4,0,0,0,[11,b(cN),[4,0,0,0,[11,b(" imply a contradiction on their constant factors.\n"),0]]]]],b("Equations E%d and E%d imply a contradiction on their constant factors.\n")],ex=[0,[11,b(dI),[4,0,0,0,[11,b(cN),[4,0,0,0,[11,b(" state that their body is at the same time equal and different\n"),0]]]]],b("Equations E%d and E%d state that their body is at the same time equal and different\n")],ey=[0,[12,69,[4,0,0,0,[11,b(cN),[4,0,0,0,[11,b(" can be merged into E"),[4,0,0,0,[11,b(aV),0]]]]]]],b("E%d and E%d can be merged into E%d.\n")],ez=[0,[11,b(cO),[4,0,0,0,[11,b(dX),[2,0,[11,b(" = 0.\n"),0]]]]],b("Equation E%d states %s = 0.\n")],eA=[0,[11,b(dN),[4,0,0,0,[11,b(" states 0 != 0.\n"),0]]],b("Inequation E%d states 0 != 0.\n")],eB=[0,[11,b(cO),[4,0,0,0,[11,b(dX),[2,0,[11,b(" >= 0.\n"),0]]]]],b("Equation E%d states %s >= 0.\n")],eC=[0,[11,b(cO),[4,0,0,0,[11,b(" is split in E"),[4,0,0,0,[11,b(cN),[4,0,0,0,[11,b("\n\n"),0]]]]]]],b("Equation E%d is split in E%d and E%d\n\n")],eD=[0,[11,b("To ensure a solution in the dark shadow the equation E"),[4,0,0,0,[11,b(" is weakened by "),[2,0,[11,b(aV),0]]]]],b("To ensure a solution in the dark shadow the equation E%d is weakened by %s.\n")],eN=b("depend"),eQ=b("solve"),eO=[0,b("_vendor+v8.10+32bit/coq/plugins/omega/omega.ml"),602,15],eM=b("disequation in simplify"),eL=b("Product dardk"),eK=[0,0,0,0],eI=b("TL"),eH=b("eliminate_with_in"),eE=[0,[12,88,[4,0,0,0,0]],b(dY)],ej=b(">= 0\n"),ek=b(dH),eg=[0,[12,69,[4,0,0,0,[11,b(dd),0]]],b("E%d: ")],eh=[0,[2,0,[11,b(dV),0]],b("%s 0\n")],ei=b(dH),ed=b("equation"),ee=b("inequation"),ef=b("disequation"),ea=b("="),eb=b(">="),ec=b("!="),d5=b(dO),d8=b(dK),d9=b(""),d6=[0,[2,0,[12,32,0]],b("%s ")],d7=[0,[2,0,[12,32,[2,0,[12,32,0]]]],b("%s %s ")],d_=[0,[11,b(dK),[2,0,[12,32,0]]],b("+ %s ")],d$=[0,[11,b(dO),[2,0,[12,32,0]]],b("- %s ")],d2=b("pgcd_l"),d3=b("Omega_plugin.Omega.MakeOmegaSolver(I).UNSOLVABLE"),d4=b("Omega_plugin.Omega.MakeOmegaSolver(I).NO_CONTRADICTION"),eF=b("Omega_plugin.Omega.MakeOmegaSolver(I).FACTOR1"),eG=b("Omega_plugin.Omega.MakeOmegaSolver(I).CHOPVAR"),eJ=b("Omega_plugin.Omega.MakeOmegaSolver(I).SOLVED_SYSTEM"),eP=b("Omega_plugin.Omega.MakeOmegaSolver(I).FULL_SOLUTION"),h$=b(dR),ia=b("+"),ib=b(dS),ic=b(dR),id=b(dL),ie=b(dS),ig=b("?"),ih=b("weight"),iw=[0,2],ix=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],iy=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],is=[0,2],it=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],iu=[0,2],iv=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],iz=[0,2],iA=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],iB=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],i3=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],[0,[0,[0,1],[0,[0,1],[0,[0,2],[0,[0,1],0]]]],0]]]],i4=[0,1],i5=[0,2],i6=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],i7=b(d1),i8=[0,2],i9=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jf=[0,1],jg=[0,2],jh=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],ji=b(d1),jj=[0,2],jk=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],jl=[0,[0,[0,1],0],0],jm=[0,1],jn=[0,2],jo=[0,1],jp=[0,2],jq=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],jr=[0,1],jI=[0,0,0],jF=[0,1],jG=[0,2],jB=[0,1],jC=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jD=[0,1],jE=[0,2],jH=[0,1],jK=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jJ=[0,2],kM=[0,b(dP),[0,b(aW),[0,b(aN),0]]],kG=[0,0],kF=[0,0],kI=b("_eqn"),kC=b("_left"),kD=b("_right"),kl=[0,1],ke=[0,2],kf=[0,1],kg=[0,2],kh=[0,1],ki=[0,2],kj=[0,1],kk=[0,1],km=[0,[0,3],[0,0,0]],kn=[0,[0,2],[0,0,0]],ko=[0,[0,2],[0,0,0]],kp=[0,[0,1],[0,0,0]],kq=[0,[0,2],[0,0,0]],kr=[0,[0,1],[0,0,0]],ks=[0,[0,2],[0,0,0]],kt=[0,[0,1],[0,0,0]],ku=[0,[0,2],[0,0,0]],kv=[0,[0,1],[0,0,0]],kw=[0,[0,2],[0,0,0]],kx=[0,[0,1],[0,0,0]],ka=[0,0,0],kb=b("Omega can't solve this system"),j_=b("State"),j8=[0,0,0],jO=[0,[0,3],0],jP=[0,[0,2],0],jQ=[0,[0,3],0],jR=[0,[0,3],0],jS=[0,[0,1],[0,0,0]],jT=[0,[0,2],[0,0,0]],jU=[0,[0,2],[0,0,0]],jV=[0,[0,[0,1],0],0],jW=[0,2],jX=[0,1],jY=[0,1],jZ=[0,[0,2],[0,0,0]],j1=[0,[0,1],[0,0,0]],j2=[0,[0,3],0],j3=[0,[0,[0,1],0],0],j4=[0,[0,3],0],j6=[0,[0,2],[0,0,0]],j7=[0,[0,2],[0,0,0]],jL=b("auxiliary"),jM=b("auxiliary_1"),jN=b("auxiliary_2"),jy=b("condense.1"),jz=[0,2],jA=[0,0,0],jx=b("reduce_factor.1"),jt=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],ju=[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]],jv=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,2],0]],0]],jw=[0,[0,[0,1],0],0],js=b("shrink.1"),jd=[0,2],je=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jb=[0,2],jc=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],i_=[0,2],i$=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]]],iQ=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]],iR=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],iS=[0,1],iT=[0,2],iU=[0,2],iV=[0,2],iW=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],iX=[0,2],iY=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],iZ=[0,2],i0=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],i1=[0,2],i2=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],iD=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]]],iE=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],iF=[0,1],iG=[0,2],iH=[0,2],iI=[0,2],iJ=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],iK=[0,2],iL=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],iM=[0,2],iN=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],iO=[0,2],iP=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],iq=[0,b(db),684,17],ip=[0,b(db),685,13],ir=[0,0],io=[0,b(db),644,9],ik=b("H"),il=b("P"),ii=b("compile_equation."),h_=[0,0],h9=b("x"),h8=b("occurrence "),h7=b("abstract_path "),h5=b(dU),h6=b(dU),h2=b("Omega: Not a quantifier-free goal"),h0=b("not"),hZ=b("Z.gt"),hY=b("Z.le"),hW=b("Z.sub"),hU=b("Z.pred"),hS=b("Z.succ"),hQ=b(" is not an evaluable constant."),hR=[0,b("Coq_omega")],fk=b("find_contr"),fi=b("tag_hypothesis"),fg=[0,1],fe=[0,[12,88,[4,0,0,0,0]],b(dY)],fd=b("WW"),fb=b("Zvar"),e$=b(aN),eS=[0,b(aN),[0,b("System"),0]],eT=b("Omega system time displaying flag"),eW=[0,b(aN),[0,b("Action"),0]],eX=b("Omega action display flag"),e0=[0,b(aN),[0,b("OldStyle"),0]],e1=b("Omega old style flag"),e4=[0,b("Stable"),[0,b(aN),0]],e5=b("Omega automatic reset of generated names"),e8=[0,b(aN),[0,b("UseLocalDefs"),0]],e9=b("Omega takes advantage of context variables with body"),fn=b("num.pos.xH"),fo=b("num.pos.xO"),fp=b("num.pos.xI"),fq=b("num.Z.Z0"),fr=b("num.Z.Zpos"),fs=b("num.Z.Zneg"),ft=b("num.Z.type"),fu=b("core.comparison.type"),fv=b("core.comparison.Gt"),fw=b("num.Z.add"),fx=b("num.Z.mul"),fy=b("num.Z.opp"),fz=b("num.Z.sub"),fA=b("num.Z.succ"),fB=b("num.Z.pred"),fC=b("num.Z.of_nat"),fD=b("num.Nat2Z.inj_add"),fE=b("num.Nat2Z.inj_mul"),fF=b("num.Nat2Z.inj_sub"),fG=b("plugins.omega.inj_minus2"),fH=b("num.Nat2Z.inj_succ"),fI=b("plugins.omega.inj_eq"),fJ=b("plugins.omega.inj_neq"),fK=b("plugins.omega.inj_le"),fL=b("plugins.omega.inj_lt"),fM=b("plugins.omega.inj_ge"),fN=b("plugins.omega.inj_gt"),fO=b("plugins.omega.fast_Zplus_assoc_reverse"),fP=b("plugins.omega.fast_Zplus_assoc"),fQ=b("plugins.omega.fast_Zmult_assoc_reverse"),fR=b("plugins.omega.fast_Zplus_permute"),fS=b("plugins.omega.fast_Zplus_comm"),fT=b("plugins.omega.fast_Zmult_comm"),fU=b("plugins.omega.Zmult_le_approx"),fV=b("plugins.omega.OMEGA1"),fW=b("plugins.omega.OMEGA2"),fX=b("plugins.omega.OMEGA3"),fY=b("plugins.omega.OMEGA4"),fZ=b("plugins.omega.OMEGA5"),f0=b("plugins.omega.OMEGA6"),f1=b("plugins.omega.OMEGA7"),f2=b("plugins.omega.OMEGA8"),f3=b("plugins.omega.OMEGA9"),f4=b("plugins.omega.fast_OMEGA10"),f5=b("plugins.omega.fast_OMEGA11"),f6=b("plugins.omega.fast_OMEGA12"),f7=b("plugins.omega.fast_OMEGA13"),f8=b("plugins.omega.fast_OMEGA14"),f9=b("plugins.omega.fast_OMEGA15"),f_=b("plugins.omega.fast_OMEGA16"),f$=b("plugins.omega.OMEGA17"),ga=b("plugins.omega.OMEGA18"),gb=b("plugins.omega.OMEGA19"),gc=b("plugins.omega.OMEGA20"),gd=b("plugins.omega.fast_Zred_factor0"),ge=b("plugins.omega.fast_Zred_factor1"),gf=b("plugins.omega.fast_Zred_factor2"),gg=b("plugins.omega.fast_Zred_factor3"),gh=b("plugins.omega.fast_Zred_factor4"),gi=b("plugins.omega.fast_Zred_factor5"),gj=b("plugins.omega.fast_Zred_factor6"),gk=b("plugins.omega.fast_Zmult_plus_distr_l"),gl=b("plugins.omega.fast_Zopp_plus_distr"),gm=b("plugins.omega.fast_Zopp_mult_distr_r"),gn=b("plugins.omega.fast_Zopp_eq_mult_neg_1"),go=b("plugins.omega.Zegal_left"),gp=b("plugins.omega.Zne_left"),gq=b("plugins.omega.Zlt_left"),gr=b("plugins.omega.Zge_left"),gs=b("plugins.omega.Zgt_left"),gt=b("plugins.omega.Zle_left"),gu=b("plugins.omega.new_var"),gv=b("plugins.omega.intro_Z"),gw=b("num.Z.eq_decidable"),gx=b("plugins.omega.dec_Zne"),gz=b("num.Z.le_decidable"),gB=b("num.Z.lt_decidable"),gD=b("plugins.omega.dec_Zgt"),gF=b("plugins.omega.dec_Zge"),gH=b("plugins.omega.not_Zeq"),gI=b("plugins.omega.not_Zne"),gK=b("plugins.omega.Znot_le_gt"),gM=b("plugins.omega.Znot_lt_ge"),gO=b("plugins.omega.Znot_ge_lt"),gQ=b("plugins.omega.Znot_gt_le"),gS=b("plugins.omega.neq"),gT=b("plugins.omega.Zne"),gU=b("num.Z.le"),gV=b("num.Z.lt"),gW=b("num.Z.ge"),gX=b("num.Z.gt"),gY=b("num.nat.type"),gZ=b("num.nat.O"),g0=b("num.nat.S"),g1=b("num.nat.le"),g2=b("num.nat.lt"),g3=b("num.nat.ge"),g4=b("num.nat.gt"),g5=b("num.nat.add"),g6=b("num.nat.sub"),g7=b("num.nat.mul"),g8=b("num.nat.pred"),g9=b("num.nat.pred_of_minus"),g_=b("num.nat.le_gt_dec"),g$=b("num.nat.eq_dec"),ha=b("num.nat.dec_le"),hc=b("num.nat.dec_lt"),he=b("num.nat.dec_ge"),hg=b("num.nat.dec_gt"),hi=b("num.nat.not_eq"),hj=b("num.nat.not_le"),hl=b("num.nat.not_lt"),hn=b("num.nat.not_ge"),hp=b("num.nat.not_gt"),hr=b("core.eq.ind_r"),hs=b("core.dec.or"),ht=b("core.dec.and"),hu=b("core.dec.imp"),hv=b("core.dec.iff"),hw=b("core.dec.not"),hx=b("core.dec.False"),hy=b("core.dec.not_not"),hz=b("core.dec.True"),hA=b("core.dec.not_or"),hB=b("core.dec.not_and"),hC=b("core.dec.not_imp"),hD=b("core.dec.not_iff"),hE=b("core.dec.dec_not_not"),hF=b("core.dec.imp_simp"),hG=b("core.iff.type"),hH=b("core.not.type"),hI=b("core.and.type"),hJ=b("core.or.type"),hK=b("core.eq.type"),hL=b("core.ex.type"),hM=b("core.False.type"),hN=b("core.True.type"),kB=b("Omega_plugin.Coq_omega.Undecidable"),k7=[0,b(dW),[0,b(dJ),[0,b(dZ),[0,b(dT),0]]]],kT=b(dZ),kU=b(dT),kV=b(dW),kW=b(dJ),kY=b("zify_positive"),kZ=b("zify_nat"),k0=b("zify_op"),k1=b("zify_N"),kX=b("No Omega knowledge base for type "),kP=[0,b("PreOmega"),[0,b(aW),[0,b(dP),0]]],k3=[0,b(aW),0],k5=b(aW),k8=[0,b(aW),[0,b(dQ),[0,b(dL),0]]],lb=b(dQ),lc=b(aW),le=b("omega'");function
dg(f){var
j=f[1],s=f[2];function
aq(b,a){var
d=c(f[2],b,a),e=d||dF(b,a);return e}function
A(b,a){return c(f[2],a,b)}function
M(b,a){var
d=c(f[2],a,b),e=d||dF(b,a);return e}var
h=f[3],k=f[4],i=f[5];function
x(b,a){return c(f[6],b,a)[1]}function
N(b,a){return c(f[6],b,a)[2]}var
d=f[8],e=f[9],y=c(h,e,e),ad=a(f[7],e);function
l(b){return c(f[2],b,d)?a(f[7],b):b}var
g=f[10],u=f[7];function
ar(b,a){return b<a?1:0}function
as(b,a){return a<b?1:0}function
at(b,a){return b<=a?1:0}function
au(b,a){return a<=b?1:0}function
av(b){a(m[33],b);a(m[36],0);return a(m[52],m[28])}function
E(b,a){a[1]=[0,b,a[1]];return 0}function
af(f,e){var
b=f,a=e;for(;;){if(c(j,a,d))return b;var
g=N(b,a),b=a,a=g;continue}}function
ag(b){return b?n(o[20],af,b[1],b[2]):a(m[3],d2)}function
F(b,a){var
g=M(b,d),f=A(a,d);return 0===g?0===f?x(b,a):c(k,x(c(h,b,e),a),e):0===f?c(k,x(c(k,b,e),a),e):x(b,a)}var
q=[aO,d3,aL(0)],ah=[aO,d4,aL(0)];function
G(h,f){var
b=f[2],k=f[1],p=0;function
q(i,b){var
k=c(s,b[1],d)?d5:i?d8:d9;a(m[31],k);var
f=l(b[1]);if(c(j,f,e)){var
o=a(h,b[2]);c(w[2],d6,o)}else{var
p=a(h,b[2]),q=a(g,f);n(w[2],d7,q,p)}return 1}n(o[20],q,p,k);if(A(b,d)){var
r=a(g,b);return c(w[2],d_,r)}var
i=c(s,b,d);if(i){var
t=a(g,l(b));return c(w[2],d$,t)}return i}function
W(a){function
b(b,a){if(15===a[0]){var
d=a[2][2],f=W(a[3][2]),g=W(d);return c(h,c(h,c(h,b,e),g),f)}return c(h,b,e)}return n(o[20],b,d,a)}function
O(a){switch(a){case
0:return ea;case
1:return eb;default:return ec}}function
P(a){switch(a){case
0:return ed;case
1:return ee;default:return ef}}function
z(d,b){function
e(a){var
b=a[4],e=a[3],f=a[2];c(w[2],eg,a[1]);G(d,[0,e,b]);var
g=O(f);return c(w[2],eh,g)}c(o[15],e,b);return a(m[31],ei)}function
aw(d,b){function
e(b){G(d,b);return a(m[31],ej)}c(o[15],e,b);return a(m[31],ek)}function
Q(d,q){var
e=q;for(;;){if(e){var
b=e[1],r=e[2];switch(b[0]){case
0:var
s=b[3],t=b[1],u=a(g,b[4]),v=a(g,s);cI(w[2],el,t[1],v,u);break;case
1:var
x=b[1],y=a(g,b[2]);n(w[2],em,x[1],y);break;case
2:c(w[2],en,b[1]);break;case
3:var
z=b[1],A=a(g,b[2]);n(w[2],eo,z[1],A);break;case
4:var
j=b[3],k=j[2],l=b[2],i=l[2],B=j[1],C=l[1],D=b[1],E=k[1],F=P(k[2]),H=a(g,B),I=i[1],J=P(i[2]),K=a(g,C),L=P(i[2]);lg(w[2],ep,L,D,K,J,I,H,F,E);break;case
5:var
f=b[1][1];c(w[2],eq,f[1]);G(d,[0,f[3],f[4]]);var
M=O(f[2]);a(m[31],M);a(m[31],er);break;case
6:var
h=b[1];c(w[2],es,h[1]);G(d,[0,h[3],h[4]]);var
N=O(h[2]);a(m[31],N);a(m[31],et);break;case
7:n(w[2],eu,b[1],b[2]);break;case
8:n(w[2],ev,b[1],b[2]);break;case
9:n(w[2],ew,b[1][1],b[2][1]);break;case
10:n(w[2],ex,b[1][1],b[2][1]);break;case
11:cI(w[2],ey,b[2][1],b[3],b[1]);break;case
12:var
R=b[1],S=a(g,b[2]);n(w[2],ez,R,S);break;case
13:c(w[2],eA,b[1]);break;case
14:var
T=b[1],U=a(g,b[2]);n(w[2],eB,T,U);break;case
15:var
o=b[3],p=b[2],V=o[2],W=p[2];cI(w[2],eC,b[1][1],p[1],o[1]);Q(d,W);a(m[36],0);Q(d,V);a(m[36],0);break;default:var
X=b[1],Y=a(g,b[2]);n(w[2],eD,X,Y)}var
e=r;continue}return a(m[52],m[28])}}function
ai(a){return c(w[4],eE,a)}var
X=[0,0];function
R(a){X[1]=0;return 0}function
S(a){return X[1]}function
b(a){if(aE[1])Q(ai,[0,a,0]);return E(a,X)}function
ax(b,a){return a[2]-b[2]|0}var
aj=a(o[48],ax);function
ay(b){var
c=b[2],d=c[2],e=b[1];return[0,e,[0,a(aj,c[1]),d]]}function
B(i){function
e(k){var
b=k;for(;;){if(b){var
f=b[2],g=b[1],h=a(i,g[1]);if(c(j,h,d)){var
b=f;continue}var
l=e(f);return[0,[0,h,g[2]],l]}return 0}}return e}function
C(c,b){var
d=a(c,b[4]),e=b[3],f=a(B(c),e);return[0,b[1],b[2],f,d]}function
az(b){return a(u,b)}function
H(a){return C(az,a)}function
J(m,l){var
b=m,a=l;for(;;){if(b){if(a){var
g=a[2],f=a[1],i=b[2],e=b[1];if(e[2]===f[2]){var
k=c(h,e[1],f[1]);if(c(j,k,d)){var
b=i,a=g;continue}var
n=J(i,g);return[0,[0,k,e[2]],n]}return f[2]<e[2]?[0,e,J(i,a)]:[0,f,J(b,g)]}return b}return a}}function
Y(e,b,d){var
f=c(h,b[4],d[4]),g=J(b[3],d[3]),i=b[2];return[0,a(e,0),i,g,f]}var
Z=[aO,eF,aL(0)];function
_(a){if(a){var
d=a[2],b=a[1];if(c(j,l(b[1]),e))return[0,b,d];var
f=_(d);return[0,f[1],[0,b,f[2]]]}throw Z}var
T=[aO,eG,aL(0)];function
K(c,a){if(a){var
d=a[2],b=a[1];if(b[2]===c)return[0,b,d];var
e=K(c,d);return[0,e[1],[0,b,e[2]]]}throw T}function
r(f){var
g=f[4],p=f[3],m=f[2],n=f[1];if(0===p)switch(m){case
0:if(c(j,g,d))return 0;b([12,n,g]);throw q;case
1:if(M(g,d))return 0;b([14,n,g]);throw q;default:if(cJ(g,d))return 0;b([13,n]);throw q}function
v(a){return l(a[1])}var
h=ag(c(o[17],v,p));if(0===m)if(cJ(N(g,h),d)){b([1,f,h]);throw q}if(2===m)if(cJ(N(g,h),d)){b([2,f[1]]);return 0}if(cJ(h,e)){var
s=F(g,h),w=c(k,g,c(i,s,h)),t=[0,n,m,a(B(function(a){return x(a,h)}),p),s];if(0===m)var
r=0;else
if(2===m)var
r=0;else
var
u=[0,f,t,h,w],r=1;if(!r)var
u=[3,f,h];b(u);return[0,t,0]}return[0,f,0]}function
D(o,g,f,d){var
h=g[1],p=d[3],q=g[2];try{var
k=K(q,p)[1],l=c(j,h,e)?a(u,k[1]):c(j,h,ad)?k[1]:a(m[3],eH),n=Y(o,d,C(function(a){return c(i,a,l)},f));b([4,n[1],[0,e,d],[0,l,f]]);return n}catch(a){a=t(a);if(a===T)return d;throw a}}function
$(b,a){var
d=c(i,y,a);return c(k,b,c(i,a,F(c(h,c(i,y,b),a),d)))}function
ak(q,f,H,G){var
g=q[1],j=f[3],I=q[3],t=a(q[2],0);if(0===j){z(I,[0,f,0]);a(m[3],eI)}var
J=a(o[6],j),L=a(o[5],j)[2],M=[0,l(a(o[5],j)[1]),L];function
N(b,a){var
c=b[1],d=b[2];if(A(c,l(a[1]))){var
e=a[2];return[0,l(a[1]),e]}return[0,c,d]}var
v=n(o[20],N,M,J),O=v[2],d=c(h,v[1],e),P=$(f[4],d),Q=f[3],R=a(B(function(a){return $(a,d)}),Q),S=[0,[0,a(u,d),t],R],w=[0,a(g,0),0,S,P],T=c(i,y,d),U=a(u,F(c(h,c(i,y,f[4]),d),T)),V=f[3],W=a(B(function(b){var
e=c(i,y,d);return a(u,F(c(h,c(i,y,b),d),e))}),V);b([5,[0,w,[0,a(g,0),0,W,U],f,d,t]]);var
X=r(w),k=a(o[5],X),s=K(O,k[3])[1];function
Y(a){return r(D(g,s,k,a))}var
Z=c(p[17][76],Y,H);function
_(a){return r(D(g,s,k,a))}var
aa=c(p[17][76],_,G),E=D(g,s,k,f),ab=C(function(a){return x(a,d)},E);b([3,E,d]);var
ac=r(ab);return[0,a(o[5],ac),Z,aa]}function
al(d,i){var
b=i;for(;;){var
f=b[3],e=b[2],a=b[1],g=d[1],j=d[3];if(aE[1])z(j,[0,a,e]);try{var
h=_(a[3])[1],k=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,g,h),l=c(p[17][76],k,f),m=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,g,h),n=[0,c(p[17][76],m,e),l];return n}catch(c){c=t(c);if(c===Z){var
b=ak(d,a,e,f);continue}throw c}}}function
aa(k,p){var
f=p;for(;;){var
g=f[2],h=f[1],r=k[3],m=function(a){if(a){var
d=a[2],b=a[1],g=b[3],h=function(a){return c(j,l(a[1]),e)};if(c(o[28],h,g))return[0,b,d];var
f=m(d);return[0,f[1],[0,b,f[2]]]}throw I};if(h){var
s=h[2],u=h[1];try{var
n=m(h),v=n[2],w=n[1],a=w,i=v}catch(b){b=t(b);if(b!==I)throw b;var
a=u,i=s,x=b}if(0===a[3]){if(c(j,a[4],d)){b([2,a[1]]);var
f=[0,i,g];continue}b([12,a[1],a[4]]);throw q}var
f=al(k,[0,a,i,g]);continue}if(aE[1])z(r,g);return g}}function
L(p,e){function
y(a){var
b=a[3];if(b)if(c(s,b[1][1],d))return[0,H(a),0];return[0,a,1]}var
f=c(v[1],0,7);function
i(z){var
l=y(z),m=l[2],a=l[1],g=a[3];if(0===g){if(c(s,a[4],d)){b([14,a[1],a[4]]);throw q}return b([2,a[1]])}try{var
o=c(v[6],f,g),i=o[2],j=o[1];if(1===m)if(j)var
k=j[1],C=c(s,k[4],a[4])?(b([7,k[1],a[1]]),k):(b([7,a[1],k[1]]),a),e=[0,[0,C],i];else
var
e=[0,[0,a],i];else
if(i){var
h=i[1];if(A(h[4],a[4]))b([8,h[1],a[1]]);else
b([8,a[1],h[1]]);var
E=A(h[4],a[4])?h:a,e=[0,j,[0,E]]}else
var
e=[0,j,[0,a]];var
p=e[1];if(p){var
r=e[2];if(r){var
u=r[1],w=p[1];if(c(s,w[4],u[4])){b([9,w,H(u)]);throw q}var
x=1}else
var
x=0}else
var
x=0;c(v[10],f,g);var
D=n(v[5],f,g,e);return D}catch(b){b=t(b);if(b===I){var
B=1===m?[0,[0,a],0]:[0,0,[0,a]];return n(v[5],f,g,B)}throw b}}c(o[15],i,e);var
h=[0,0],g=[0,0];function
k(n,f){var
d=f[1];if(d){var
i=f[2];if(i){var
k=i[1],e=d[1];if(c(j,e[4],k[4])){var
l=a(p,0);b([11,l,e,k[1]]);return E([0,l,0,e[3],e[4]],h)}}}var
m=f[2];if(d)E(d[1],g);return m?E(H(m[1]),g):0}c(v[12],k,f);return[0,h[1],g[1]]}var
U=[aO,eJ,aL(0)];function
am(g){var
b=c(v[1],0,7);function
h(e,d){try{var
a=c(v[6],b,e),g=l(d);a[1]=c(m[6],a[1],g);var
h=0;return h}catch(a){a=t(a);if(a===I){var
f=[0,l(d)];return n(v[5],b,e,f)}throw a}}function
i(a){var
b=a[3];function
d(a){return h(a[2],a[1])}return c(o[15],d,b)}c(o[15],i,g);var
e=[0,d],a=[0,-1],f=[0,0];function
j(h,g){var
b=g[1];f[1]++;var
i=c(s,b,e[1]),d=i||(-1===a[1]?1:0),j=d?(a[1]=h,e[1]=b,0):d;return j}c(v[12],j,b);if(f[1]<1)throw U;return a[1]}function
an(i,b){function
c(c,b){var
e=c[3],f=c[2],g=c[1];try{var
h=K(i,b[3])[1],j=M(h[1],d)?[0,g,[0,[0,h[1],b],f],e]:[0,g,f,[0,[0,a(u,h[1]),b],e]];return j}catch(a){a=t(a);if(a===T)return[0,[0,b,g],f,e];throw a}}return n(o[20],c,eK,b)}function
ao(u,t,d,g){var
f=0;function
h(h,d){var
j=d[2],f=d[1];function
l(n,l){var
o=l[2],g=l[1],v=C(function(a){return c(i,a,f)},o),p=Y(u,C(function(a){return c(i,a,g)},j),v);b([4,p[1],[0,g,j],[0,f,o]]);var
h=r(p);if(h){var
d=h[1];if(h[2])return a(m[3],eL);if(t){var
w=c(k,g,e),q=c(i,c(k,f,e),w);b([16,d[1],q]);var
x=c(k,d[4],q),s=[0,d[1],1,d[3],x]}else
var
s=d;return[0,s,n]}return n}return n(o[20],l,h,g)}return n(o[20],h,f,d)}function
ab(d,f,b){var
g=d[3],h=d[1],a=an(am(b),b),i=a[1],j=ao(h,f,a[2],a[3]),e=c(m[26],i,j);if(aE[1])z(g,e);return e}function
aA(d,l,e){var
h=d[1],n=d[3];function
q(a){return 2===a[2]?1:0}if(c(o[28],q,e))a(m[3],eM);R(0);function
s(a){return b([6,a])}c(o[15],s,e);var
u=c(p[17][76],r,e);function
v(a){return 0===a[2]?1:0}var
i=c(o[37],v,u),w=i[1],j=L(h,i[2]),x=j[2],y=[0,c(m[26],w,j[1]),x];function
g(b,c){var
a=aa(d,c);return b<50?f(b+1|0,a):cK(f,[0,a])}function
f(e,f){var
a=L(h,f),b=a[2],c=a[1];if(0===c)return b;var
d=[0,c,b];return e<50?g(e+1|0,d):cK(g,[0,d])}function
A(a){return cL(g(0,a))}function
B(a){return cL(f(0,a))}function
k(b){try{var
a=k(B(ab(d,l,b)));return a}catch(a){a=t(a);if(a===U){if(aE[1])z(n,b);return b}throw a}}return k(A(y))}function
V(k,j,i){var
f=k,d=j,e=i;for(;;){if(e){var
g=e[2],b=e[1];switch(b[0]){case
0:if(c(ae[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
1:var
f=[0,b[1][1],f],d=[0,b,d],e=g;continue;case
2:var
e=g;continue;case
3:if(c(ae[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
4:var
l=b[3][2],n=b[2][2];if(c(ae[4][1],b[1],f)){var
f=[0,n[1],[0,l[1],f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
5:var
h=b[1],o=h[3];if(c(ae[4][1],h[1][1],f)){var
f=[0,o[1],f],d=[0,b,d],e=g;continue}var
e=g;continue;case
6:if(c(ae[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
7:var
e=g;continue;case
8:var
e=g;continue;case
9:var
f=[0,b[1][1],[0,b[2][1],f]],d=[0,b,d],e=g;continue;case
10:var
f=[0,b[1][1],[0,b[2][1],f]],d=[0,b,d],e=g;continue;case
11:var
p=b[3],q=b[2];if(c(ae[4][1],b[1],f)){var
f=[0,q[1],[0,p,f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
12:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
13:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
14:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
15:return a(m[3],eN);default:if(c(ae[4][1],b[1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue}}return[0,f,d]}}function
ap(a){var
g=a[2],h=a[1];function
i(a){return 2===a[2]?1:0}var
j=c(o[37],i,g)[1];function
e(a){var
b=a[3];if(b)if(c(s,b[1][1],d))return[0,H(a),0];return[0,a,1]}var
f=c(v[1],0,7);function
k(a){var
b=e(a),c=b[1];return n(v[5],f,[0,c[3],c[4]],[0,b[2],a])}c(o[15],k,j);function
l(a){if(0===a[2]){var
d=e(a),g=d[1],i=d[2],j=g[4],k=g[3];try{var
h=c(v[6],f,[0,k,j]);b([10,a,h[2],i===h[1]?1:0]);throw q}catch(a){a=t(a);if(a===I)return 0;throw a}}throw[0,ba,eO]}return c(o[15],l,h)}var
ac=[aO,eP,aL(0)];return[0,j,s,aq,A,M,h,k,i,x,N,d,e,y,ad,l,g,u,ar,as,at,au,av,E,af,ag,F,q,ah,G,W,O,P,z,aw,g,Q,ai,b,S,R,aj,ay,B,C,H,J,Y,Z,_,T,K,r,D,$,ak,al,aa,L,U,am,an,ao,ab,aA,V,ap,ac,function(d,j){var
f=d[1],D=d[3];R(0);function
E(a){return b([6,a])}c(o[15],E,j);function
i(c,a){ap(a);var
b=aa(d,a);return c<50?h(c+1|0,b):cK(h,[0,b])}function
h(j,k){function
l(a){return 2===a[2]?1:0}var
a=c(o[37],l,k),b=a[1],d=L(f,a[2]),e=d[2],g=d[1];if(0===g)return c(m[26],b,e);var
h=[0,g,c(m[26],b,e)];return j<50?i(j+1|0,h):cK(i,[0,h])}function
F(a){return cL(i(0,a))}function
G(a){return cL(h(0,a))}function
l(b){try{var
a=l(G(ab(d,0,b)));return a}catch(a){a=t(a);if(a===U){if(aE[1])z(D,b);return b}throw a}}function
H(l){var
d=l;for(;;){var
g=d[1];if(g){var
j=d[2],b=g[1],n=d[3],p=g[2],h=a(f,0),i=a(f,0),q=c(k,b[4],e),r=[0,h,1,b[3],q],s=c(k,a(u,b[4]),e),t=b[3],v=[0,i,1,a(B(u),t),s],w=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,0],a[1]],[0,d,a[2]]]}}(b,i,v),x=c(o[17],w,j),y=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,1],a[1]],[0,d,a[2]]]}}(b,h,r),z=c(o[17],y,j),A=c(m[26],z,x),d=[0,p,A,[0,[0,b[1],[0,b,h,i]],n]];continue}return[0,d[2],d[3]]}}try{var
J=c(p[17][76],r,j),K=function(a){return 0===a[2]?1:0},s=c(o[37],K,J),M=s[2],N=s[1],O=function(a){return 2===a[2]?1:0},w=c(o[37],O,M),P=w[1],x=L(f,w[2]),Q=x[1],T=c(m[26],x[2],P),W=F([0,c(m[26],N,Q),T]),X=function(a){return 2===a[2]?1:0},y=c(o[37],X,W),Y=y[2],Z=y[1],_=S(0),A=H([0,Z,[0,[0,0,Y],0],0]),$=A[2],ad=A[1],af=function(a){var
b=a[1],f=a[2];R(0);try{l(f);throw ah}catch(a){a=t(a);if(a===q){var
d=V(0,0,S(0)),e=d[1],g=d[2],h=function(a){return c(ae[4][1],a[2],e)},i=c(o[37],h,b)[1],j=function(a){return a[1]};return[0,c(o[17],j,i),e,b,g]}throw a}},ag=c(o[17],af,ad),ai=function(e){var
b=c(v[1],0,7),a=[0,-1],d=[0,0];function
f(d){try{c(v[6],b,d)[1]++;var
a=0;return a}catch(a){a=t(a);if(a===I)return n(v[5],b,d,[0,1]);throw a}}function
g(a){var
b=a[1];if(b)return c(o[15],f,b);throw[0,ac,a[4],a[2]]}c(o[15],g,e);function
h(e,b){var
c=d[1]<b[1]?1:0,f=c?(a[1]=e,d[1]=b[1],0):c;return f}c(v[12],h,b);return a[1]},g=function(e){try{var
d=ai(e),l=function(g){var
b=g[3];for(;;){if(b){var
c=b[1],e=b[2],f=c[3];if(d===c[1])return f;var
b=e;continue}return a(m[3],eQ)}},f=c(o[37],l,e),q=f[2],r=f[1],h=function(a){var
b=a[4],c=a[3],e=a[2],f=a[1];function
g(b,a){return b===a?1:0}return[0,n(p[17][95],g,d,f),e,c,b]},s=c(o[17],h,r),u=c(o[17],h,q),i=g(s),v=i[2],w=i[1],j=g(u),x=j[2],y=j[1],b=c(ae[4][2],d,$),k=b[1],z=b[3],A=b[2],B=function(b,a){return b===a?1:0},C=n(p[17][129],B,v,x),D=[0,[0,[15,k,[0,A,w],[0,z,y]],0],[0,k[1],C]];return D}catch(a){a=t(a);if(a[1]===ac)return[0,a[2],a[3]];throw a}},C=g(ag),aj=V(C[2],C[1],_)[2];return aj}catch(a){a=t(a);if(a===q)return V(0,0,S(0))[2];throw a}}]}cH(398,[0,aE,dg],"Omega_plugin__Omega");var
l=dg([0,C[17],C[16],C[12],C[13],C[14],C[15],C[22],C[5],C[6],C[2]]);function
aP(b){var
c=a(h[11],b);return a(j[aM],c)}function
bb(b){var
c=a(h[11],b);return a(j[87],c)}var
cQ=[0,0],bc=[0,0],bd=[0,0],cR=[0,1],cS=[0,1];function
aX(b,a){b[1]=a;return 0}function
eR(a){return aX(cQ,a)}var
eU=[0,0,eT,eS,function(a){return cQ[1]},eR];c(aY[4],0,eU);function
eV(a){return aX(bc,a)}var
eY=[0,0,eX,eW,function(a){return bc[1]},eV];c(aY[4],0,eY);function
eZ(a){return aX(bd,a)}var
e2=[0,0,e1,e0,function(a){return bd[1]},eZ];c(aY[4],0,e2);function
e3(a){return aX(cS,a)}var
e6=[0,1,e5,e4,function(a){return cS[1]},e3];c(aY[4],0,e6);function
e7(a){return aX(cR,a)}var
e_=[0,0,e9,e8,function(a){return cR[1]},e7];c(aY[4],0,e_);var
cT=[0,0];function
aZ(a){var
b=[0,a];cT[1]=[0,[0,b,a],cT[1]];return b}var
dh=aZ(0);function
af(e){var
b=a(m[22],dh[1]),d=c(m[17],e$,b);dh[1]++;return a(D[1][6],d)}var
di=aZ(0);function
fa(e){var
b=a(m[22],di[1]),d=c(m[17],fb,b);di[1]++;return a(D[1][6],d)}var
dj=aZ(0);function
be(a){dj[1]++;return dj[1]}var
dk=aZ(1000);function
dl(a){dk[1]++;return dk[1]}var
dm=aZ(0);function
fc(a){dm[1]++;return c(a0[1],fd,[0,dm[1]])}function
a1(a){return c(w[4],fe,a)}var
cU=[0,0],bf=c(v[1],0,7),cV=c(v[1],0,7);function
dn(b){try{var
a=c(v[6],cV,b);return a}catch(a){a=t(a);if(a===I){var
d=fc(0);n(v[5],bf,d,b);n(v[5],cV,b,d);return d}throw a}}function
cW(b){try{var
a=c(v[6],bf,b);return a}catch(a){a=t(a);if(a===I){var
d=cU[1];n(v[5],bf,b,d);n(v[5],cV,d,b);cU[1]++;return d}throw a}}function
P(b){return a(k[65][22],b)}function
ff(a){return cI(j[109],0,fg,1,[0,[0,a,0]])}function
u(b){return a(j[148],b)}function
O(b){var
c=f(b),h=0,i=g===c?b[1]:d===c?a(e[2],b):b;return a(j[69],[0,[0,0,i],h])}function
cX(b,a){return n(F[35][1],cY[9],b,a)}var
bg=[0,0];function
fh(c){return function(d){var
a=d;for(;;){if(a){var
b=a[1],e=a[2],f=b[1];if(c===b[2])return f;var
a=e;continue}throw I}}}function
J(b){try{var
c=bg[1],d=a(fh(b),c);return d}catch(b){b=t(b);if(b===I)return a(m[3],fi);throw b}}function
aF(b,a){bg[1]=[0,[0,b,a],bg[1]];return 0}var
a2=[0,0];function
fj(a){return a2[1]}function
dp(a){a2[1]=0;return 0}function
fl(d,c,b,a){a2[1]=[0,[0,d,[0,c,b,a]],a2[1]];return 0}function
i(b){return[d,function(e){var
c=a(dq[2],b),d=a(fm[22],c);return a(h[9],d)}]}var
al=i(fn),am=i(fo),an=i(fp),Q=i(fq),R=i(fr),S=i(fs),x=i(ft),bh=i(fu),ao=i(fv),y=i(fw),K=i(fx),L=i(fy),ag=i(fz),ah=i(fA),a3=i(fB),ap=i(fC),bi=i(fD),bj=i(fE),bk=i(fF),bl=i(fG),bm=i(fH),bn=i(fI),bo=i(fJ),bp=i(fK),bq=i(fL),br=i(fM),bs=i(fN),A=i(fO),bt=i(fP),bu=i(fQ),aq=i(fR),ar=i(fS),bv=i(fT),bw=i(fU),bx=i(fV),by=i(fW),bz=i(fX),bA=i(fY),bB=i(fZ),bC=i(f0),bD=i(f1),bE=i(f2),bF=i(f3),bG=i(f4),T=i(f5),M=i(f6),bH=i(f7),bI=i(f8),bJ=i(f9),bK=i(f_),bL=i(f$),bM=i(ga),bN=i(gb),bO=i(gc),bP=i(gd),bQ=i(ge),bR=i(gf),bS=i(gg),bT=i(gh),U=i(gi),bU=i(gj),bV=i(gk),bW=i(gl),bX=i(gm),V=i(gn),bY=i(go),bZ=i(gp),b0=i(gq),b1=i(gr),b2=i(gs),b3=i(gt),b4=i(gu),b5=i(gv),b6=i(gw),gy=i(gx),gA=i(gz),gC=i(gB),gE=i(gD),gG=i(gF),b7=i(gH),gJ=i(gI),gL=i(gK),gN=i(gM),gP=i(gO),gR=i(gQ),as=i(gS),at=i(gT),a4=i(gU),b8=i(gV),b9=i(gW),ai=i(gX),W=i(gY),au=i(gZ),av=i(g0),aw=i(g1),b_=i(g2),b$=i(g3),ax=i(g4),ca=i(g5),ay=i(g6),cb=i(g7),cc=i(g8),cd=i(g9),ce=i(g_),cf=i(g$),hb=i(ha),hd=i(hc),hf=i(he),hh=i(hg),cg=i(hi),hk=i(hj),hm=i(hl),ho=i(hn),hq=i(hp),ch=i(hr),ci=i(hs),cj=i(ht),ck=i(hu),cl=i(hv),cm=i(hw),cn=i(hx),co=i(hy),cp=i(hz),cq=i(hA),cr=i(hB),cs=i(hC),ct=i(hD),cu=i(hE),cv=i(hF),cw=i(hG),aj=i(hH),az=i(hI),aA=i(hJ),X=i(hK),cx=i(hL),aB=i(hM),cy=i(hN);function
aQ(o,b){var
i=a(hO[2],0),p=a(hP[17],i),j=f(b),q=g===j?b[1]:d===j?a(e[2],b):b,k=c(h[3],p,q);if(10===k[0]){var
l=k[1][1];if(c(cY[2],i,[1,l]))return[1,l]}var
r=c(m[17],o,hQ),s=a(ac[3],r);return n(_[3],0,hR,s)}var
hT=[d,function(a){return aQ(hS,ah)}],hV=[d,function(a){return aQ(hU,a3)}],hX=[d,function(a){return aQ(hW,ag)}],dr=[d,function(a){return aQ(hY,a4)}],aG=[d,function(a){return aQ(hZ,ai)}],cZ=[d,function(a){return aQ(h0,aj)}];function
ad(i,c){var
b=f(y),j=[0,i,c],k=g===b?y[1]:d===b?a(e[2],y):y;return a(h[23],[0,k,j])}function
c0(i,c){var
b=f(K),j=[0,i,c],k=g===b?K[1]:d===b?a(e[2],K):K;return a(h[23],[0,k,j])}function
h1(i,c){var
b=f(ag),j=[0,i,c],k=g===b?ag[1]:d===b?a(e[2],ag):ag;return a(h[23],[0,k,j])}function
ds(j,i,c){var
b=f(X),k=[0,j,i,c],l=g===b?X[1]:d===b?a(e[2],X):X;return a(h[23],[0,l,k])}function
cz(h,c){var
b=f(x),i=g===b?x[1]:d===b?a(e[2],x):x;return ds(i,h,c)}function
aH(i,c){var
b=f(ai),j=[0,i,c],k=g===b?ai[1]:d===b?a(e[2],ai):ai;return a(h[23],[0,k,j])}function
aI(c){var
b=f(L),i=[0,c],j=g===b?L[1]:d===b?a(e[2],L):L;return a(h[23],[0,j,i])}function
cA(i,c){var
b=f(az),j=[0,i,c],k=g===b?az[1]:d===b?a(e[2],az):az;return a(h[23],[0,k,j])}function
c1(i,c){var
b=f(aA),j=[0,i,c],k=g===b?aA[1]:d===b?a(e[2],aA):aA;return a(h[23],[0,k,j])}function
aC(c){var
b=f(aj),i=[0,c],j=g===b?aj[1]:d===b?a(e[2],aj):aj;return a(h[23],[0,j,i])}function
aJ(c){var
b=f(ap),i=[0,c],j=g===b?ap[1]:d===b?a(e[2],ap):ap;return a(h[23],[0,j,i])}function
G(b){function
i(b){if(c(l[1],b,l[12])){var
j=f(al);return g===j?al[1]:d===j?a(e[2],al):al}var
o=[0,i(c(l[9],b,l[13]))],p=l[11],q=c(l[10],b,l[13]);if(c(l[1],q,p))var
k=f(am),r=g===k?am[1]:d===k?a(e[2],am):am,m=r;else
var
n=f(an),s=g===n?an[1]:d===n?a(e[2],an):an,m=s;return a(h[23],[0,m,o])}if(c(l[1],b,l[11])){var
j=f(Q);return g===j?Q[1]:d===j?a(e[2],Q):Q}var
o=[0,i(a(l[15],b))];if(c(l[4],b,l[11]))var
k=f(R),p=g===k?R[1]:d===k?a(e[2],R):R,m=p;else
var
n=f(S),q=g===n?S[1]:d===n?a(e[2],S):S,m=q;return a(h[23],[0,m,o])}function
aR(l,H){function
j(b,a){return n(h[dc],l,b,a)}var
m=c(h[91],l,H),b=m[2],i=m[1],k=c(h[3],l,i);if(b){var
o=b[2];if(o){var
p=o[2];if(p){if(!p[2]){var
q=f(X),M=g===q?X[1]:d===q?a(e[2],X):X;if(j(M,i))return[1,16,b]}}else{var
r=f(as),N=g===r?as[1]:d===r?a(e[2],as):as;if(j(i,N))return[1,17,b];var
s=f(at),O=g===s?at[1]:d===s?a(e[2],at):at;if(j(i,O))return[1,18,b];var
t=f(a4),P=g===t?a4[1]:d===t?a(e[2],a4):a4;if(j(i,P))return[1,19,b];var
u=f(b8),Q=g===u?b8[1]:d===u?a(e[2],b8):b8;if(j(i,Q))return[1,20,b];var
v=f(b9),R=g===v?b9[1]:d===v?a(e[2],b9):b9;if(j(i,R))return[1,21,b];var
w=f(ai),S=g===w?ai[1]:d===w?a(e[2],ai):ai;if(j(i,S))return[1,22,b];var
x=f(az),T=g===x?az[1]:d===x?a(e[2],az):az;if(j(i,T))return[1,25,b];var
y=f(aA),U=g===y?aA[1]:d===y?a(e[2],aA):aA;if(j(i,U))return[1,26,b];var
z=f(cw),V=g===z?cw[1]:d===z?a(e[2],cw):cw;if(j(i,V))return[1,30,b];var
A=f(aw),W=g===A?aw[1]:d===A?a(e[2],aw):aw;if(j(i,W))return[1,31,b];var
B=f(b_),Y=g===B?b_[1]:d===B?a(e[2],b_):b_;if(j(i,Y))return[1,32,b];var
C=f(b$),Z=g===C?b$[1]:d===C?a(e[2],b$):b$;if(j(i,Z))return[1,33,b];var
D=f(ax),$=g===D?ax[1]:d===D?a(e[2],ax):ax;if(j(i,$))return[1,34,b]}}else{var
E=f(aj),aa=g===E?aj[1]:d===E?a(e[2],aj):aj;if(j(i,aa))return[1,29,b]}}else{var
F=f(aB),ab=g===F?aB[1]:d===F?a(e[2],aB):aB;if(j(i,ab))return[1,27,b];var
G=f(cy),ad=g===G?cy[1]:d===G?a(e[2],cy):cy;if(j(i,ad))return[1,28,b]}switch(k[0]){case
1:if(!b)return[0,k[1]];break;case
6:if(k[1][1]){if(!b){var
I=a(ac[3],h2);return n(_[6],0,0,I)}}else
if(!b)return[2,k[2],k[3]];break;case
10:var
J=a(c2[41],[1,k[1][1]]);return[1,[0,a(c3[20],J)],b];case
11:var
K=a(c2[41],[2,k[1][1]]);return[1,[0,a(c3[20],K)],b];case
12:var
L=a(c2[41],[3,k[1][1]]);return[1,[0,a(c3[20],L)],b]}return 0}var
h3=cY[9];function
a5(k,b,q){var
l=n(c4[81],0,k,b),r=n(h3,k,b,q),m=c(h[91],b,r),i=m[2],j=m[1];c(h[3],b,j);if(!i){var
o=f(x),s=g===o?x[1]:d===o?a(e[2],x):x;if(c(l,j,s))return[1,23,i];var
p=f(W),t=g===p?W[1]:d===p?a(e[2],W):W;if(c(l,j,t))return[1,24,i]}return 0}function
cB(k,G){function
j(b,a){return n(h[dc],k,b,a)}var
l=c(h[91],k,G),b=l[2],i=l[1],m=c(h[3],k,i);if(b){var
o=b[2];if(o){if(!o[2]){var
p=f(y),H=g===p?y[1]:d===p?a(e[2],y):y;if(j(i,H))return[1,0,b];var
q=f(K),I=g===q?K[1]:d===q?a(e[2],K):K;if(j(i,I))return[1,1,b];var
r=f(ag),J=g===r?ag[1]:d===r?a(e[2],ag):ag;if(j(i,J))return[1,2,b];var
s=f(ca),M=g===s?ca[1]:d===s?a(e[2],ca):ca;if(j(i,M))return[1,6,b];var
t=f(cb),N=g===t?cb[1]:d===t?a(e[2],cb):cb;if(j(i,N))return[1,7,b];var
u=f(ay),O=g===u?ay[1]:d===u?a(e[2],ay):ay;if(j(i,O))return[1,8,b]}}else{var
v=f(ah),P=g===v?ah[1]:d===v?a(e[2],ah):ah;if(j(i,P))return[1,3,b];var
w=f(a3),T=g===w?a3[1]:d===w?a(e[2],a3):a3;if(j(i,T))return[1,5,b];var
x=f(L),U=g===x?L[1]:d===x?a(e[2],L):L;if(j(i,U))return[1,4,b];var
z=f(cc),V=g===z?cc[1]:d===z?a(e[2],cc):cc;if(j(i,V))return[1,9,b];var
A=f(av),W=g===A?av[1]:d===A?a(e[2],av):av;if(j(i,W))return[1,10,b];var
B=f(R),X=g===B?R[1]:d===B?a(e[2],R):R;if(j(i,X))return[1,13,b];var
C=f(S),Y=g===C?S[1]:d===C?a(e[2],S):S;if(j(i,Y))return[1,12,b];var
D=f(ap),Z=g===D?ap[1]:d===D?a(e[2],ap):ap;if(j(i,Z))return[1,15,b]}}else{var
E=f(au),_=g===E?au[1]:d===E?a(e[2],au):au;if(j(i,_))return[1,11,b];var
F=f(Q),$=g===F?Q[1]:d===F?a(e[2],Q):Q;if(j(i,$))return[1,14,b];if(1===m[0])return[0,m[1]]}return 0}function
h4(j,u){function
b(b,a){return n(h[dc],j,b,a)}function
i(t){var
o=c(h[91],j,t),k=o[2],n=o[1];if(k){if(!k[2]){var
p=k[1],q=f(an),u=g===q?an[1]:d===q?a(e[2],an):an;if(b(n,u)){var
v=i(p),w=c(l[8],l[13],v);return c(l[6],l[12],w)}var
r=f(am),x=g===r?am[1]:d===r?a(e[2],am):am;if(b(n,x)){var
y=i(p);return c(l[8],l[13],y)}}}else{var
s=f(al),z=g===s?al[1]:d===s?a(e[2],al):al;if(b(n,z))return l[12]}return a(m[3],h5)}var
p=c(h[91],j,u),k=p[2],o=p[1];if(k){if(!k[2]){var
q=k[1],r=f(R),v=g===r?R[1]:d===r?a(e[2],R):R;if(b(o,v))return i(q);var
s=f(S),w=g===s?S[1]:d===s?a(e[2],S):S;if(b(o,w)){var
x=i(q);return a(l[17],x)}}}else{var
t=f(Q),y=g===t?Q[1]:d===t?a(e[2],Q):Q;if(b(o,y))return l[11]}return a(m[3],h6)}function
c5(y,x,e,b){function
d(f,e,o){var
b=c(h[3],y,o);if(5===b[0]){var
S=b[3],T=b[2],U=[0,d(f,e,b[1]),T,S];return a(h[19],U)}if(e){var
q=e[1];if(q){var
r=q[1],z=e[2];switch(b[0]){case
9:var
D=b[1],j=a(p[19][8],b[2]),s=r-1|0,t=r-1|0,E=d(f,z,aU(j,s)[1+s]);aU(j,t)[1+t]=E;return a(h[23],[0,D,j]);case
14:var
i=0;break;default:var
i=1}}else{var
n=e[2];switch(b[0]){case
6:var
I=b[3],J=b[1],K=[0,J,d(f,n,b[2]),I];return a(h[20],K);case
7:var
L=b[3],M=b[1],N=[0,M,d(f,n,b[2]),L];return a(h[21],N);case
8:var
O=b[4],P=b[2],Q=b[1],R=[0,Q,P,d(f,n,b[3]),O];return a(h[22],R);case
14:var
i=0;break;default:var
i=1}}if(i){var
A=a(p[17][1],e),B=a(m[22],A),C=c(m[17],h7,B);return a(m[3],C)}var
u=b[1],k=u[2],l=k[3],v=u[1],g=v[2],F=k[2],G=k[1],w=a(p[19][8],l),H=d(f+(l.length-1)|0,e,aU(l,g)[1+g]);aU(w,g)[1+g]=H;return a(h[33],[0,v,[0,G,F,w]])}return c(x,f,o)}return d(1,e,b)}function
dt(g,f,e,d){var
b=[0,a(h[10],0)],i=c5(g,function(d,c){b[1]=c;return a(h[10],d)},e,d),j=b[1],k=[0,a(D[1][6],h9)],l=[0,c(z[4],k,0),f,i];return[0,a(h[21],l),j]}function
$(g){function
b(b){var
c=a(F[35][6],b),d=a(p[17][9],g);function
e(c,a){return cX(b,a)}var
f=c5(a(F[35][4],b),e,d,c);return n(j[3],h_,f,2)}return a(q[68][8],b)}function
aK(b){switch(b[0]){case
0:var
c=b[2],d=b[1];a(m[31],h$);aK(d);a(m[31],ia);aK(c);return a(m[31],ib);case
1:var
e=b[2],f=b[1];a(m[31],ic);aK(f);a(m[31],id);aK(e);return a(m[31],ie);case
2:var
g=a(D[1][8],b[1]);return a(m[31],g);case
3:var
h=a(l[16],b[1]);return a(m[31],h);default:return a(m[31],ig)}}function
Y(c){var
b=c;for(;;)switch(b[0]){case
0:return a(m[3],ih);case
1:var
b=b[1];continue;case
2:return cW(b[1]);case
3:return-1;default:return-1}}function
B(b){switch(b[0]){case
0:var
j=b[1],k=B(b[2]),l=[0,B(j),k],c=f(y),m=g===c?y[1]:d===c?a(e[2],y):y;return a(h[23],[0,m,l]);case
1:var
n=b[1],o=B(b[2]),p=[0,B(n),o],i=f(K),q=g===i?K[1]:d===i?a(e[2],K):K;return a(h[23],[0,q,p]);case
2:return a(h[11],b[1]);case
3:return G(b[1]);default:return b[1]}}function
N(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1],g=c(a[2]);return[0,[1,[2,dn(e)],[3,f]],g]}return[3,b[4]]}return c(b[3])}function
du(b,a,c){var
d=n(c4[19],b,a,c);return lh(ij[4],0,0,0,0,0,0,0,0,b,a,d)}function
dv(i,o,b,m){function
j(j){var
q=a(F[35][6],j),r=a(F[35][5],j),s=a(p[17][9],o),k=dt(a(F[35][4],j),i,s,q),l=k[1],t=k[2];function
u(o){var
q=a(h[10],1),s=[0,i,b,a(h[10],2),q,t,m],j=f(ch),p=[0,l,0],u=g===j?ch[1]:d===j?a(e[2],ch):ch,v=a(h[23],[0,u,s]),w=[0,a(h[10],1),[0,b,0]],x=a(h[40],w),y=[0,a(D[1][6],ik)],A=[0,c(z[4],y,0),x,v],B=a(h[21],A),C=n(h[35],i,0,h[16]),E=[0,a(D[1][6],il)],F=[0,c(z[4],E,0),C,B],G=[0,a(h[21],F),p],H=a(h[40],G),k=du(r,o,a(h[23],[0,l,[0,b]])),I=k[1];return[0,I,a(h[40],[0,H,[0,k[2],0]])]}return c(dw[1],0,u)}return a(q[68][8],j)}function
c6(i,h,c){var
b=f(x),j=g===b?x[1]:d===b?a(e[2],x):x;return dv(j,i,h,c)}function
a6(d,c,b){return c6(d,c,a(h[40],[0,b[1],b[2]]))}function
im(k,j,c){var
l=a(h[40],[0,c[1],c[2]]),b=f(W),i=g===b?W[1]:d===b?a(e[2],W):W;return dv(i,k,j,l)}function
dx(d,b){function
e(e){var
i=a(F[35][5],d),j=c(F[35][8],d,b),f=c(h[3],e,j);if(6===f[0]){var
g=du(i,e,f[2]),k=g[1];return[0,k,a(h[40],[0,b,[0,g[2],0]])]}throw[0,ba,io]}return c(dw[1],0,e)}function
s(n,l,k){function
b(i){var
o=a(F[35][6],i),q=a(p[17][9],n),b=f(x),r=g===b?x[1]:d===b?a(e[2],x):x,j=dt(a(F[35][4],i),r,q,o),u=j[2],s=j[1];function
t(v){var
b=v,e=u,w=a(F[35][4],i);for(;;){var
d=c(h[3],w,e);if(5===d[0]){var
e=d[1];continue}if(b){var
j=b[1];if(j){var
o=b[2],q=j[1];switch(d[0]){case
9:var
k=q-1|0,b=o,e=aU(d[2],k)[1+k];continue;case
14:var
f=0;break;default:var
f=1}}else{var
g=b[2];switch(d[0]){case
6:var
b=g,e=d[2];continue;case
7:var
b=g,e=d[2];continue;case
8:var
b=g,e=d[3];continue;case
14:var
f=0;break;default:var
f=1}}if(f){var
r=a(p[17][1],b),s=a(m[22],r),t=c(m[17],h8,s);return a(m[3],t)}var
l=d[1],n=l[1][2],e=aU(l[2][3],n)[1+n];continue}return e}}var
v=c(p[17][68],t,l),w=[0,k,c(p[18],v,[0,s,0])];return dx(i,a(h[40],w))}return a(q[68][8],b)}function
a7(e,f){function
b(b){var
d=a(F[35][4],b);function
k(l,j){if(0===l)return cX(b,j);var
e=c(h[3],d,j);if(9===e[0]){var
f=e[2];if(2===f.length-1){var
m=e[1],n=f[2],g=c(h[3],d,f[1]);if(9===g[0]){var
i=g[2];if(2===i.length-1){var
o=g[1],p=i[1],q=cX(b,i[2]),r=k(l-1|0,n),s=[0,m,[0,a(h[23],[0,o,[0,p,q]]),r]];return a(h[23],s)}}throw[0,ba,iq]}}throw[0,ba,ip]}var
g=a(p[17][1],e),i=a(p[17][1],f)-g|0,l=a(F[35][6],b),m=a(p[17][9],e),o=c5(d,function(b,a){return k(i,a)},m,l);return n(j[3],ir,o,2)}return a(q[68][8],b)}function
a8(i,m){var
b=m[2],h=m[1];switch(h[0]){case
0:var
n=h[2],j=h[1];if(0===b[0]){var
o=b[1],D=b[2],E=Y(o),F=Y(j);if(c(l[19],F,E)){var
p=a8([0,is,i],[0,n,b]),G=p[1],H=[0,j,p[2]],q=f(A),I=g===q?A[1]:d===q?a(e[2],A):A;return[0,[0,s(i,it,I),G],H]}var
r=a8([0,iu,i],[0,h,D]),J=r[1],K=[0,o,r[2]],t=f(aq),L=g===t?aq[1]:d===t?a(e[2],aq):aq;return[0,[0,s(i,iv,L),J],K]}var
M=Y(b),N=Y(j);if(c(l[19],N,M)){var
u=a8([0,iw,i],[0,n,b]),O=u[1],P=[0,j,u[2]],v=f(A),Q=g===v?A[1]:d===v?a(e[2],A):A;return[0,[0,s(i,ix,Q),O],P]}var
w=f(ar),R=[0,b,h],S=0,T=g===w?ar[1]:d===w?a(e[2],ar):ar;return[0,[0,s(i,iy,T),S],R];case
3:var
af=h[1];switch(b[0]){case
0:var
k=0;break;case
3:var
ag=[3,c(C[12],af,b[1])];return[0,[0,$(i),0],ag];default:var
k=1}break;default:var
k=0}if(!k)if(0===b[0]){var
x=b[1],U=b[2],V=Y(h),W=Y(x);if(c(l[19],W,V)){var
y=a8([0,iz,i],[0,h,U]),X=y[1],Z=[0,x,y[2]],z=f(aq),_=g===z?aq[1]:d===z?a(e[2],aq):aq;return[0,[0,s(i,iA,_),X],Z]}return[0,0,[0,h,b]]}var
aa=Y(b),ab=Y(h);if(c(l[18],ab,aa)){var
B=f(ar),ac=[0,b,h],ad=0,ae=g===B?ar[1]:d===B?a(e[2],ar):ar;return[0,[0,s(i,iB,ae),ad],ac]}return[0,0,[0,h,b]]}function
iC(o,E,i,D,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],r=j[1],t=r[2],G=r[1];if(q===t){var
u=f(bG),H=g===u?bG[1]:d===u?a(e[2],bG):bG,v=s(b,iD,H),I=l[11],J=c(C[14],D,G),K=c(C[14],E,F),L=c(C[12],K,J);if(c(l[1],L,I)){var
w=f(U),N=g===w?U[1]:d===w?a(e[2],U):U,O=s(b,iE,N),P=[0,O,h(b,[0,k,n])];return[0,v,[0,$([0,iG,[0,iF,b]]),P]]}return[0,v,h([0,iH,b],[0,k,n])]}if(c(l[19],q,t)){var
Q=h([0,iI,b],[0,k,j]),x=f(T),R=g===x?T[1]:d===x?a(e[2],T):T;return[0,s(b,iJ,R),Q]}var
S=h([0,iK,b],[0,i,n]),y=f(M),V=g===y?M[1]:d===y?a(e[2],M):M;return[0,s(b,iL,V),S]}var
W=h([0,iM,b],[0,k,0]),z=f(T),X=g===z?T[1]:d===z?a(e[2],T):T;return[0,s(b,iN,X),W]}var
A=m[2];if(A){var
Y=h([0,iO,b],[0,0,A[2]]),B=f(M),Z=g===B?M[1]:d===B?a(e[2],M):M;return[0,s(b,iP,Z),Y]}return[0,a7(o,b),0]}return h(o,[0,i,b])}function
c7(o,i,E,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],r=j[1],t=r[2],G=r[1];if(q===t){var
u=f(bJ),H=g===u?bJ[1]:d===u?a(e[2],bJ):bJ,v=s(b,iQ,H),I=l[11],J=c(C[14],E,G),K=c(C[12],F,J);if(c(l[1],K,I)){var
w=f(U),L=g===w?U[1]:d===w?a(e[2],U):U,N=s(b,iR,L),O=[0,N,h(b,[0,k,n])];return[0,v,[0,$([0,iT,[0,iS,b]]),O]]}return[0,v,h([0,iU,b],[0,k,n])]}if(c(l[19],q,t)){var
P=h([0,iV,b],[0,k,j]),x=f(A),Q=g===x?A[1]:d===x?a(e[2],A):A;return[0,s(b,iW,Q),P]}var
R=h([0,iX,b],[0,i,n]),y=f(M),S=g===y?M[1]:d===y?a(e[2],M):M;return[0,s(b,iY,S),R]}var
T=h([0,iZ,b],[0,k,0]),z=f(A),V=g===z?A[1]:d===z?a(e[2],A):A;return[0,s(b,i0,V),T]}var
B=m[2];if(B){var
W=h([0,i1,b],[0,0,B[2]]),D=f(M),X=g===D?M[1]:d===D?a(e[2],M):M;return[0,s(b,i2,X),W]}return[0,a7(o,b),0]}return h(o,[0,i,b])}function
dy(h,b){if(b){var
m=b[2];if(c(l[4],b[1][1],l[11]))var
i=f(bH),n=g===i?bH[1]:d===i?a(e[2],bH):bH,j=n;else
var
k=f(bI),p=g===k?bI[1]:d===k?a(e[2],bI):bI,j=p;var
o=s(h,i3,j);return[0,o,dy(h,m)]}return[0,$(h),0]}function
cC(j,i,b){switch(b[0]){case
0:var
u=b[2],k=cC([0,i4,j],i,b[1]),v=k[2],w=k[1],m=cC([0,i5,j],i,u),x=[0,v,m[2]],y=c(p[18],w,m[1]),o=f(bV),z=g===o?bV[1]:d===o?a(e[2],bV):bV;return[0,[0,s(j,i6,z),y],x];case
1:var
q=b[2],A=b[1];if(3===q[0]){var
C=[1,A,[3,c(l[8],i,q[1])]],D=[0,$([0,i8,j]),0],r=f(bu),E=g===r?bu[1]:d===r?a(e[2],bu):bu;return[0,[0,s(j,i9,E),D],C]}var
B=a(ac[3],i7);return n(_[6],0,0,B);case
2:return[0,0,[1,b,[3,i]]];case
3:var
F=[3,c(l[8],i,b[1])];return[0,[0,$(j),0],F];default:var
H=b[1],I=[0,G(i),H],t=f(K),J=g===t?K[1]:d===t?a(e[2],K):K;return[0,0,[4,a(h[23],[0,J,I])]]}}function
c8(c){function
h(b,i){if(i){var
k=h([0,i_,b],i[2]),j=f(bK),l=g===j?bK[1]:d===j?a(e[2],bK):bK;return[0,s(b,i$,l),k]}return[0,a7(c,b),0]}return function(a){return h(c,a)}}function
ja(c){function
h(b,i){if(i){var
k=h([0,jb,b],i[2]),j=f(A),l=g===j?A[1]:d===j?a(e[2],A):A;return[0,s(b,jc,l),k]}return[0,a7(c,b),0]}return function(a){return h(c,a)}}function
c9(c){function
h(b,i){if(i){var
k=h([0,jd,b],i[2]),j=f(T),l=g===j?T[1]:d===j?a(e[2],T):T;return[0,s(b,je,l),k]}return[0,a7(c,b),0]}return function(a){return h(c,a)}}function
c_(i,b){switch(b[0]){case
0:var
u=b[2],j=c_([0,jf,i],b[1]),v=j[2],w=j[1],k=c_([0,jg,i],u),x=[0,v,k[2]],y=c(p[18],w,k[1]),m=f(bW),z=g===m?bW[1]:d===m?a(e[2],bW):bW;return[0,[0,s(i,jh,z),y],x];case
1:var
o=b[2],A=b[1];if(3===o[0]){var
C=[1,A,[3,a(l[17],o[1])]],D=[0,$([0,jj,i]),0],q=f(bX),E=g===q?bX[1]:d===q?a(e[2],bX):bX;return[0,[0,s(i,jk,E),D],C]}var
B=a(ac[3],ji);return n(_[6],0,0,B);case
2:var
F=[1,b,[3,l[14]]],r=f(V),G=0,H=g===r?V[1]:d===r?a(e[2],V):V;return[0,[0,s(i,jl,H),G],F];case
3:var
I=[3,a(l[17],b[1])];return[0,[0,$(i),0],I];default:var
J=[0,b[1]],t=f(L),K=g===t?L[1]:d===t?a(e[2],L):L;return[0,0,[4,a(h[23],[0,K,J])]]}}function
ak(k,j,r){function
u(o,e){try{try{var
g=a2[1],i=a(h[104],k),l=n(p[17][115],i,e,g),d=l}catch(b){b=t(b);if(b!==I)throw b;var
d=a(m[3],fk)}var
b=d[1],r=a(h[11],d[2]),s=[0,[0,c6(j,a(h[11],b),r),0],[2,b]];return s}catch(b){b=t(b);if(a(_[18],b)){var
c=fa(0),f=af(0);fl(e,c,f,o);var
q=a(h[11],f);return[0,[0,c6(j,a(h[11],c),q),0],[2,c]]}throw b}}try{var
o=cB(k,r);if(typeof
o==="number")var
i=0;else
switch(o[0]){case
0:var
v=[0,0,[2,o[1]]],i=1;break;case
1:var
w=o[1];if(typeof
w==="number")if(16<=w)var
i=0;else{switch(w){case
0:var
x=o[2];if(x){var
z=x[2];if(z)if(z[2])var
i=0,b=0;else
var
ai=z[1],P=ak(k,[0,jm,j],x[1]),aj=P[2],al=P[1],Q=ak(k,[0,jn,j],ai),am=Q[1],R=a8(j,[0,aj,Q[2]]),an=R[2],ao=c(p[18],am,R[1]),q=[0,c(p[18],al,ao),an],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
A=o[2];if(A){var
B=A[2];if(B)if(B[2])var
i=0,b=0;else{var
ap=B[1],S=ak(k,[0,jo,j],A[1]),C=S[2],T=S[1],U=ak(k,[0,jp,j],ap),D=U[2],V=U[1];if(3===D[0])var
Y=cC(j,D[1],C),av=Y[2],aw=c(p[18],V,Y[1]),E=[0,c(p[18],T,aw),av];else
if(3===C[0])var
aq=C[1],W=f(bv),ar=g===W?bv[1]:d===W?a(e[2],bv):bv,as=s(j,jq,ar),X=cC(j,aq,D),at=X[2],au=c(p[18],V,[0,as,X[1]]),E=[0,c(p[18],T,au),at];else
var
E=u(0,r);var
q=E,b=1}else
var
i=0,b=0}else
var
i=0,b=0;break;case
2:var
F=o[2];if(F){var
H=F[2];if(H)if(H[2])var
i=0,b=0;else
var
ax=F[1],ay=[0,H[1]],Z=f(L),az=g===Z?L[1]:d===Z?a(e[2],L):L,aA=[0,ax,a(h[23],[0,az,ay])],$=f(y),aB=g===$?y[1]:d===$?a(e[2],y):y,aa=ak(k,j,a(h[23],[0,aB,aA])),aC=aa[2],aE=aa[1],q=[0,[0,O(hX),aE],aC],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
3:var
J=o[2];if(J)if(J[2])var
i=0,b=0;else
var
aF=J[1],aG=[0,aF,G(l[12])],ab=f(y),aH=g===ab?y[1]:d===ab?a(e[2],y):y,ac=ak(k,j,a(h[23],[0,aH,aG])),aI=ac[2],aJ=ac[1],q=[0,[0,O(hT),aJ],aI],b=1;else
var
i=0,b=0;break;case
4:var
K=o[2];if(K)if(K[2])var
i=0,b=0;else
var
ad=ak(k,[0,jr,j],K[1]),aK=ad[1],ae=c_(j,ad[2]),aL=ae[2],q=[0,c(p[18],aK,ae[1]),aL],b=1;else
var
i=0,b=0;break;case
5:var
M=o[2];if(M)if(M[2])var
i=0,b=0;else
var
aM=M[1],aN=[0,aM,G(l[14])],ag=f(y),aO=g===ag?y[1]:d===ag?a(e[2],y):y,ah=ak(k,j,a(h[23],[0,aO,aN])),aP=ah[2],aQ=ah[1],q=[0,[0,O(hV),aQ],aP],b=1;else
var
i=0,b=0;break;case
15:var
N=o[2];if(N)if(N[2])var
i=0,b=0;else
var
q=u(1,N[1]),b=1;else
var
i=0,b=0;break;case
12:case
13:case
14:try{var
aR=[0,0,[3,h4(k,r)]],q=aR,b=1}catch(c){c=t(c);if(!a(_[18],c))throw c;var
q=u(0,r),b=1}break;default:var
i=0,b=0}if(b)var
v=q,i=1}else
var
i=0;break;default:var
i=0}if(!i)var
v=u(0,r);return v}catch(b){b=t(b);if(a(aD[4],b))return u(0,r);throw b}}function
dz(h,c,b){switch(c[0]){case
1:var
i=c[1],t=c[2];switch(b[0]){case
1:if(2===i[0]){var
u=[1,[2,i[1]],[0,c[2],b[2]]],j=f(bT),v=g===j?bT[1]:d===j?a(e[2],bT):bT;return[0,s(h,jt,v),u]}break;case
2:var
w=[1,[2,b[1]],[0,t,[3,l[12]]]],k=f(bS),x=g===k?bS[1]:d===k?a(e[2],bS):bS;return[0,s(h,ju,x),w]}break;case
2:var
o=c[1];switch(b[0]){case
1:var
y=[1,[2,o],[0,b[2],[3,l[12]]]],p=f(bR),z=g===p?bR[1]:d===p?a(e[2],bR):bR;return[0,s(h,jv,z),y];case
2:var
A=[1,[2,o],[3,l[13]]],q=f(bQ),B=g===q?bQ[1]:d===q?a(e[2],bQ):bQ;return[0,s(h,jw,B),A]}break}aK(c);a(m[36],0);aK(b);a(m[36],0);a(m[52],m[1][27]);var
r=a(ac[3],js);return n(_[6],0,0,r)}function
cD(i,b){switch(b[0]){case
1:var
j=b[1];if(2===j[0]){var
k=b[2],p=j[1];if(3===k[0])return[0,0,b];var
h=function(b){switch(b[0]){case
0:var
d=b[1],e=h(b[2]),f=h(d);return c(C[12],f,e);case
3:return b[1];default:var
g=a(ac[3],jy);return n(_[6],0,0,g)}},q=[1,[2,p],[3,h(k)]];return[0,[0,$([0,jz,i]),0],q]}break;case
2:var
r=[1,[2,b[1]],[3,l[12]]],m=f(bP),t=0,u=g===m?bP[1]:d===m?a(e[2],bP):bP;return[0,[0,s(i,jA,u),t],r]}aK(b);var
o=a(ac[3],jx);return n(_[6],0,0,o)}function
a9(b,j){switch(j[0]){case
0:var
h=j[2],i=j[1];switch(h[0]){case
0:var
k=h[1],A=h[2],B=Y(k);if(Y(i)===B){var
m=dz([0,jB,b],i,k),C=m[2],D=m[1],n=f(bt),E=g===n?bt[1]:d===n?a(e[2],bt):bt,F=s(b,jC,E),o=a9(b,[0,C,A]);return[0,[0,F,[0,D,o[1]]],o[2]]}var
q=cD([0,jD,b],i),G=q[2],H=q[1],r=a9([0,jE,b],h),I=[0,G,r[2]];return[0,c(p[18],H,r[1]),I];case
3:var
O=h[1],x=cD([0,jH,b],i);return[0,x[1],[0,x[2],[3,O]]];default:var
J=Y(h);if(Y(i)===J){var
t=dz(b,i,h),K=t[1],u=a9(b,t[2]);return[0,[0,K,u[1]],u[2]]}var
v=cD([0,jF,b],i),L=v[2],M=v[1],w=a9([0,jG,b],h),N=[0,L,w[2]];return[0,c(p[18],M,w[1]),N]}case
3:return[0,0,j];default:var
y=cD(b,j),P=y[1],Q=[0,y[2],[3,l[11]]],z=f(bU),R=g===z?bU[1]:d===z?a(e[2],bU):bU,S=[0,s(b,jI,R),0];return[0,c(p[18],P,S),Q]}}function
c$(i,b){if(0===b[0]){var
h=b[1];if(1===h[0])if(2===h[1][0]){var
k=h[2];if(3===k[0]){var
o=b[2];if(c(l[1],k[1],l[11])){var
m=f(U),p=g===m?U[1]:d===m?a(e[2],U):U,q=s(i,jK,p),n=c$(i,o);return[0,[0,q,n[1]],n[2]]}}}var
j=c$([0,jJ,i],b[2]);return[0,j[1],[0,h,j[2]]]}return[0,0,b]}function
dA(bp){var
i=a(D[1][6],jL),m=a(D[1][6],jM),n=a(D[1][6],jN),y=G(l[11]);function
o(bq){var
p=bq;for(;;){if(p){var
b=p[1];switch(b[0]){case
0:var
al=b[2],am=b[1],br=p[2],bs=b[4],bt=b[3],r=J(am[1]),an=B(N(am)),ap=B(N(al)),F=G(bt),U=G(bs),aq=ad(c0(ap,F),U),bu=cz(an,aq),bv=al[3],bG=a(c9(jO),bv),bH=j[H],bI=P(bG),bJ=[0,c(k[65][3],bI,bH),0],bK=[0,j[62],[0,j[H],0]],bP=[0,O(aG),bK],bQ=[0,a(k[65][22],bP),0],bR=[0,j[62],[0,j[H],0]],bS=[0,O(aG),bR],bT=[0,a(k[65][22],bS),0],bU=[0,o(br),0],bV=[0,a(j[25],[0,r,0]),bU],bW=[0,a(j[76],[0,m,[0,n,[0,r,0]]]),bV],bY=a(h[11],r),bZ=a(h[11],n),b0=[0,F,ap,U,a(h[11],m),bZ,bY],ar=f(bw),bX=0,b1=g===ar?bw[1]:d===ar?a(e[2],bw):bw,b2=[0,u([0,a(h[23],[0,b1,b0]),bX]),bW],b3=[0,a(j[25],[0,m,[0,n,0]]),b2],b4=[0,a(k[65][22],b3),bT],b5=aH(F,y),b6=a(j[Z],b5),b7=[0,c(k[65][21],b6,b4),bQ],b8=aH(F,U),b9=[0,a(j[Z],b8),0],b_=[0,a(j[25],[0,r,0]),b9],b$=[0,a(j[76],[0,i,[0,r,0]]),b_],cb=a(h[11],r),cc=[0,an,aq,a(h[11],i),cb],as=f(bx),ca=0,cd=g===as?bx[1]:d===as?a(e[2],bx):bx,ce=[0,u([0,a(h[23],[0,cd,cc]),ca]),b$],cf=[0,a(j[25],[0,i,0]),ce],cg=a(k[65][22],cf),ch=[0,c(k[65][21],cg,b7),bJ],ci=a(j[Z],bu);return c(k[65][21],ci,ch);case
1:var
K=b[2],L=b[1],at=c(l[26],L[4],K),cj=c(C[14],at,K),ck=c(C[13],L[4],cj),cl=L[3],cm=function(a){return c(l[9],a,K)},cn=c(l[43],cm,cl),au=[0,L[1],0,cn,at],co=B(N(au)),av=G(K),W=G(ck),cp=au[3],cq=a(c9(jP),cp),cr=[0,j[62],[0,j[H],0]],cs=[0,O(aG),cr],ct=[0,a(k[65][22],cs),0],cu=[0,j[62],[0,j[H],0]],cv=[0,O(aG),cu],cw=[0,a(k[65][22],cv),0],cy=[0,j[42],0],cA=[0,P(cq),cy],cB=[0,bb(i),cA],cC=[0,a(j[25],[0,i,0]),cB],cD=[0,O(cZ),cC],cE=[0,a(j[76],[0,m,[0,n,0]]),cD],cG=a(h[11],n),cH=[0,W,av,co,a(h[11],m),cG],aw=f(bA),cF=0,cI=g===aw?bA[1]:d===aw?a(e[2],bA):bA,cJ=[0,u([0,a(h[23],[0,cI,cH]),cF]),cE],cK=[0,a(j[25],[0,n,[0,m,0]]),cJ],cL=[0,a(k[65][22],cK),cw],cM=aH(av,W),cN=a(j[Z],cM),cO=[0,c(k[65][21],cN,cL),ct],cP=aH(W,y),cQ=a(j[Z],cP);return c(k[65][21],cQ,cO);case
3:var
ax=p[2],ay=b[2],M=b[1],v=J(M[1]),cR=function(a){return c(l[9],a,ay)},Y=c(l[44],cR,M),_=B(N(M)),$=B(N(Y)),Q=G(ay),az=cz(_,c0($,Q));if(2===M[2]){var
cS=Y[3],cT=a(c8(jQ),cS),cU=j[H],cV=P(cT),cW=[0,c(k[65][3],cV,cU),0],cX=[0,o(ax),0],cY=[0,a(j[25],[0,v,0]),cX],c1=[0,a(j[76],[0,m,[0,v,0]]),cY],c3=a(h[11],v),c4=[0,_,$,Q,a(h[11],m),c3],aA=f(bM),c2=0,c5=g===aA?bM[1]:d===aA?a(e[2],bM):bM,c6=[0,u([0,a(h[23],[0,c5,c4]),c2]),c1],c_=[0,a(j[25],[0,m,0]),c6],c$=[0,a(k[65][22],c_),cW],da=a(j[Z],az);return c(k[65][21],da,c$)}var
db=Y[3],dc=a(c8(jR),db),dd=j[H],de=P(dc),df=[0,c(k[65][3],de,dd),0],dg=[0,j[62],[0,j[H],0]],dh=[0,O(aG),dg],di=[0,a(k[65][22],dh),0],dj=[0,o(ax),0],dk=[0,a(j[25],[0,v,0]),dj],dl=[0,a(j[76],[0,m,[0,n,[0,v,0]]]),dk],dp=a(h[11],v),dq=a(h[11],m),ds=[0,_,$,Q,a(h[11],n),dq,dp],aB=f(bz),dm=0,dt=g===aB?bz[1]:d===aB?a(e[2],bz):bz,du=[0,u([0,a(h[23],[0,dt,ds]),dm]),dl],dv=[0,a(j[25],[0,n,[0,m,0]]),du],dw=[0,a(k[65][22],dv),di],dx=aH(Q,y),dz=a(j[Z],dx),dA=[0,c(k[65][21],dz,dw),df],dC=a(j[Z],az);return c(k[65][21],dC,dA);case
4:var
aC=p[2],aD=b[3],A=aD[2],R=aD[1],aE=b[2],w=aE[2],aa=aE[1],dD=b[1],ab=af(0);aF(ab,dD);var
aJ=J(w[1]),aK=J(A[1]),aL=B(N(w)),aN=B(N(A));if(c(l[1],aa,l[12]))if(0===A[2]){switch(w[2]){case
0:var
aO=f(bB),dE=g===aO?bB[1]:d===aO?a(e[2],bB):bB,ac=dE;break;case
1:var
aQ=f(bC),dO=g===aQ?bC[1]:d===aQ?a(e[2],bC):bC,ac=dO;break;default:var
aR=f(bO),dP=g===aR?bO[1]:d===aR?a(e[2],bO):bO,ac=dP}var
dF=G(R),dG=2===w[2]?jS:jT,dH=c7(dG,w[3],R,A[3]),dI=[0,o(aC),0],dJ=[0,a(j[25],[0,ab,0]),dI],dK=[0,P(dH),dJ],dL=a(h[11],aK),dM=[0,ac,[0,aL,aN,dF,a(h[11],aJ),dL]],dN=[0,u([0,a(h[23],dM),0]),dK];return a(k[65][22],dN)}var
aS=G(aa),aT=G(R),dQ=iC(jU,aa,w[3],R,A[3]),dR=[0,j[62],[0,j[H],0]],dS=[0,O(aG),dR],dT=[0,a(k[65][22],dS),0],dU=[0,j[62],[0,j[H],0]],dV=[0,O(aG),dU],dW=[0,a(k[65][22],dV),0],dX=[0,o(aC),0],dY=[0,a(j[25],[0,ab,0]),dX],dZ=[0,P(dQ),dY],d0=[0,a(j[76],[0,m,[0,n,0]]),dZ],d2=a(h[11],aK),d3=a(h[11],aJ),d4=a(h[11],n),d5=[0,aL,aN,aS,aT,a(h[11],m),d4,d3,d2],aU=f(bD),d1=0,d6=g===aU?bD[1]:d===aU?a(e[2],bD):bD,d7=[0,u([0,a(h[23],[0,d6,d5]),d1]),d0],d8=[0,a(j[25],[0,n,[0,m,0]]),d7],d9=[0,a(k[65][22],d8),dW],d_=aH(aT,y),d$=a(j[Z],d_),ea=[0,c(k[65][21],d$,d9),dT],eb=aH(aS,y),ec=a(j[Z],eb);return c(k[65][21],ec,ea);case
5:var
E=b[1],aV=E[5],aW=E[4],ae=E[3],aX=E[2],ed=p[2],ee=E[1],aY=af(0),ef=J(ae[1]);aF(aY,ee[1]);var
ag=B(N(aX)),eg=B(N(ae)),ah=dn(aV),eh=cz(a(h[10],1),ag),aZ=f(x),ei=g===aZ?x[1]:d===aZ?a(e[2],x):x,ej=[0,c(z[4],[0,ah],0),ei,eh],ek=a(h[21],ej),a0=f(x),el=g===a0?x[1]:d===a0?a(e[2],x):x,a1=f(cx),em=[0,el,ek],en=g===a1?cx[1]:d===a1?a(e[2],cx):cx,eo=a(h[23],[0,en,em]),ep=G(aW),eq=c7(dB,ae[3],aW,[0,[0,l[14],aV],aX[3]]),a2=f(V),er=g===a2?V[1]:d===a2?a(e[2],V):V,es=[0,s([0,jY,[0,jX,[0,jW,dB]]],jV,er),eq],et=j[H],eu=ff(ag),ev=[0,c(k[65][3],eu,et),0],ew=[0,o(ed),0],ex=[0,a(j[25],[0,aY,0]),ew],ey=[0,a(j[76],[0,i,0]),ex],ez=[0,P(es),ey],eB=a(h[11],i),eC=a(h[11],ef),eD=[0,a(h[11],ah),eg,ag,ep,eC,eB],a3=f(bF),eA=0,eE=g===a3?bF[1]:d===a3?a(e[2],bF):bF,eF=[0,u([0,a(h[23],[0,eE,eD]),eA]),ez],eG=[0,a(j[25],[0,ah,[0,i,0]]),eF],eH=[0,a(j[76],[0,i,0]),eG],eI=[0,aP(i),eH],eJ=[0,a(j[25],[0,i,0]),eI],eK=[0,a(k[65][22],eJ),ev],eL=a(j[Z],eo);return c(k[65][21],eL,eK);case
6:var
a4=p[2],eM=b[1];try{var
eN=o(a4),eO=J(eM[1]),eP=c(D[1][13][3],eO,bp),eQ=c(k[65][3],eP,eN);return eQ}catch(a){a=t(a);if(a===I){var
p=a4;continue}throw a}case
9:var
a5=b[2],ai=b[1],eR=N(ai),eS=N(a5),eT=dy(jZ,ai[3]),a6=f(ao),eU=g===a6?ao[1]:d===a6?a(e[2],ao):ao,a7=f(ao),eV=g===a7?ao[1]:d===a7?a(e[2],ao):ao,a8=f(bh),eW=g===a8?bh[1]:d===a8?a(e[2],bh):bh,a9=f(X),eX=[0,eW,eV,eU],eY=g===a9?X[1]:d===a9?a(e[2],X):X,eZ=a(h[23],[0,eY,eX]),e0=[0,j[42],[0,j[H],0]],e1=[0,a(j0[1],eZ),0],e2=[0,j[62],[0,j[16],e1]],e3=[0,O(dr),e2],e4=a(k[65][22],e3),e5=c(k[65][21],e4,e0),e6=J(a5[1]),e7=a(h[11],e6),e8=J(ai[1]),e9=a(h[11],e8),e_=B(eS),e$=[0,B(eR),e_,e9,e7],a_=f(by),fa=g===a_?by[1]:d===a_?a(e[2],by):by,fb=a(h[23],[0,fa,e$]),fc=P(eT),fd=u([0,fb,0]),fe=c(k[65][3],fd,fc);return c(q[18],fe,e5);case
10:var
aj=b[2],ak=b[1],fg=b[3],fh=N(aj),fi=N(ak),fj=J(aj[1]),fk=J(ak[1]),a$=fg?l[14]:l[12],fl=c7(j1,aj[3],a$,ak[3]),fm=[0,j[H],0],fn=[0,bb(i),fm],fo=[0,a(j[25],[0,i,0]),fn],fp=[0,P(fl),fo],fr=a(h[11],fk),fs=a(h[11],fj),ft=G(a$),fu=B(fi),fv=[0,B(fh),fu,ft,fs,fr],ba=f(bL),fq=0,fw=g===ba?bL[1]:d===ba?a(e[2],bL):bL,fx=[0,u([0,a(h[23],[0,fw,fv]),fq]),fp];return a(k[65][22],fx);case
11:var
S=b[2],fy=p[2],fz=b[3],fA=b[1],bc=af(0);aF(bc,fA);var
bd=J(S[1]),be=J(fz),bf=B(N(S)),bg=B(N(a(l[45],S))),fB=S[3],fC=a(c8(j2),fB),bi=f(V),fD=g===bi?V[1]:d===bi?a(e[2],V):V,fE=[0,s(j4,j3,fD),fC],fF=j[H],fG=P(fE),fH=[0,c(k[65][3],fG,fF),0],fI=[0,o(fy),0],fJ=[0,a(j[25],[0,bc,0]),fI],fK=[0,a(j[76],[0,bd,[0,be,[0,i,0]]]),fJ],fM=a(h[11],i),fN=a(h[11],be),fO=[0,bf,bg,a(h[11],bd),fN,fM],bj=f(bE),fL=0,fP=g===bj?bE[1]:d===bj?a(e[2],bE):bE,fQ=[0,u([0,a(h[23],[0,fP,fO]),fL]),fK],fR=[0,a(j[25],[0,i,0]),fQ],fS=[0,a(k[65][22],fR),fH],fT=cz(bf,aI(bg)),fU=a(j[Z],fT);return c(k[65][21],fU,fS);case
12:var
fV=j5[15],fW=J(b[1]),fX=u([0,a(h[11],fW),0]);return c(k[65][3],fX,fV);case
13:var
fY=j[H],fZ=bb(J(b[1]));return c(k[65][3],fZ,fY);case
14:var
f0=b[1],f1=[0,j[H],0],f2=[0,bb(i),f1],f3=[0,a(j[25],[0,i,0]),f2],f4=[0,O(cZ),f3],f5=[0,j[62],f4],f6=[0,O(dr),f5],f7=J(f0),f8=[0,u([0,a(h[11],f7),0]),f6];return a(k[65][22],f8);case
15:var
bk=b[3],bl=b[2],T=b[1],f9=bk[2],f_=bk[1],f$=bl[2],ga=bl[1],bm=af(0),bn=af(0);aF(bm,ga);aF(bn,f_);var
gb=J(T[1]),gc=T[3],gd=a(ja(j6),gc),ge=T[3],gf=a(c9(j7),ge),gg=B(N(T)),gh=[0,o(f9),0],gi=[0,a(j[25],[0,bn,0]),gh],gj=[0,P(gf),gi],gk=[0,a(k[65][22],gj),0],gl=[0,o(f$),0],gm=[0,a(j[25],[0,bm,0]),gl],gn=[0,P(gd),gm],go=[0,a(k[65][22],gn),gk],gp=[0,gg,[0,a(h[11],gb),0]],bo=f(bN),gq=g===bo?bN[1]:d===bo?a(e[2],bN):bN,gr=a(h[40],[0,gq,gp]),gs=a(j[aM],gr);return c(k[65][21],gs,go)}}return a(q[16],0)}}return o}function
aS(O,w,N,M,L,K,J,I,v){var
x=v[2],y=v[1],g=[0,[0,L],j8],q=ak(O,g,K),E=q[1],r=a9(g,q[2]),F=r[1],s=c$(g,r[2]),G=s[2],H=c(p[18],F,s[1]),t=c(p[18],E,H),Q=a(j[76],[0,w,0]),R=a(k[65][24],Q),S=[0,M,[0,J,I,a(h[11],w)]],T=u([0,a(h[23],S),0]),U=c(k[65][3],T,R);if(a(p[17][48],t))return[0,y,x];var
i=af(0),e=0,b=G;for(;;){switch(b[0]){case
0:var
f=b[1];if(1===f[0]){var
l=f[1];if(2===l[0]){var
m=f[2];if(3===m[0]){var
B=b[2],C=m[1],e=[0,[0,C,cW(l[1])],e],b=B;continue}var
d=0}else
var
d=0}else
var
d=0;break;case
3:var
D=b[1],o=be(0);aF(i,o);var
z=[0,o,N,a(p[17][9],e),D],d=1;break;default:var
d=0}if(!d)var
A=a(ac[3],ii),z=n(_[3],0,0,A);var
V=[0,a(j[25],[0,i,0]),0],W=[0,U,[0,P(t),V]];return[0,[0,[0,i,a(k[65][22],W)],y],[0,z,x]]}}function
j9(ab,k,i,H){var
m=H[1],ac=H[2],ae=a(a0[3],m);if(c(p[15][34],ae,j_))return i;try{var
j=aR(k,ac);if(typeof
j==="number")var
h=0;else
if(1===j[0]){var
r=j[1];if(typeof
r==="number")if(16<=r){switch(r+cP|0){case
0:var
s=j[2];if(s){var
u=s[2];if(u){var
v=u[2];if(v)if(v[2])var
h=0,b=0;else{var
J=v[1],K=u[1],q=a5(ab,k,s[1]);if(typeof
q==="number")var
o=0;else
if(1===q[0]){var
N=q[1];if(typeof
N==="number")if(23===N)if(q[2])var
o=0;else
var
L=1,o=1;else
var
o=0;else
var
o=0}else
var
o=0;if(!o)var
L=0;if(L)var
af=ad(K,aI(J)),M=f(bY),ag=2,ah=g===M?bY[1]:d===M?a(e[2],bY):bY,n=aS(k,m,0,ah,ag,af,K,J,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0}else
var
h=0,b=0}else
var
h=0,b=0;break;case
2:var
w=j[2];if(w){var
x=w[2];if(x)if(x[2])var
h=0,b=0;else
var
O=x[1],P=w[1],ai=ad(P,aI(O)),Q=f(bZ),aj=1,ak=g===Q?bZ[1]:d===Q?a(e[2],bZ):bZ,n=aS(k,m,2,ak,aj,ai,P,O,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
3:var
y=j[2];if(y){var
z=y[2];if(z)if(z[2])var
h=0,b=0;else
var
R=z[1],S=y[1],al=ad(R,aI(S)),T=f(b3),am=2,an=g===T?b3[1]:d===T?a(e[2],b3):b3,n=aS(k,m,1,an,am,al,S,R,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
4:var
A=j[2];if(A){var
B=A[2];if(B)if(B[2])var
h=0,b=0;else
var
U=B[1],V=A[1],ao=aI(V),ap=ad(ad(U,G(l[14])),ao),W=f(b0),aq=2,ar=g===W?b0[1]:d===W?a(e[2],b0):b0,n=aS(k,m,1,ar,aq,ap,V,U,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
5:var
C=j[2];if(C){var
D=C[2];if(D)if(D[2])var
h=0,b=0;else
var
X=D[1],Y=C[1],as=ad(Y,aI(X)),Z=f(b1),at=2,au=g===Z?b1[1]:d===Z?a(e[2],b1):b1,n=aS(k,m,1,au,at,as,Y,X,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
6:var
E=j[2];if(E){var
F=E[2];if(F)if(F[2])var
h=0,b=0;else
var
_=F[1],$=E[1],av=aI(_),aw=ad(ad($,G(l[14])),av),aa=f(b2),ax=2,ay=g===aa?b2[1]:d===aa?a(e[2],b2):b2,n=aS(k,m,1,ay,ax,aw,$,_,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;default:var
h=0,b=0}if(b)var
I=n,h=1}else
var
h=0;else
var
h=0}else
var
h=0;if(!h)var
I=i;return I}catch(b){b=t(b);if(a(aD[4],b))return i;throw b}}function
aT(b){var
d=a(j[22],b),e=a(j[76],[0,b,0]),f=a(k[65][24],e);return c(k[65][3],f,d)}function
j$(i){dp(0);var
w=a(F[35][14],i),x=c(F[35][1],j9,i),m=n(p[17][15],x,ka,w),o=m[1],y=m[2],z=fj(0),A=[0,a(q[16],0),0];function
B(n,m){var
c=m[2],o=c[2],i=c[1],p=m[1],q=n[2],r=n[1];if(c[3]){var
b=af(0),s=be(0);aF(b,s);var
v=l[11],w=cW(i),x=[0,[0,s,1,[0,[0,l[12],w],0],v],q],y=[0,a(j[25],[0,o,[0,b,0]]),[0,r,0]],z=[0,a(j[76],[0,b,0]),y],A=[0,aP(b),z],B=[0,a(j[25],[0,i,[0,b,0]]),A],t=f(b5),C=[0,p,0],D=g===t?b5[1]:d===t?a(e[2],b5):b5,E=a(h[40],[0,D,C]),F=[0,a(j[aM],E),B];return[0,a(k[65][22],F),x]}var
G=[0,a(j[25],[0,i,[0,o,0]]),[0,r,0]],u=f(b4),H=[0,p,0],I=g===u?b4[1]:d===u?a(e[2],b4):b4,J=a(h[40],[0,I,H]),K=[0,a(j[aM],J),G];return[0,a(k[65][22],K),q]}var
r=n(p[17][15],B,A,z),s=r[1],b=c(p[18],y,r[2]);if(cQ[1])c(l[33],a1,b);if(bd[1])try{n(l[64],[0,be,dl,a1],0,b);var
E=a(q[16],0);return E}catch(b){b=t(b);if(b===l[27]){var
C=a(l[39],0),u=n(l[65],0,0,C)[2];if(bc[1])c(l[36],a1,u);var
D=a(dA(o),u);return c(k[65][3],s,D)}throw b}try{var
v=c(l[68],[0,be,dl,a1],b);if(bc[1])c(l[36],a1,v);var
H=a(dA(o),v),I=c(k[65][3],s,H);return I}catch(b){b=t(b);if(b===l[28]){var
G=a(ac[3],kb);return c(k[65][5],0,G)}throw b}}var
kc=a(q[68][8],j$);function
kd(b){var
i=c4[81];function
n(a){return c(i,0,a)}var
aa=c(F[35][1],n,b);function
m(n,L){function
b(w){try{var
p=cB(w,L);if(typeof
p==="number")var
i=0;else
if(1===p[0]){var
x=p[1];if(typeof
x==="number")if(12<=x)var
i=0;else{switch(x){case
6:var
y=p[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else
var
A=z[1],B=y[1],ab=[0,m([0,ke,n],A),0],ac=[0,m([0,kf,n],B),ab],N=f(bi),ae=[0,B,[0,A,0]],ag=g===N?bi[1]:d===N?a(e[2],bi):bi,ai=aJ(A),aj=[0,a6(n,ad(aJ(B),ai),[0,ag,ae]),ac],u=a(k[65][22],aj),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
7:var
C=p[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
E=D[1],F=C[1],ak=[0,m([0,kg,n],E),0],al=[0,m([0,kh,n],F),ak],O=f(bj),am=[0,F,[0,E,0]],an=g===O?bj[1]:d===O?a(e[2],bj):bj,ao=aJ(E),ap=[0,a6(n,c0(aJ(F),ao),[0,an,am]),al],u=a(k[65][22],ap),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
8:var
H=p[2];if(H){var
I=H[2];if(I)if(I[2])var
i=0,b=0;else
var
r=I[1],s=H[1],v=af(0),P=f(ax),aq=0,ar=0,as=[0,r,s],at=g===P?ax[1]:d===P?a(e[2],ax):ax,az=o([0,[0,v,a(h[23],[0,at,as])],ar]),aA=[0,s,[0,r,[0,a(h[11],v),0]]],Q=f(bl),aB=g===Q?bl[1]:d===Q?a(e[2],bl):bl,aC=a6(n,G(l[11]),[0,aB,aA]),aE=[0,c(k[65][3],aC,az),aq],aF=[0,m([0,ki,n],r),0],aG=[0,m([0,kj,n],s),aF],R=f(aw),aH=0,aI=[0,r,s],aK=g===R?aw[1]:d===R?a(e[2],aw):aw,aL=[0,o([0,[0,v,a(h[23],[0,aK,aI])],aH]),aG],aN=[0,s,[0,r,[0,a(h[11],v),0]]],S=f(bk),aO=g===S?bk[1]:d===S?a(e[2],bk):bk,aP=aJ(r),aQ=[0,a6(n,h1(aJ(s),aP),[0,aO,aN]),aL],aR=[0,a(k[65][22],aQ),aE],aS=a(j[25],[0,v,0]),T=f(ce),aT=[0,r,[0,s,0]],aU=g===T?ce[1]:d===T?a(e[2],ce):ce,aV=a(h[40],[0,aU,aT]),aW=a(j[aM],aV),aX=c(k[65][3],aW,aS),u=c(k[65][21],aX,aR),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
9:var
J=p[2];if(J)if(J[2])var
i=0,b=0;else
var
U=J[1],V=f(au),aY=g===V?au[1]:d===V?a(e[2],au):au,W=f(av),aZ=[0,aY],a0=g===W?av[1]:d===W?a(e[2],av):av,a1=[0,U,a(h[23],[0,a0,aZ])],X=f(ay),a2=g===X?ay[1]:d===X?a(e[2],ay):ay,Y=a(h[23],[0,a2,a1]),a3=m(n,Y),Z=f(cd),a4=[0,U,0],a5=g===Z?cd[1]:d===Z?a(e[2],cd):cd,a7=im([0,kk,n],Y,[0,a5,a4]),u=c(k[65][3],a7,a3),b=1;else
var
i=0,b=0;break;case
10:var
K=p[2];if(K)if(K[2])var
i=0,b=0;else
var
a8=K[1],_=function(i){try{var
d=cB(w,i);if(typeof
d==="number")var
b=0;else
if(1===d[0]){var
e=d[1];if(typeof
e==="number"){if(10===e){var
f=d[2];if(f)if(f[2])var
b=0,c=0;else
var
h=_(f[1]),c=1;else
var
b=0,c=0}else
if(11===e)if(d[2])var
b=0,c=0;else
var
h=1,c=1;else
var
b=0,c=0;if(c)var
g=h,b=1}else
var
b=0}else
var
b=0;if(!b)var
g=0;return g}catch(b){b=t(b);if(a(aD[4],b))return 0;throw b}},aa=function(i,l){try{var
j=cB(w,l);if(typeof
j==="number")var
b=0;else
if(1===j[0]){var
q=j[1];if(typeof
q==="number")if(10===q){var
n=j[2];if(n)if(n[2])var
b=0;else
var
o=n[1],u=aa([0,kl,i],o),r=f(bm),v=[0,o,0],x=g===r?bm[1]:d===r?a(e[2],bm):bm,z=[0,aJ(o)],s=f(ah),y=[0,x,v],A=g===s?ah[1]:d===s?a(e[2],ah):ah,B=a6(i,a(h[23],[0,A,z]),y),p=c(k[65][3],B,u),b=1;else
var
b=0}else
var
b=0;else
var
b=0}else
var
b=0;if(!b)var
p=m(i,l);return p}catch(b){b=t(b);if(a(aD[4],b))return m(i,l);throw b}},a9=_(a8)?$(n):aa(n,L),u=a9,b=1;else
var
i=0,b=0;break;case
11:if(p[2])var
i=0,b=0;else
var
u=$(n),b=1;break;default:var
i=0,b=0}if(b)var
M=u,i=1}else
var
i=0}else
var
i=0;if(!i)var
M=a(q[16],0);return M}catch(b){b=t(b);if(a(aD[4],b))return a(q[16],0);throw b}}return c(q[73][1],q[54],b)}function
o(b){if(b){var
l=b[2],i=b[1],j=i[1],ab=i[2],n=function(ac){try{var
n=aR(ac,ab);if(typeof
n==="number")var
i=0;else
if(1===n[0]){var
q=n[1];if(typeof
q==="number")if(16<=q){switch(q+cP|0){case
0:var
r=n[2];if(r){var
s=r[2];if(s){var
v=s[2];if(v)if(v[2])var
i=0,b=0;else{var
H=v[1],I=s[1],ad=r[1],J=f(W),ae=g===J?W[1]:d===J?a(e[2],W):W;if(c(aa,ad,ae))var
af=[0,o(l),0],ag=[0,aT(j),af],ah=[0,m(km,H),ag],ai=[0,m(kn,I),ah],ak=[0,I,H,a(h[11],j)],K=f(bn),aj=0,al=g===K?bn[1]:d===K?a(e[2],bn):bn,am=[0,u([0,a(h[23],[0,al,ak]),aj]),ai],L=a(k[65][22],am);else
var
L=o(l);var
p=L,b=1}else
var
i=0,b=0}else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
w=n[2];if(w){var
x=w[2];if(x)if(x[2])var
i=0,b=0;else
var
M=x[1],N=w[1],an=[0,o(l),0],ao=[0,aT(j),an],ap=[0,m(ko,M),ao],aq=[0,m(kp,N),ap],as=[0,N,M,a(h[11],j)],O=f(bo),ar=0,at=g===O?bo[1]:d===O?a(e[2],bo):bo,au=[0,u([0,a(h[23],[0,at,as]),ar]),aq],p=a(k[65][22],au),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
15:var
y=n[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else
var
P=z[1],Q=y[1],av=[0,o(l),0],aw=[0,aT(j),av],ax=[0,m(kq,P),aw],ay=[0,m(kr,Q),ax],aA=[0,Q,P,a(h[11],j)],R=f(bp),az=0,aB=g===R?bp[1]:d===R?a(e[2],bp):bp,aC=[0,u([0,a(h[23],[0,aB,aA]),az]),ay],p=a(k[65][22],aC),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
16:var
A=n[2];if(A){var
B=A[2];if(B)if(B[2])var
i=0,b=0;else
var
S=B[1],T=A[1],aE=[0,o(l),0],aF=[0,aT(j),aE],aG=[0,m(ks,S),aF],aH=[0,m(kt,T),aG],aJ=[0,T,S,a(h[11],j)],U=f(bq),aI=0,aK=g===U?bq[1]:d===U?a(e[2],bq):bq,aL=[0,u([0,a(h[23],[0,aK,aJ]),aI]),aH],p=a(k[65][22],aL),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
17:var
C=n[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
V=D[1],X=C[1],aM=[0,o(l),0],aN=[0,aT(j),aM],aO=[0,m(ku,V),aN],aP=[0,m(kv,X),aO],aS=[0,X,V,a(h[11],j)],Y=f(br),aQ=0,aU=g===Y?br[1]:d===Y?a(e[2],br):br,aV=[0,u([0,a(h[23],[0,aU,aS]),aQ]),aP],p=a(k[65][22],aV),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
18:var
E=n[2];if(E){var
F=E[2];if(F)if(F[2])var
i=0,b=0;else
var
Z=F[1],_=E[1],aW=[0,o(l),0],aX=[0,aT(j),aW],aY=[0,m(kw,Z),aX],aZ=[0,m(kx,_),aY],a1=[0,_,Z,a(h[11],j)],$=f(bs),a0=0,a2=g===$?bs[1]:d===$?a(e[2],bs):bs,a3=[0,u([0,a(h[23],[0,a2,a1]),a0]),aZ],p=a(k[65][22],a3),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;default:var
i=0,b=0}if(b)var
G=p,i=1}else
var
i=0;else
var
i=0}else
var
i=0;if(!i)var
G=o(l);return G}catch(b){b=t(b);if(a(aD[4],b))return o(l);throw b}};return c(q[73][1],q[54],n)}return a(q[16],0)}var
r=a(F[35][14],b);return o(a(p[17][9],r))}var
ky=a(q[68][8],kd);function
kz(a){if(typeof
a==="number")if(18<=a)switch(a+de|0){case
0:return gy;case
1:return gA;case
2:return gC;case
3:return gG;case
4:return gE;case
13:return hb;case
14:return hd;case
15:return hf;case
16:return hh}throw I}function
kA(a){if(typeof
a==="number")if(18<=a)switch(a+de|0){case
0:return gJ;case
1:return gL;case
2:return gN;case
3:return gP;case
4:return gR;case
13:return hk;case
14:return hm;case
15:return ho;case
16:return hq}throw I}var
a_=[aO,kB,aL(0)];function
aa(j,i,X){var
c=aR(i,X);if(typeof
c!=="number")switch(c[0]){case
1:var
l=c[1];if(typeof
l==="number")if(16<=l)switch(l+cP|0){case
0:var
n=c[2];if(n){var
o=n[2];if(o){var
p=o[2];if(p){if(!p[2]){var
B=p[1],C=o[1],k=a5(j,i,n[1]);if(typeof
k!=="number"&&1===k[0]){var
q=k[1];if(typeof
q==="number")if(23===q){if(!k[2]){var
D=f(b6),Y=[0,C,B],Z=g===D?b6[1]:d===D?a(e[2],b6):b6;return a(h[23],[0,Z,Y])}}else
if(24===q)if(!k[2]){var
E=f(cf),_=[0,C,B],$=g===E?cf[1]:d===E?a(e[2],cf):cf;return a(h[23],[0,$,_])}}throw a_}var
b=1}else
var
b=0}else
var
b=1}else
var
b=1;break;case
9:var
u=c[2];if(u){var
v=u[2];if(v){if(!v[2]){var
G=v[1],H=u[1],ag=aa(j,i,G),ah=[0,H,G,aa(j,i,H),ag],J=f(cj),ai=g===J?cj[1]:d===J?a(e[2],cj):cj;return a(h[23],[0,ai,ah])}var
b=1}else
var
b=1}else
var
b=1;break;case
10:var
w=c[2];if(w){var
x=w[2];if(x){if(!x[2]){var
K=x[1],L=w[1],aj=aa(j,i,K),ak=[0,L,K,aa(j,i,L),aj],M=f(ci),al=g===M?ci[1]:d===M?a(e[2],ci):ci;return a(h[23],[0,al,ak])}var
b=1}else
var
b=1}else
var
b=1;break;case
11:if(!c[2]){var
N=f(cn);return g===N?cn[1]:d===N?a(e[2],cn):cn}var
b=0;break;case
12:if(!c[2]){var
O=f(cp);return g===O?cp[1]:d===O?a(e[2],cp):cp}var
b=0;break;case
13:var
y=c[2];if(y){if(!y[2]){var
P=y[1],am=[0,P,aa(j,i,P)],Q=f(cm),an=g===Q?cm[1]:d===Q?a(e[2],cm):cm;return a(h[23],[0,an,am])}var
b=0}else
var
b=1;break;case
14:var
z=c[2];if(z){var
A=z[2];if(A){if(!A[2]){var
R=A[1],S=z[1],ao=aa(j,i,R),ap=[0,S,R,aa(j,i,S),ao],T=f(cl),aq=g===T?cl[1]:d===T?a(e[2],cl):cl;return a(h[23],[0,aq,ap])}var
b=1}else
var
b=1}else
var
b=1;break;default:var
b=0}else
var
b=0;else
var
b=0;if(!b){var
r=c[2];if(r){var
s=r[2];if(s)if(!s[2]){var
ab=s[1],ac=r[1];try{var
m=kz(l),F=f(m),ad=[0,ac,ab],ae=g===F?m[1]:d===F?a(e[2],m):m,af=a(h[23],[0,ae,ad]);return af}catch(a){a=t(a);if(a===I)throw a_;throw a}}}}break;case
2:var
U=c[2],V=c[1],ar=aa(j,i,U),as=[0,V,U,aa(j,i,V),ar],W=f(ck),at=g===W?ck[1]:d===W?a(e[2],ck):ck;return a(h[23],[0,at,as])}throw a_}function
cE(d,c,b){var
e=a(q[68][4],b);return n(j[13],d,c,e)}function
ab(b,e){function
d(f){var
d=cE(D[1][10][1],b,f),g=a(e,d),h=a(j[2],d);return c(k[65][3],h,g)}var
f=a(q[68][8],d),g=a(j[76],[0,b,0]),h=a(k[65][24],g);return c(k[65][3],h,f)}function
dC(b,g){function
d(d){var
h=c(a0[5],b,kC),e=cE(D[1][10][1],h,d),i=c(a0[5],b,kD),f=cE(D[1][10][1],i,d),l=[0,c(g,e,f),0],m=[0,a(j[2],f),l],n=[0,a(j[2],e),m];return a(k[65][22],n)}var
e=a(q[68][8],d),f=a(j[76],[0,b,0]),h=a(k[65][24],f);return c(k[65][3],h,e)}function
kE(m){var
a4=a(F[35][7],m),G=a(q[68][4],m),i=a(q[68][5],m);function
x(a){return aa(G,i,a)}function
b(o){if(o){var
w=o[1];if(1===w[0]){var
p=o[2],r=w[3],s=w[1],y=w[2];if(cR[1]){var
A=function(o){try{var
e=a5(G,o,r);if(typeof
e==="number")var
d=0;else
if(1===e[0]){var
g=e[1];if(typeof
g==="number")if(1<(g-23|0)>>>0)var
d=0;else
var
q=c(a0[5],s[1],kI),i=cE(D[1][10][1],q,m),l=ds(r,a(h[11],s[1]),y),u=b([0,[0,c(z[4],i,0),l],p]),v=n(j[140],[0,i],l,j[H]),f=c(k[65][3],v,u),d=1;else
var
d=0}else
var
d=0;if(!d)var
f=b(p);return f}catch(c){c=t(c);if(a(aD[4],c))return b(p);throw c}};return c(q[73][1],q[54],A)}}var
l=o[2],i=a(z[11][1][2],w),v=function(A){try{var
s=aR(A,a(z[11][1][4],w));if(typeof
s==="number")var
m=0;else
switch(s[0]){case
1:var
O=s[1];if(typeof
O==="number")if(18<=O){switch(O+de|0){case
7:var
P=s[2];if(P){var
Q=P[2];if(Q)if(Q[2])var
m=0,o=0;else
var
a6=Q[1],a7=P[1],a8=dC(i,function(d,a){var
e=[0,[0,c(z[4],a,0),a6],l];return b([0,[0,c(z[4],d,0),a7],e])}),a9=aP(i),B=c(k[65][3],a9,a8),o=1;else
var
m=0,o=0}else
var
m=0,o=0;break;case
8:var
R=s[2];if(R){var
S=R[2];if(S)if(S[2])var
m=0,o=0;else
var
a$=S[1],ba=R[1],bb=0,bc=[0,ab(i,function(a){return b([0,[0,c(z[4],a,0),a$],l])}),bb],be=[0,ab(i,function(a){return b([0,[0,c(z[4],a,0),ba],l])}),bc],bf=aP(i),B=c(k[65][21],bf,be),o=1;else
var
m=0,o=0}else
var
m=0,o=0;break;case
9:if(s[2])var
m=0,o=0;else
var
B=aP(i),o=1;break;case
11:var
T=s[2];if(T)if(T[2])var
m=0,o=0;else{var
v=aR(A,T[1]);if(typeof
v==="number")var
q=0;else
switch(v[0]){case
1:var
F=v[1];if(typeof
F==="number")if(16<=F){switch(F+cP|0){case
0:var
V=v[2];if(V){var
W=V[2];if(W){var
X=W[2];if(X)if(X[2])var
q=0,r=0,p=0;else{var
H=X[1],J=W[1],az=V[1];if(bd[1]){var
Y=a5(G,A,az);if(typeof
Y==="number")var
D=0;else
if(1===Y[0]){var
Z=Y[1];if(typeof
Z==="number"){if(23===Z)var
bg=0,bh=[0,ab(i,function(a){return b(l)}),bg],bi=[0,J,H,a(h[11],i)],aF=f(b7),bj=g===aF?b7[1]:d===aF?a(e[2],b7):b7,bk=a(h[23],[0,bj,bi]),bl=[0,a(j[aM],bk),bh],aG=a(k[65][22],bl),ar=1;else
if(24===Z)var
bm=0,bn=[0,ab(i,function(a){return b(l)}),bm],bo=[0,J,H,a(h[11],i)],aH=f(cg),bp=g===aH?cg[1]:d===aH?a(e[2],cg):cg,bq=a(h[23],[0,bp,bo]),br=[0,a(j[aM],bq),bn],aG=a(k[65][22],br),ar=1;else
var
D=0,ar=0;if(ar)var
aA=aG,D=1}else
var
D=0}else
var
D=0;if(!D)var
aA=b(l);var
aB=aA}else{var
_=a5(G,A,az);if(typeof
_==="number")var
E=0;else
if(1===_[0]){var
$=_[1];if(typeof
$==="number"){if(23===$)var
bs=b(l),aJ=f(at),bt=[0,J,H],bu=g===aJ?at[1]:d===aJ?a(e[2],at):at,bv=a(h[23],[0,bu,bt]),bw=c(z[11][1][7],bv,w),bx=c(j[4],kF,bw),aK=c(k[65][3],bx,bs),au=1;else
if(24===$)var
by=b(l),aL=f(as),bz=[0,J,H],bA=g===aL?as[1]:d===aL?a(e[2],as):as,bB=a(h[23],[0,bA,bz]),bC=c(z[11][1][7],bB,w),bD=c(j[4],kG,bC),aK=c(k[65][3],bD,by),au=1;else
var
E=0,au=0;if(au)var
aI=aK,E=1}else
var
E=0}else
var
E=0;if(!E)var
aI=b(l);var
aB=aI}var
C=aB,p=1}else
var
r=1,p=0}else
var
q=0,r=0,p=0}else
var
q=0,r=0,p=0;break;case
9:var
ad=v[2];if(ad){var
ae=ad[2];if(ae)if(ae[2])var
q=0,r=0,p=0;else
var
aQ=ae[1],af=ad[1],bN=x(af),bO=0,bP=[0,ab(i,function(a){var
d=aC(aQ),e=c1(aC(af),d);return b([0,[0,c(z[4],a,0),e],l])}),bO],bR=[0,af,aQ,bN,a(h[11],i)],aS=f(cr),bQ=0,bS=g===aS?cr[1]:d===aS?a(e[2],cr):cr,bT=[0,u([0,a(h[23],[0,bS,bR]),bQ]),bP],C=a(k[65][22],bT),p=1;else
var
q=0,r=0,p=0}else
var
q=0,r=0,p=0;break;case
10:var
ag=v[2];if(ag){var
ah=ag[2];if(ah)if(ah[2])var
q=0,r=0,p=0;else
var
aT=ah[1],aU=ag[1],bU=0,bV=[0,ab(i,function(a){var
d=aC(aT),e=cA(aC(aU),d);return b([0,[0,c(z[4],a,0),e],l])}),bU],bX=[0,aU,aT,a(h[11],i)],aV=f(cq),bW=0,bY=g===aV?cq[1]:d===aV?a(e[2],cq):cq,bZ=[0,u([0,a(h[23],[0,bY,bX]),bW]),bV],C=a(k[65][22],bZ),p=1;else
var
q=0,r=0,p=0}else
var
q=0,r=0,p=0;break;case
13:var
ai=v[2];if(ai)if(ai[2])var
r=1,p=0;else
var
aj=ai[1],b0=x(aj),b1=0,b2=[0,ab(i,function(a){return b([0,[0,c(z[4],a,0),aj],l])}),b1],b4=[0,aj,b0,a(h[11],i)],aW=f(cu),b3=0,b5=g===aW?cu[1]:d===aW?a(e[2],cu):cu,b6=[0,u([0,a(h[23],[0,b5,b4]),b3]),b2],C=a(k[65][22],b6),p=1;else
var
q=0,r=0,p=0;break;case
14:var
ak=v[2];if(ak){var
al=ak[2];if(al)if(al[2])var
q=0,r=0,p=0;else
var
L=al[1],M=ak[1],b8=x(M),b9=x(L),b_=0,b$=[0,ab(i,function(a){var
d=cA(aC(M),L),e=c1(cA(M,aC(L)),d);return b([0,[0,c(z[4],a,0),e],l])}),b_],cb=[0,M,L,b8,b9,a(h[11],i)],aX=f(ct),ca=0,cc=g===aX?ct[1]:d===aX?a(e[2],ct):ct,cd=[0,u([0,a(h[23],[0,cc,cb]),ca]),b$],C=a(k[65][22],cd),p=1;else
var
q=0,r=0,p=0}else
var
q=0,r=0,p=0;break;default:var
r=1,p=0}if(p)var
aE=C,r=2}else
var
r=1;else
var
r=1;switch(r){case
0:var
y=0;break;case
1:var
aa=v[2];if(aa){var
ac=aa[2];if(ac)if(ac[2])var
q=0,y=0;else{var
bE=ac[1],bF=aa[1];try{var
K=kA(F),bG=0,bH=[0,ab(i,function(a){return b(l)}),bG],bJ=[0,bF,bE,a(h[11],i)],aO=f(K),bI=0,bK=g===aO?K[1]:d===aO?a(e[2],K):K,bL=[0,u([0,a(h[23],[0,bK,bJ]),bI]),bH],bM=a(k[65][22],bL),aN=bM}catch(a){a=t(a);if(a!==I)throw a;var
aN=b(l)}var
aE=aN,y=1}else
var
q=0,y=0}else
var
q=0,y=0;break;default:var
y=1}if(y)var
U=aE,q=1;break;case
2:var
aY=v[2],am=v[1],ce=x(am),cf=0,ch=[0,ab(i,function(a){var
d=cA(am,aC(aY));return b([0,[0,c(z[4],a,0),d],l])}),cf],cj=[0,am,aY,ce,a(h[11],i)],aZ=f(cs),ci=0,ck=g===aZ?cs[1]:d===aZ?a(e[2],cs):cs,cl=[0,u([0,a(h[23],[0,ck,cj]),ci]),ch],U=a(k[65][22],cl),q=1;break;default:var
q=0}if(!q)var
U=b(l);var
B=U,o=1}else
var
m=0,o=0;break;case
12:var
an=s[2];if(an){var
ao=an[2];if(ao)if(ao[2])var
m=0,o=0;else
var
a0=ao[1],a1=an[1],cm=dC(i,function(d,a){var
e=n(h[35],a0,0,a1),f=[0,[0,c(z[4],a,0),e],l],g=n(h[35],a1,0,a0);return b([0,[0,c(z[4],d,0),g],f])}),cn=aP(i),B=c(k[65][3],cn,cm),o=1;else
var
m=0,o=0}else
var
m=0,o=0;break;case
0:case
1:case
2:case
3:case
4:var
aw=s[2];if(aw){var
ax=aw[2];if(ax)if(ax[2])var
m=0,o=0;else
var
ay=b(l),o=2;else
var
m=0,o=0}else
var
m=0,o=0;break;default:var
m=0,o=0}switch(o){case
0:var
av=0;break;case
1:var
ay=B,av=1;break;default:var
av=1}if(av)var
N=ay,m=1}else
var
m=0;else
var
m=0;break;case
2:var
ap=s[2],aq=s[1],co=a(a4,ap);if(c(kH[107],A,co))var
cp=x(aq),cw=0,cx=[0,ab(i,function(a){var
d=c1(aC(aq),ap);return b([0,[0,c(z[4],a,0),d],l])}),cw],cz=[0,aq,ap,cp,a(h[11],i)],a2=f(cv),cy=0,cB=g===a2?cv[1]:d===a2?a(e[2],cv):cv,cC=[0,u([0,a(h[23],[0,cB,cz]),cy]),cx],a3=a(k[65][22],cC);else
var
a3=b(l);var
N=a3,m=1;break;default:var
m=0}if(!m)var
N=b(l);return N}catch(c){c=t(c);if(c===a_)return b(l);if(a(aD[4],c))return b(l);throw c}};return c(q[73][1],q[54],v)}return c(k[65][3],ky,kc)}return b(a(q[68][3],m))}var
da=a(q[68][8],kE);function
kJ(b){var
i=a(q[68][2],b),m=a(q[68][4],b),n=a(q[68][5],b);function
u(a){return aa(m,n,a)}function
l(i){function
b(b){function
m(d){var
c=aR(b,i);return a(q[16],c)}function
n(b){if(typeof
b!=="number")switch(b[0]){case
1:var
p=b[1];if(typeof
p==="number"){var
r=p-27|0;if(!(2<r>>>0))switch(r){case
0:if(!b[2])return da;break;case
1:break;default:var
s=b[2];if(s)if(!s[2]){var
B=j[16],C=O(cZ),D=c(k[65][3],C,B);return c(k[65][3],D,da)}}}break;case
2:var
E=l(b[2]);return c(k[65][3],j[16],E)}try{var
w=u(i),x=j[16],y=function(c){var
b=f(co),j=[0,i,w],k=g===b?co[1]:d===b?a(e[2],co):co;return dx(c,a(h[23],[0,k,j]))},z=a(q[68][8],y),A=c(k[65][3],z,x),o=A}catch(b){b=t(b);if(b===a_)var
m=f(aB),v=g===m?aB[1]:d===m?a(e[2],aB):aB,n=a(j[108],v);else{if(!a(q[72][9],b))throw b;var
n=c(q[21],0,b)}var
o=n}return c(k[65][3],o,da)}var
o=a(q[72][10],m);return c(q[73][1],o,n)}return c(q[73][1],q[54],b)}return l(i)}var
kK=a(q[68][8],kJ);function
kL(e){a(dq[12],kM);if(cS[1]){var
b=cT[1],d=function(a){a[1][1]=a[2];return 0};c(p[17][11],d,b);cU[1]=0;a(v[2],bf);bg[1]=0;dp(0)}return kK}var
kN=a(q[16],0),dD=c(q[73][1],kN,kL);cH(426,[0,dD],"Omega_plugin__Coq_omega");a(kO[9],cF);function
a$(b){var
d=c(o[17],D[1][6],kP),e=a(D[5][4],d),f=a(D[6][4],b),g=c(D[13][1],[0,e],f),h=a(kQ[12],g);return a(kR[22],h)}function
cG(b){var
d=c(p[17][137],kS[33],b);function
e(b){if(cM(b,kT)){if(cM(b,kU)){if(cM(b,kV)){if(cM(b,kW)){var
d=c(m[17],kX,b),e=a(ac[3],d);return n(_[6],0,0,e)}return a$(kY)}return a$(kZ)}return a$(k0)}return a$(k1)}var
f=c(o[17],e,d),g=a(k[65][22],f),h=a(k[65][32],g);return c(k[65][3],h,dD)}var
k2=0,k4=[0,[0,k3,function(a){return cG(0)}],k2];dG(dE[8],cF,k5,0,0,k4);var
k6=0,k9=[0,[0,k8,function(a){return cG(k7)}],k6];function
k_(a,b){return cG(c(o[17],D[1][8],a))}var
ld=[0,[0,[0,lc,[0,lb,[1,[0,[5,a(la[16],k$[7])]],0]]],k_],k9];dG(dE[8],cF,le,0,0,ld);cH(434,[0,cF,a$,cG],"Omega_plugin__G_omega");return}
