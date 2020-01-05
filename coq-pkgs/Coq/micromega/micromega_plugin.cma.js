function(Ix){"use strict";var
ni="QMicromega",nF="*",ja="param.csdp",nA="__varmap",nE="diagonalize: not PSD",nc="csdp: error ",n2="monoid",n9="__p",i7="_vendor+v8.10+32bit/coq/plugins/micromega/mfourier.ml",nQ="__p%i",nR=",",jj='"',n8=" 1 ",nP="1\n",n7="__ff",nh="=",nz="find_pivot",nO="%s*%a",n1="Require Import ZArith Lia. Open Scope Z_scope.\n",i6='"\n',no="buggy certificate",nD="lia",ao="ZMicromega",nN=" * ",M="micromega",g2='command "',ny=")^2",nb="compare_num",jl="real_nonlinear_prover",ng=" [*] ",nM="__wit",n0=143,nf="Zero",jk="Lia",i$="; csdp ",nn="ERROR: no compatible proof",aP="",cr="RingMicromega",nL='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',jh="real nonlinear prover",jg="0",jf="> /dev/null",nx="%a * %a",na="__arith",eo="Reals",n6=", False\n",i_=438,aX=248,m_="<=",m$="QArith",je="cd ",nC=") * (",dk=" + ",A="Coq",nZ="psatz_R",ne=148,nY=".",nl="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",nm=" : ",nX="Bad logical fragment",e=246,nk="-%a",nB="(%a) * (%a)",n5="x%i",nK=118,ac="Tauto",jd="%a + %a",nJ=119,ji=" Cannot find witness",nI="csdp: Problem is infeasible",nW="Z",nw="the use of a specialized external tool called csdp. \n\n",av=" ",n4="e",an=103,g1=120,nV="EnvRing",m9="t",nd="PsatzZ",i9=".dat-s",nv="-",n3="Rdefinitions",nu="Timeout",nH="D",nT="linear prover",nU="Depth",nj="(%a)^2",nG=" Skipping what remains of this tactic: the complexity of the goal requires ",ns="psatz_Q",nt='" exited ',g3="_vendor+v8.10+32bit/coq/plugins/micromega/certificate.ml",g4="%s",nq="psatz_Z",jc=".out",nr="Rpow_def",np="nat",m8="%i",jb="sos",i8="pure_sos",aO="\n",nS=0.693147180559945286,aF="VarMap",k=250,E=Ix.jsoo_runtime,K=E.caml_check_bound,gY=E.caml_compare,_=E.caml_equal,aW=E.caml_fresh_oo_id,bk=E.caml_int_compare,m6=E.caml_int_of_string,cq=E.caml_lessthan,Iu=E.caml_list_of_js_array,cQ=E.caml_ml_string_length,gZ=E.caml_mul,c=E.caml_new_string,j=E.caml_obj_tag,aE=E.caml_register_global,m5=E.caml_string_equal,b0=E.caml_string_get,dj=E.caml_sys_remove,i5=E.caml_sys_system_command,q=E.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):E.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):E.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):E.caml_call_gen(a,[b,c,d])}function
F(a,b,c,d,e){return a.length==4?a(b,c,d,e):E.caml_call_gen(a,[b,c,d,e])}function
W(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):E.caml_call_gen(a,[b,c,d,e,f])}function
Z(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):E.caml_call_gen(a,[b,c,d,e,f,g])}function
g0(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):E.caml_call_gen(a,[b,c,d,e,f,g,h])}function
m7(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):E.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
Iw(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
Iv(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
r=E.caml_get_global_data(),cx=[0,0,0],hD=[0,0],hM=[0,0],bp=[0,1],kC=c(" \t\n\r"),kD=c(",;"),kE=c("()[]{}"),kF=c("\\!@#$%^&*-+|\\<=>/?~.:"),kG=c("'abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"),kH=c("0123456789"),k1=[0,1],h3=c("axtol=1.0e-8\natytol=1.0e-8\nobjtol=1.0e-8\npinftol=1.0e8\ndinftol=1.0e8\nmaxiter=100\nminstepfrac=0.9\nmaxstepfrac=0.97\nminstepp=1.0e-8\nminstepd=1.0e-8\nusexzgap=1\ntweakgap=0\naffine=0\nprintlevel=1\n"),mi=Iu([[0,c(A),[0,c("Lists"),[0,c("List"),0]]],[0,c(A),[0,c(M),[0,c(ao),0]]],[0,c(A),[0,c(M),[0,c(ac),0]]],[0,c(A),[0,c(M),[0,c("DeclConstant"),0]]],[0,c(A),[0,c(M),[0,c(cr),0]]],[0,c(A),[0,c(M),[0,c(nV),0]]],[0,c(A),[0,c(M),[0,c(ao),0]]],[0,c(A),[0,c(M),[0,c("RMicromega"),0]]],[0,c(A),[0,c(M),[0,c(ac),0]]],[0,c(A),[0,c(M),[0,c(cr),0]]],[0,c(A),[0,c(M),[0,c(nV),0]]],[0,c(A),[0,c(m$),[0,c("QArith_base"),0]]],[0,c(A),[0,c(eo),[0,c(n3),0]]],[0,c(A),[0,c(eo),[0,c(nr),0]]],[0,c("LRing_normalise"),0]]),aB=c("micromega_plugin"),f=r.Stdlib,R=r.Unix,m=r.Stdlib__printf,eL=r.Stdlib__marshal,eG=r.Stdlib__printexc,g=r.Stdlib__list,n=r.Big_int,a0=r.Assert_failure,d=r.Num,dK=r.Ratio,hy=r.Stdlib__set,cy=r.Stdlib__map,bn=r.Stdlib__hashtbl,eX=r.Failure,bg=r.Stdlib__string,kJ=r.End_of_file,bM=r.Stdlib__filename,O=r.Not_found,lE=r.Invalid_argument,i=r.CamlinternalLazy,df=r.CErrors,o=r.Util,a4=r.Option,iG=r.CList,b_=r.Names,l=r.EConstr,a$=r.Tacmach,Q=r.Tacticals,bA=r.Pp,di=r.Proofview,mM=r.CAst,aM=r.Tactics,ml=r.Stdlib__array,mC=r.Context,mk=r.Retyping,mu=r.Evd,d9=r.Coqlib,d8=r.Goptions,aC=r.Ltac_plugin__Tacinterp,aN=r.Ltac_plugin__Tacentries,gX=r.Stdarg,ad=r.Genarg,aD=r.Ltac_plugin__Tacarg,sX=r.Sys_error,z5=r.CMap,Gq=r.Coq_config,Gu=r.Envars,Fn=r.Sorts,E8=r.Redexpr,ET=r.UnivProblem,EU=r.Univ,EO=r.Reductionops,Ey=r.Typeclasses,BV=r.UnivGen,Gp=r.System,Hc=r.Mltop;aE(976,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"Micromega_plugin");var
pC=[0,0],pP=[0,0,0],pQ=[0,[0,0],0],pV=[0,[0,0],0],pW=[0,0,0],pT=[0,[0,0],0],pU=[0,0,0],pI=[0,[0,0],0],pJ=[0,0,0],pG=[0,[0,0],0],pH=[0,0,0],pA=[0,1],pB=[0,0],pz=[0,0],px=[0,0,1,0],pr=[0,0],pq=[0,0],pp=[0,0],ph=[0,[0,0]],pi=[0,[0,0]],pj=[0,[0,0]],pk=[0,[0,0]],pd=[0,[0,0]],pe=[0,[0,0]],pf=[0,[0,0]],pg=[0,[0,0]],oW=[0,[0,0],0],oV=[0,0,0],oR=[0,1],oS=[0,2],oT=[0,3],oM=[0,0],oO=[0,0],oN=[0,1],oQ=[0,3],oP=[0,0],oL=[0,0,0],or=[0,[1,0]],os=[0,0,[0,0]],ot=[0,[0,0],0],ou=[0,0],ov=[0,[1,0]],ow=[0,[1,0]],ox=[0,0],oy=[0,[1,0]],oz=[0,[1,0]],oA=[0,[1,0]],oB=[0,0],oC=[0,[1,0]],oD=[0,0,0],oE=[0,0,0],oF=[0,0],oG=[0,0,0],oH=[0,0],ol=[0,0],oj=[0,0],oi=[1,0],oh=[0,0],oa=[1,0],ob=[1,0],oc=[0,0],oe=[0,0],od=[0,0],o1=[0,0],pb=[0,0],pv=[0,0],pE=[0,[0,0],0],pF=[0,0,0],pK=[0,0,0],pL=[0,0,0],pM=[0,[0,0],0],pN=[0,0,0],pR=[0,[0,0],0],pS=[0,0,0],pX=[0,0,0],pY=[0,0,0],rw=[0,[11,c(g2),[2,0,[11,c(nt),[4,3,0,0,0]]]],c('command "%s" exited %i')],rv=[0,[11,c(g2),[2,0,[11,c(nt),[2,0,0]]]],c('command "%s" exited %s')],rx=[0,[11,c(g2),[2,0,[11,c('" killed '),[4,3,0,0,0]]]],c('command "%s" killed %i')],ry=[0,[11,c(g2),[2,0,[11,c('" stopped '),[4,3,0,0,0]]]],c('command "%s" stopped %i')],rj=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/mutils.ml"),262,7],rf=[0,0,0],re=[0,0,0],rc=[0,0,0],qA=[0,[4,3,0,0,[12,32,0]],c("%i ")],rH=[0,[15,[11,c(dk),[15,0]]],c(jd)],rI=c(jg),rP=c(nb),rQ=c(nb),rT=[0,0],rU=[0,0],r2=[0,0],r3=[0,0],r$=[0,1],r8=[0,0],r7=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/vect.ml"),285,16],r4=[0,[0,0,[0,0]],0],r0=[0,0],rZ=[0,0],rY=[0,0],rV=[0,1],rW=[0,1],rS=[0,1],rR=[0,0],rO=[0,0],rN=[0,0],rL=[0,[15,[12,32,0]],c("%a ")],rM=[0,[11,c("(+ "),[15,[12,41,0]]],c("(+ %a)")],rJ=[0,[12,g1,[4,3,0,0,0]],c(n5)],rD=[0,0],rE=[0,[2,0,0],c(g4)],rG=[0,[11,c("(- "),[15,[12,41,0]]],c("(- %a)")],rF=[0,[11,c("(* "),[2,0,[12,32,[15,[12,41,0]]]]],c("(* %s %a)")],rz=[0,0],rA=[0,[2,0,0],c(g4)],rC=[0,[12,45,[15,0]],c(nk)],rB=[0,[2,0,[12,42,[15,0]]],c(nO)],sa=c(jg),sb=[0,[12,nK,[2,0,0]],c("v%s")],sc=[0,[11,c("- ("),[15,[12,41,0]]],c("- (%a)")],sd=[0,[12,40,[15,[11,c(")+("),[15,[12,41,0]]]]],c("(%a)+(%a)")],se=[0,[12,40,[15,[11,c(")-("),[15,[12,41,0]]]]],c("(%a)-(%a)")],sf=[0,[12,40,[15,[11,c(")*("),[15,[12,41,0]]]]],c("(%a)*(%a)")],sg=[0,[12,40,[15,[11,c(")^("),[4,3,0,0,[12,41,0]]]]],c("(%a)^(%i)")],sh=[0,[11,c("Aeq("),[4,3,0,0,[12,41,0]]],c("Aeq(%i)")],si=[0,[11,c("Ale("),[4,3,0,0,[12,41,0]]],c("Ale(%i)")],sj=[0,[11,c("Alt("),[4,3,0,0,[12,41,0]]],c("Alt(%i)")],sk=[0,[11,c("eq("),[2,0,[12,41,0]]],c("eq(%s)")],sl=[0,[11,c("le("),[2,0,[12,41,0]]],c("le(%s)")],sm=[0,[11,c("lt("),[2,0,[12,41,0]]],c("lt(%s)")],sn=[0,[12,40,[15,[11,c(ny),0]]],c(nj)],so=[0,[11,c(n2),0],c(n2)],sp=[0,[15,[11,c(nN),[15,0]]],c(nx)],sq=[0,[15,[11,c(dk),[15,0]]],c(jd)],sr=[0,[15,[11,c(nN),[15,0]]],c(nx)],sx=c("end_itlist"),sz=c("tryfind"),sB=c("choose: completely undefined function"),sZ=c(aO),sY=c("strings_of_file: can't open "),sU=c(" expected"),sC=c("apply"),sy=c(aP),st=[0,2],su=[0,10],sD=c("Micromega_plugin.Sos_lib.Noparse"),s1=c("Micromega_plugin.Sos_lib.TooDeep"),tu=[0,1],v9=[0,0,0],wG=[0,0],wE=[0,0],wF=[0,0],wC=[0,1],wz=[0,1],wA=[0,2],wy=[0,0,0],ww=[0,1],wx=[0,-1],wB=[0,0,0],wD=[0,0,0],wu=c(nI),wv=c(nc),wl=c(i9),wm=c(jb),wn=c(jc),wo=c(ja),wp=c(aP),wt=c(jf),wq=c(av),wr=c(i$),ws=c(je),wf=c(aP),wg=c(aO),wh=c(nP),wi=c(aO),wj=c(i6),wk=c(jj),wc=c(aO),wd=c(av),we=c(av),wa=c(n8),wb=c(aP),v_=c(aO),v$=c(av),v3=[0,0],v1=[0,0],v2=[0,0],v0=[0,1],vW=[0,1],vX=[0,2],vV=[0,1],vY=[0,0,0,0],vZ=[0,0,0,0],v4=[0,-1],v5=[0,0,0,0],vS=[0,0],vQ=[0,0],vN=c(nI),vO=c(nc),vE=c(i9),vF=c(jb),vG=c(jc),vH=c(ja),vI=c(aP),vM=c(jf),vJ=c(av),vK=c(i$),vL=c(je),vx=c(aP),vy=c(aO),vz=c(av),vA=c(aO),vB=c(aO),vC=c(i6),vD=c(jj),vt=c(aO),vu=c(av),vv=c(av),vw=c(av),vr=c(av),vs=c(aP),vq=[0,0,0,0],vl=[0,0],vm=[0,1],vk=[0,0],vn=[0,0],vo=[0,1],vp=[0,1],vg=[0,0],vh=c(nE),vi=[0,0],vj=c(nE),vf=c("diagonalize: non-square matrix"),ve=[0,0,0],vc=[0,0],vd=[0,0],vb=[0,-1],va=c("choose_variable"),u$=[0,0],u_=[0,0],u9=[0,0],u7=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/sos.ml"),533,12],u6=[0,1],u5=c("linear_program: An error occurred in the SDP solver"),u1=[0,1],u2=[0,1],u3=[0,0],u4=[0,0],uS=c(i9),uT=c(jb),uU=c(jc),uV=c(ja),uW=c(aP),u0=c(jf),uX=c(av),uY=c(i$),uZ=c(je),ux=[0,0,0],us=c("mkparser: unparsed input"),uo=[0,10],uc=c("+"),ud=c(nv),tR=c(aP),tS=c(aO),tT=c(nP),tU=c(aO),tV=c(i6),tW=c(jj),tO=c(aO),tP=c(av),tQ=c(av),tM=c(n8),tN=c(aP),tK=c(aO),tL=c(av),tD=[0,0],tE=c(" - "),tF=c(dk),tB=c("<<0>>"),tC=c(aP),tG=c(">>"),tH=c(dk),tJ=c(nv),tI=c("<<"),ty=[0,1],tz=c(nF),tw=c("1"),tx=c(nF),tv=c("^"),tt=[0,0],ts=[0,0],tr=[0,0],tq=[0,0],tp=[0,1],tl=[0,0],tk=c("matrix_add: incompatible dimensions"),tj=[0,0],th=[0,0],tg=[0,0],tf=[0,0],te=[0,0],s3=[0,10],s4=[0,1],s5=[0,10],s6=[0,1],s7=[0,10],s8=[0,0],s9=c("0.0"),s_=[0,1],s$=c(aP),td=c(n4),ta=[0,0],tb=c("-0."),tc=c("0."),s2=c("Micromega_plugin.Sos.Sanity"),t7=c(nY),ug=c("E"),ui=c(n4),ut=c("}"),uu=c(nR),uv=c("{"),uy=c(nh),uz=c("xVec"),uC=c(aO),uE=c(av),uK=c(av),vT=[0,-1],yL=c("pivot: equality as second argument"),yM=[0,1],yN=[0,-1],yK=[0,1,0,0],yG=[0,0],yH=[0,-1],yI=c("cutting_plane ignore strict constraints"),yE=[0,0],yz=[0,[15,[12,10,0]],c("%a\n")],yx=[0,[15,[12,32,[2,0,[11,c(" 0 by "),[15,[12,10,0]]]]]],c("%a %s 0 by %a\n")],xW=[0,[11,c(nf),0],c(nf)],xX=[0,[12,40,[15,[12,41,[12,64,[2,0,0]]]]],c("(%a)@%s")],xY=[0,[11,c("Hyp "),[4,3,0,0,0]],c("Hyp %i")],xZ=[0,[11,c("Def "),[4,3,0,0,0]],c("Def %i")],x0=[0,[11,c("Cst "),[2,0,0]],c("Cst %s")],x1=[0,[12,40,[15,[11,c(ny),0]]],c(nj)],x2=[0,[12,40,[15,[11,c(nC),[15,[12,41,0]]]]],c(nB)],x3=[0,[12,40,[15,[11,c(")/"),[2,0,0]]]],c("(%a)/%s")],x4=[0,[12,40,[15,[11,c(nC),[15,[12,41,0]]]]],c(nB)],x5=[0,[15,[11,c(dk),[15,0]]],c(jd)],x6=[0,[12,91,[15,[12,93,0]]],c("[%a]")],x7=[0,[12,46,0],c(nY)],x8=[0,[4,3,0,0,[11,c(":= "),[15,[11,c(" ; "),[15,0]]]]],c("%i:= %a ; %a")],x9=c(";"),x_=[0,[4,3,0,0,[12,123,[15,[11,c(m_),[15,[11,c(m_),[15,[12,125,[15,0]]]]]]]]],c("%i{%a<=%a<=%a}%a")],ya=[0,1],yb=[0,1],x$=[0,0],yc=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/polynomial.ml"),695,14],yd=[0,1],yg=[0,1],yf=[0,1],ye=[0,1],yn=[0,0],yo=c("eval_prf_rule : negative constant"),yp=[0,[11,c("MulC("),[15,[12,44,[15,[11,c(") invalid 2d arg "),[15,[12,32,[2,0,0]]]]]]]],c("MulC(%a,%a) invalid 2d arg %a %s")],yq=c("eval_prf_rule : not an equality"),yt=c("Proof is not finished"),yu=[0,[11,c("Last inference "),[15,[12,32,[2,0,[12,10,0]]]]],c("Last inference %a %s\n")],yv=c("Not implemented"),yr=[0,0],yl=c("Cuts should already be compiled"),yi=[0,[4,3,0,0,[12,44,[2,0,0]]],c("%i,%s")],yj=c(aP),yk=[0,[11,c("id_of_hyp "),[4,3,0,0,[12,32,[2,0,0]]]],c("id_of_hyp %i %s")],xT=[0,[11,c("(H"),[4,3,0,0,[11,c(nm),[15,[12,32,[2,0,[11,c(" 0)\n"),0]]]]]]],c("(H%i : %a %s 0)\n")],xR=[0,[11,c("(x"),[4,3,0,0,[11,c(nm),[2,0,[11,c(") "),0]]]]],c("(x%i : %s) ")],xS=[0,[11,c("forall "),[15,[12,10,0]]],c("forall %a\n")],xU=[0,[11,c(n6),0],c(n6)],xO=[0,0],xL=[0,[12,nK,[4,3,0,0,0]],c("v%i")],xJ=[0,1],xK=[0,0],xI=[0,0],xH=[0,1],xF=[0,1],xD=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c("%a %s %s")],xz=c(nh),xA=c(">="),xB=c(">"),xw=[0,0],xx=[0,0],xu=[0,0],xt=[0,1],xr=[0,0],xq=[0,[15,[11,c(dk),0]],c("%a + ")],xm=[0,0],xn=[0,[2,0,0],c(g4)],xp=[0,[12,45,[15,0]],c(nk)],xo=[0,[2,0,[12,42,[15,0]]],c(nO)],wM=[0,[15,[12,42,[15,0]]],c("%a*%a")],wK=[0,[12,g1,[4,3,0,0,0]],c(n5)],wL=[0,[12,g1,[4,3,0,0,[12,94,[4,3,0,0,0]]]],c("x%i^%i")],xy=c("Micromega_plugin.Polynomial.Strict"),yA=c("Micromega_plugin.Polynomial.WithProof.InvalidProof"),yU=[0,0],yV=c("find_pivot_column"),zu=[0,1],zv=[0,0],zr=[0,1],zs=[0,1],zt=[0,1],zm=[0,0],zn=[0,2],zo=[0,1],zp=[0,1],zq=[0,1],zw=[0,1],zl=[0,0],zk=[0,0],zj=[0,1],zg=[0,1],zf=[0,-1],za=[0,0],zb=c("push_real"),zc=[0,-1],zd=[0,0],ze=[0,1],y$=[0,0],y_=c(nz),y7=[0,0],y6=c("pivot"),y3=[0,0],y4=[0,0],y5=[0,1],yY=[0,0],yZ=c("Cannot solve column"),y0=[0,-1],y1=[0,0],y2=[0,-1],yX=c(nz),yW=[0,0],yS=[0,0],yQ=[0,[11,c("Cannot restrict "),[4,3,0,0,0]],c("Cannot restrict %i")],zh=c("Micromega_plugin.Simplex.FoundVar"),zD=[0,0,0],zB=[0,0,0],zz=[0,0,[0,5,0]],zC=[0,1,[0,4,[0,5,0]]],zA=[0,1,[0,6,[0,5,0]]],zx=c("Micromega_plugin.Persistent_cache.PHashtable(Key).InvalidTableFormat"),zy=c("Micromega_plugin.Persistent_cache.PHashtable(Key).UnboundTable"),zJ=[0,1],zE=[0,[12,91,[2,0,0]],c("[%s")],zI=c("]-oo"),zF=c(nR),zG=[0,[2,0,[12,93,0]],c("%s]")],zH=c("+oo["),zL=[0,[12,72,[4,3,0,0,0]],c("H%i")],zM=[0,[11,c("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("E(%i,%a,%a)")],zN=[0,[11,c("A("),[15,[12,44,[15,[12,41,0]]]]],c("A(%a,%a)")],Az=[0,1],AA=[0,[0,0,0]],Aw=c("merge_proof : pivot is not possible"),Ax=[0,1],Ay=[0,1],Av=[0,0],Ap=[0,1],Aq=[0,1],Ar=[0,1],As=[0,1],At=[0,1],Au=[0,1],Al=[0,0],Am=[0,-1],An=[0,[11,c("optimise Exception : "),[2,0,0]],c("optimise Exception : %s")],Ab=[0,0],Ac=[0,0],Ad=[0,0],Ae=[0,0],Af=[0,0],Ag=[0,0],Ah=[0,0],Ai=[0,0],z_=[0,[11,c("bound_of_variable: eval_vecr "),[15,[11,c(" = "),[2,0,[12,44,[15,[12,10,0]]]]]]],c("bound_of_variable: eval_vecr %a = %s,%a\n")],z$=[0,[11,c("current interval:  "),[15,[12,10,0]]],c("current interval:  %a\n")],Aa=c("bound_of_variable: impossible"),z9=[0,0,0],z7=[0,0,0],z8=c("SystemContradiction"),z6=[0,0],z3=[0,0],z2=[0,0,0,0],zW=[0,0],zX=[0,0],z0=[0,c(i7),200,4],zY=[0,1],zZ=[0,1],zU=[0,c(i7),151,6],zT=[0,0,0],zS=[0,0],zP=[0,c(i7),95,4],zK=c("Micromega_plugin.Mfourier.SystemContradiction"),zR=c("Micromega_plugin.Mfourier.TimeOut"),A1=c("scale term: not implemented"),A2=[0,0],A6=[0,0],Bv=c("nia"),Bu=c(nD),Bm=c(".v"),Bn=[0,[11,c(n1),0],c(n1)],Bo=c(nW),Bp=[0,[11,c("Goal "),[15,[11,c(".\n"),0]]],c("Goal %a.\n")],Br=[0,[11,c("Proof.\n intros. "),[2,0,[11,c(".\nQed.\n"),0]]],c("Proof.\n intros. %s.\nQed.\n")],Bq=[0,[11,c("Proof.\n intros. Fail "),[2,0,[11,c(".\nAbort.\n"),0]]],c("Proof.\n intros. Fail %s.\nAbort.\n")],Bh=[0,c(g3),922,4],Bi=[0,0],Bj=[0,1],Bk=[0,c(g3),951,4],Bd=[0,1],Be=[0,0,0],Bf=c("Interval without proof"),Ba=[0,1],Bb=[0,-1],A7=[0,0],A8=[0,1],A5=[0,0],A3=[0,1],A4=[0,0],A0=c("P"),AZ=[0,1],AW=[0,1],AX=[0,-1],AS=[0,1],AT=[0,c(g3),339,11],AU=c("check_sat : Unexpected operator"),AV=[0,0],AQ=[0,c(g3),302,14],AN=[0,0],AO=c("dual_raw_certificate: empty_certificate"),AK=[0,1],AJ=[0,0],AD=[0,0],AG=[0,[0,0],0],AH=[0,0,0],AR=c("Micromega_plugin.Certificate.FoundProof"),FF=[0,[12,68,0],c(nH)],FG=[0,[11,c("R["),[15,[12,44,[15,[12,93,0]]]]],c("R[%a,%a]")],FH=[0,[11,c("C["),[15,[12,44,[15,[12,93,0]]]]],c("C[%a,%a]")],FI=c("]"),FJ=c("["),FK=[0,[11,c("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],c("EP[%a,%a,%a]")],FV=c("abstract_wrt_formula"),Hb=c("Not ground"),G3=c(jl),G2=c(jl),G1=c(jl),GV=c(jh),GU=c(jh),GT=c(jh),Gy=c(no),Gx=c(no),Gr=c("csdpcert"),Gs=c(M),Gt=c("plugins"),Gk=c(na),Gl=[0,0],Gj=c(ji),Gd=c(nX),Ge=c(nu),Gf=c(nl),Gg=c(nL),Gh=c(nw),Gi=c(nG),F9=c(nM),F_=c(m9),F$=[0,[0,c(A),[0,c(M),[0,c(aF),0]]],[0,[0,c(aF),0],0]],Ga=c(aF),Gb=c(nA),Gc=c(n7),F4=c(ji),F5=c(na),F6=[0,0],F7=c(ji),FY=c(nX),FZ=c(nu),F0=c(nl),F1=c(nL),F2=c(nw),F3=c(nG),FT=[0,[11,c(nn),0],c(nn)],FU=c("Cannot find compatible proof"),FR=c("bad old index"),FS=c("proof compaction error"),FQ=[0,0],FN=c(nM),FO=c(nA),FP=c(n7),FE=[0,[15,[12,47,[15,0]]],c("%a/%a")],FB=c(m9),FC=[0,[0,c(A),[0,c(M),[0,c(aF),0]]],[0,[0,c(aF),0],0]],FD=c(aF),Fy=c("Empty"),Fz=[0,[0,c(A),[0,c(M),[0,c(aF),0]]],[0,[0,c(aF),0],0]],FA=c(aF),Fv=c("Elt"),Fw=[0,[0,c(A),[0,c(M),[0,c(aF),0]]],[0,[0,c(aF),0],0]],Fx=c(aF),Fs=c("Branch"),Ft=[0,[0,c(A),[0,c(M),[0,c(aF),0]]],[0,[0,c(aF),0],0]],Fu=c(aF),E_=[0,0],Fr=[0,[11,c(n9),[4,3,0,0,0]],c(nQ)],Fq=[0,[11,c(n9),[4,3,0,0,0]],c(nQ)],Fp=[0,[11,c("__x"),[4,3,0,0,0]],c("__x%i")],Fo=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/coq_micromega.ml"),1222,11],Fh=c("error : parse_arith(2)"),Fd=[0,0,0],EV=c("get_rank"),ES=[1,c("Oups")],EH=[0,[12,48,0],c(jg)],EI=[0,[11,c("(In "),[15,[12,41,[12,37,[11,c(np),0]]]]],c("(In %a)%%nat")],EJ=[0,[12,40,[15,[11,c("^2)"),0]]],c("(%a^2)")],EK=[0,[11,c("( "),[15,[11,c(ng),[15,[12,41,0]]]]],c("( %a [*] %a)")],EL=[0,[12,40,[15,[11,c(ng),[15,[12,41,0]]]]],c("(%a [*] %a)")],EM=[0,[12,40,[15,[11,c(" [+] "),[15,[12,41,0]]]]],c("(%a [+] %a)")],EN=[0,[12,40,[15,[12,41,[12,37,[11,c("positive"),0]]]]],c("(%a)%%positive")],EE=[0,[11,c("Pc "),[15,0]],c("Pc %a")],EF=[0,[11,c("Pinj("),[15,[12,44,[15,[12,41,0]]]]],c("Pinj(%a,%a)")],EG=[0,[11,c("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("PX(%a,%a,%a)")],EB=[0,[15,[11,c(" ,"),[15,0]]],c("%a ,%a")],EC=[0,[15,0],c("%a")],ED=[0,[2,0,[15,[2,0,0]]],c("%s%a%s")],Ez=[0,[2,0,0],c(g4)],Ex=[0,[4,3,0,0,0],c(m8)],Ew=[0,[4,3,0,0,0],c(m8)],Er=c("Formula"),Es=[0,[0,c(A),[0,c(M),[0,c(cr),0]]],[0,[0,c(cr),0],0]],Et=c(cr),Eo=c("Build_Formula"),Ep=[0,[0,c(A),[0,c(M),[0,c(cr),0]]],[0,[0,c(cr),0],0]],Eq=c(cr),El=c("QWitness"),Em=[0,[0,c(A),[0,c(M),[0,c(ni),0]]],0],En=c(ni),Ei=c("BFormula"),Ej=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],Ek=c(ao),Ee=c("I"),Ef=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],Eg=c(ao),Ea=c("X"),Eb=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],Ec=c(ao),D8=c("A"),D9=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],D_=c(ao),D4=c("N"),D5=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],D6=c(ao),D0=c(nH),D1=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],D2=c(ao),DW=c("Cj"),DX=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],DY=c(ao),DS=c("FF"),DT=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],DU=c(ao),DO=c("TT"),DP=[0,[0,c(A),[0,c(M),[0,c(ac),0]]],[0,[0,c(ac),0],0]],DQ=c(ao),DN=c("DeclaredConstant"),DM=c(nd),DL=c("PsatzC"),DK=c("PsatzAdd"),DJ=c("PsatzMulC"),DI=c("PsatzMulE"),DH=c("PsatzSquare"),DG=c("PsatzIn"),DF=c("OpGt"),DE=c("OpGe"),DD=c("OpLt"),DC=c("OpLe"),DB=c("OpNEq"),DA=c("OpEq"),Dz=c("Pinj"),Dy=c("Pc"),Dx=c("PX"),Dw=c("PEpow"),Dv=c("PEsub"),Du=c("PEmul"),Dt=c("PEopp"),Ds=c("PEadd"),Dr=c("PEc"),Dq=c("PEX"),Dp=c("Q2R"),Do=c("IZR"),Dn=c("powerRZ"),Dm=c("pow"),Dl=c("Rinv"),Dk=c("Rmult"),Dj=c("Ropp"),Di=c("Rminus"),Dh=c("Rplus"),Df=c("Rlt"),Dd=c("Rle"),Db=c("Rge"),C$=c("Rgt"),C_=c("Qpower"),C9=c("Qmult"),C8=c("Qopp"),C7=c("Qminus"),C6=c("Qplus"),C4=c("Qeq"),C2=c("Qlt"),C0=c("Qle"),CZ=c("Z.pow"),CY=c("Z.mul"),CX=c("Z.opp"),CW=c("Z.sub"),CV=c("Z.add"),CU=c("eq"),CS=c("Z.lt"),CQ=c("Z.le"),CO=c("Z.ge"),CM=c("Z.gt"),CL=c("EnumProof"),CK=c("CutProof"),CJ=c("RatProof"),CI=c("DoneProof"),CH=c("ZArithProof"),CG=c("R1"),CF=c("R0"),CE=c("COpp"),CD=c("CInv"),CC=c("CPow"),CB=c("CMult"),CA=c("CMinus"),Cz=c("CPlus"),Cy=c("CZ"),Cx=c("CQ"),Cw=c("C1"),Cv=c("C0"),Cu=c("Rcst"),Ct=c("Qmake"),Cs=c("R"),Cr=c("Q"),Cq=c("Zneg"),Cp=c("Zpos"),Co=c("Z0"),Cn=c(nW),Cm=c("xI"),Cl=c("xO"),Ck=c("xH"),Cj=c("Npos"),Ci=c("N0"),Ch=c("inr"),Cg=c("inl"),Cf=c("tt"),Ce=c("None"),Cd=c("unit"),Cc=c(np),Cb=c("S"),Ca=c("O"),B$=c("list"),B_=c("nil"),B9=c("cons"),B8=c("False"),B7=c("True"),B6=c("iff"),B5=c("not"),B4=c("or"),B3=c("and"),Bw=c(aP),By=[0,c(jk),[0,c("Enum"),0]],Bz=c("Lia Enum"),BC=[0,c("Simplex"),0],BD=c("Use the Simplex instead of Fourier elimination"),BG=[0,c("Dump"),[0,c("Arith"),0]],BH=c("Generate Coq goals in file from calls to 'lia' 'nia'"),BJ=[0,c("Lra"),[0,c(nU),0]],BL=[0,c(jk),[0,c(nU),0]],BN=[0,c(A),[0,c("Logic"),[0,c("Decidable"),0]]],BS=[0,[0,c(A),[0,c("Numbers"),[0,c("BinNums"),0]]],0],BT=[0,[0,c(A),[0,c(eo),[0,c(n3),0]]],[0,[0,c(A),[0,c(eo),[0,c(nr),0]]],[0,[0,c(A),[0,c(eo),[0,c("Raxioms"),0]]],[0,[0,c(A),[0,c(m$),[0,c("Qreals"),0]]],0]]]],BU=[0,[0,c(A),[0,c("ZArith"),[0,c("BinInt"),0]]],0],BX=c(ao),BY=c(ao),BZ=c(ao),B0=c(ao),B1=c(ao),B2=c(ao),Eu=c("Micromega_plugin.Coq_micromega.M.ParseError"),FW=c("Micromega_plugin.Coq_micromega.CsdpNotFound"),Gn=c(".csdp.cache"),Go=c("csdp"),GB=c(".lia.cache"),GE=c(".nia.cache"),GH=c(".nra.cache"),GL=c(nT),GP=c(nT),GS=c("nra"),GW=c(nD),GY=c("nlia"),G4=c(i8),G7=c(i8),G_=c(i8),He=[0,c("myred"),0],Hg=c("RED"),Hj=c("is_ground"),Hl=c("ISGROUND"),Ho=c(nq),Hs=c(nq),Hu=c(nd),Hx=c("xlia"),Hz=c(jk),HC=c("xnlia"),HE=c("Nia"),HH=c("xnra"),HJ=c("NRA"),HM=c("xnqa"),HO=c("NQA"),HR=c("sos_Z"),HT=c("Sos_Z"),HW=c("sos_Q"),HY=c("Sos_Q"),H1=c("sos_R"),H3=c("Sos_R"),H6=c("lra_Q"),H8=c("LRA_Q"),H$=c("lra_R"),Ib=c("LRA_R"),Ie=c(nZ),Ii=c(nZ),Ik=c("PsatzR"),In=c(ns),Ir=c(ns),It=c("PsatzQ");function
bC(a){return 0===a?1:0}function
b1(a){return a[1]}function
n_(a){return a[2]}function
S(a,b){if(a){var
c=a[1];return[0,c,S(a[2],b)]}return b}function
jm(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
g5(b,a){return b?[0,g5(b[1],a)]:a}function
jn(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
bl(c,a){if(a){var
d=a[1],e=bl(c,a[2]);return[0,b(c,d),e]}return 0}function
ep(d,c,b){if(b){var
e=b[1];return a(d,e,ep(d,c,b[2]))}return c}var
n$=[0];function
bD(a){return typeof
a==="number"?oa:0===a[0]?[1,bD(a[1])]:[0,a[1]]}function
cR(b,a){if(typeof
b==="number")return typeof
a==="number"?ob:0===a[0]?[1,bD(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,bD(c)]:0===a[0]?[1,dl(c,a[1])]:[0,cR(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,cR(d,a[1])]:[1,cR(d,a[1])]}}function
dl(b,a){if(typeof
b==="number")return typeof
a==="number"?oc:0===a[0]?[0,bD(a[1])]:[1,bD(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,bD(c)]:0===a[0]?[0,dl(c,a[1])]:[1,dl(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,bD(d)]:0===a[0]?[1,dl(d,a[1])]:[0,cR(d,a[1])]}}function
dm(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,dm(a[1])]}function
dn(a){return typeof
a==="number"?0===a?od:1:[0,[0,a[1]]]}function
dp(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
jo(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,dm(a[1])]]}function
cS(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?dp(cS(c,a[1])):dn(cS(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,dm(d)]:0===a[0]?dn(dq(d,a[1])):dp(cS(d,a[1]))}}function
dq(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,dm(c)]:0===a[0]?dn(dq(c,a[1])):dp(cS(c,a[1]))}var
d=b[1];return typeof
a==="number"?jo(d):0===a[0]?dp(dq(d,a[1])):dn(dq(d,a[1]))}}function
g6(c,b){var
a=cS(c,b);return typeof
a==="number"?0:a[1]}function
g7(b,a){return typeof
b==="number"?a:0===b[0]?cR(a,[1,g7(b[1],a)]):[1,g7(b[1],a)]}function
eq(a,h,g){var
d=h,c=g;for(;;)if(typeof
c==="number")return b(a,d);else{if(0===c[0]){var
e=c[1];return b(a,eq(a,eq(a,d,e),e))}var
f=c[1],d=eq(a,d,f),c=f;continue}}function
dr(a){return typeof
a==="number"?oe:0===a[0]?[0,dr(a[1])]:[0,dr(a[1])]}function
jp(h,g,f){var
c=h,b=g,a=f;for(;;)if(typeof
b==="number")return typeof
a==="number"?c:1;else{if(0===b[0]){var
d=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
b=d,a=a[1];continue}var
c=2,b=d,a=a[1];continue}}var
e=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
c=1,b=e,a=a[1];continue}var
b=e,a=a[1];continue}}}var
of=0;function
jq(a,b){return jp(of,a,b)}function
g8(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(jq(e,f)){case
0:return b;case
1:var
c=d,a=b,b=g6(f,e);continue;default:var
c=d,b=g6(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,g8(d,g,a[1])]}}}return 0}}function
og(b,a){var
c=dr(a);return g8(g5(dr(b),c),b,a)}function
jr(a){return a?bD(jr(a[1])):0}var
w=[0,bD,cR,dl,dm,dn,dp,jo,cS,dq,g6,g7,eq,dr,jp,jq,g8,og,jr],js=[0,function(a){return a?[0,b(w[18],a[1])]:0}];function
er(b,d,c){if(typeof
c==="number")return d;else{if(0===c[0]){var
e=er(b,d,c[1]);return a(b,d,a(b,e,e))}var
f=er(b,d,c[1]);return a(b,f,f)}}function
g9(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
jt(a){return typeof
a==="number"?oh:0===a[0]?[0,[0,a[1]]]:[1,b(w[4],a[1])]}function
ju(a){return typeof
a==="number"?oi:0===a[0]?[0,b(w[4],a[1])]:[1,[0,a[1]]]}function
cs(c,a){if(typeof
c==="number")return typeof
a==="number"?0:0===a[0]?[1,[1,a[1]]]:[1,b(w[4],a[1])];else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?[0,[1,d]]:0===a[0]?g9(cs(d,a[1])):jt(cs(d,a[1]))}var
e=c[1];return typeof
a==="number"?[0,b(w[4],e)]:0===a[0]?ju(cs(e,a[1])):g9(cs(e,a[1]))}}function
b2(c,b){if(typeof
c==="number")return b;else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?c:0===b[0]?[0,a(w[2],d,b[1])]:cs(d,b[1])}var
e=c[1];return typeof
b==="number"?c:0===b[0]?cs(b[1],e):[1,a(w[2],e,b[1])]}}function
ct(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
es(b,a){return b2(b,ct(a))}function
b3(c,b){if(typeof
c==="number")return 0;else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?0:0===b[0]?[0,a(w[11],d,b[1])]:[1,a(w[11],d,b[1])]}var
e=c[1];return typeof
b==="number"?0:0===b[0]?[1,a(w[11],e,b[1])]:[0,a(w[11],e,b[1])]}}function
jv(b){function
c(a){return b3(b,a)}return a(w[12],c,oj)}function
ok(c,a){if(typeof
a==="number")return ol;else{if(0===a[0]){var
d=a[1];return b(jv(c),d)}return 0}}function
ds(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?1:2;else{if(0===c[0]){var
d=c[1];if(typeof
b!=="number"&&0===b[0])return a(w[15],d,b[1]);return 2}var
e=c[1];if(typeof
b!=="number"&&1===b[0])return jm(a(w[15],e,b[1]));return 1}}function
jw(b,a){return 2<=ds(b,a)?0:1}function
g_(b,a){return 1===ds(b,a)?1:0}function
om(b,a){return 2<=ds(b,a)?1:0}function
on(b,a){return 1===ds(b,a)?a:b}function
et(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
oo(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
op(a){return a?[0,b(w[18],a[1])]:0}function
oq(a){return a?[0,a[1]]:0}function
cu(b,a){if(typeof
b==="number")return jw(or,a)?os:ot;else{if(0===b[0]){var
e=cu(b[1],a),f=e[1],c=b2(b3(ov,e[2]),ou);if(g_(c,a))return[0,b3(ow,f),c];var
i=es(c,a);return[0,b2(b3(oy,f),ox),i]}var
g=cu(b[1],a),h=g[1],d=b3(oz,g[2]);if(g_(d,a))return[0,b3(oA,h),d];var
j=es(d,a);return[0,b2(b3(oC,h),oB),j]}}function
jx(b,a){if(typeof
b==="number")return oD;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return oE;else{if(0===a[0])return cu(c,a);var
d=cu(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,ct(f),0];var
l=b2(a,e);return[0,ct(b2(f,oF)),l]}}var
g=b[1];if(typeof
a==="number")return oG;else{if(0===a[0]){var
h=cu(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,ct(j),0];var
m=es(a,i);return[0,ct(b2(j,oH)),m]}var
k=cu(g,[0,a[1]]),n=k[1];return[0,n,ct(k[2])]}}}function
oI(b,a){return jx(b,a)[1]}var
p=[0,g9,jt,ju,cs,b2,ct,es,b3,jv,ok,ds,jw,g_,om,on,et,oo,op,oq,cu,jx,oI,function(c,b){if(typeof
c==="number")return et(b);else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?et(c):0===b[0]?[0,a(w[17],d,b[1])]:[0,a(w[17],d,b[1])]}var
e=c[1];return typeof
b==="number"?et(c):0===b[0]?[0,a(w[17],e,b[1])]:[0,a(w[17],e,b[1])]}}];function
aQ(c,b){return 0===a(p[11],c,b)?1:0}function
oJ(a){return[0,a]}function
oK(a){return[0,a]}function
g$(d,f,e){var
c=f,b=e;for(;;)switch(c[0]){case
0:var
g=c[1];return 0===b[0]?a(d,g,b[1]):0;case
1:var
h=c[2],i=c[1];if(1===b[0]){var
j=b[2];if(0===a(w[15],i,b[1])){var
c=h,b=j;continue}return 0}return 0;default:var
k=c[3],l=c[2],m=c[1];if(2===b[0]){var
n=b[3],o=b[1];if(0===a(w[15],l,b[2])){if(g$(d,m,o)){var
c=k,b=n;continue}return 0}return 0}return 0}}function
ag(c,b){switch(b[0]){case
0:return b;case
1:var
d=b[2];return[1,a(w[2],c,b[1]),d];default:return[1,c,b]}}function
jy(a,c){return typeof
a==="number"?c:0===a[0]?[1,[1,a[1]],c]:[1,b(w[4],a[1]),c]}function
X(f,e,b,d,c){switch(b[0]){case
0:return a(e,b[1],f)?ag(0,c):[2,b,d,c];case
1:return[2,b,d,c];default:var
g=b[2],h=b[1];return g$(e,b[3],[0,f])?[2,h,a(w[2],g,d),c]:[2,b,d,c]}}function
jz(c,b,a){return[2,[0,b],a,[0,c]]}function
jA(b,a){return jz(b,a,0)}function
aw(c,a){switch(a[0]){case
0:return[0,b(c,a[1])];case
1:var
d=a[1];return[1,d,aw(c,a[2])];default:var
e=a[2],f=a[1],g=aw(c,a[3]);return[2,aw(c,f),e,g]}}function
b4(d,b,c){switch(b[0]){case
0:return[0,a(d,b[1],c)];case
1:var
e=b[1];return[1,e,b4(d,b[2],c)];default:var
f=b[2],g=b[1];return[2,g,f,b4(d,b[3],c)]}}function
cT(d,b,c){switch(b[0]){case
0:return[0,a(d,b[1],c)];case
1:var
e=b[1];return[1,e,cT(d,b[2],c)];default:var
f=b[2],g=b[1];return[2,g,f,cT(d,b[3],c)]}}function
dt(g,f,e,c,d){switch(d[0]){case
0:return ag(c,b4(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=a(p[4],m,c);return typeof
h==="number"?ag(c,a(f,i,e)):0===h[0]?ag(c,a(f,[1,h[1],i],e)):ag(m,dt(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
c==="number"?[2,l,k,a(f,j,e)]:0===c[0]?[2,l,k,dt(g,f,e,[1,c[1]],j)]:[2,l,k,dt(g,f,e,b(w[4],c[1]),j)]}}function
du(h,g,f,e,c,d){switch(d[0]){case
0:var
o=d[1];return ag(c,b4(h,aw(g,e),o));case
1:var
j=d[2],n=d[1],i=a(p[4],n,c);return typeof
i==="number"?ag(c,a(f,j,e)):0===i[0]?ag(c,a(f,[1,i[1],j],e)):ag(n,du(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
c==="number"?[2,m,l,a(f,k,e)]:0===c[0]?[2,m,l,du(h,g,f,e,[1,c[1]],k)]:[2,m,l,du(h,g,f,e,b(w[4],c[1]),k)]}}function
ha(f,g,j,d,e,c){switch(c[0]){case
0:return[2,d,e,c];case
1:var
k=c[2],h=c[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,b(w[4],h[1]),k]];default:var
l=c[3],m=c[2],n=c[1],i=a(p[4],m,e);return typeof
i==="number"?X(f,g,a(j,n,d),m,l):0===i[0]?X(f,g,a(j,[2,n,i[1],[0,f]],d),e,l):X(f,g,ha(f,g,j,d,i[1],n),m,l)}}function
hb(g,f,h,k,d,e,c){switch(c[0]){case
0:return[2,aw(f,d),e,c];case
1:var
l=c[2],i=c[1];if(typeof
i==="number")return[2,aw(f,d),e,l];else{if(0===i[0]){var
q=[1,[1,i[1]],l];return[2,aw(f,d),e,q]}var
r=[1,b(w[4],i[1]),l];return[2,aw(f,d),e,r]}default:var
m=c[3],n=c[2],o=c[1],j=a(p[4],n,e);return typeof
j==="number"?X(g,h,a(k,o,d),n,m):0===j[0]?X(g,h,a(k,[2,o,j[1],[0,g]],d),e,m):X(g,h,hb(g,f,h,k,d,j[1],o),n,m)}}function
ae(c,e,d,f,g){switch(g[0]){case
0:return b4(e,f,g[1]);case
1:var
r=g[2],s=g[1];return dt(e,function(a,b){return ae(c,e,d,a,b)},r,s,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,b4(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,ae(c,e,d,m,h)]:0===k[0]?[2,i,j,ae(c,e,d,[1,[1,k[1]],m],h)]:[2,i,j,ae(c,e,d,[1,b(w[4],k[1]),m],h)];default:var
n=f[3],o=f[2],q=f[1],l=a(p[4],o,j);if(typeof
l==="number"){var
t=ae(c,e,d,n,h);return X(c,d,ae(c,e,d,q,i),o,t)}else{if(0===l[0]){var
u=l[1],v=ae(c,e,d,n,h);return X(c,d,ae(c,e,d,[2,q,u,[0,c]],i),j,v)}var
x=l[1],y=ae(c,e,d,n,h);return X(c,d,ha(c,d,function(a,b){return ae(c,e,d,a,b)},i,x,q),o,y)}}}}function
L(d,f,g,c,e,h,i){switch(i[0]){case
0:return cT(g,h,i[1]);case
1:var
t=i[2],u=i[1];return du(f,c,function(a,b){return L(d,f,g,c,e,a,b)},t,u,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
v=h[1],x=b4(f,aw(c,j),v);return[2,aw(c,k),l,x];case
1:var
o=h[2],m=h[1];if(typeof
m==="number"){var
y=L(d,f,g,c,e,o,j);return[2,aw(c,k),l,y]}else{if(0===m[0]){var
z=L(d,f,g,c,e,[1,[1,m[1]],o],j);return[2,aw(c,k),l,z]}var
A=L(d,f,g,c,e,[1,b(w[4],m[1]),o],j);return[2,aw(c,k),l,A]}default:var
q=h[3],r=h[2],s=h[1],n=a(p[4],r,l);if(typeof
n==="number"){var
B=L(d,f,g,c,e,q,j);return X(d,e,L(d,f,g,c,e,s,k),r,B)}else{if(0===n[0]){var
C=n[1],D=L(d,f,g,c,e,q,j);return X(d,e,L(d,f,g,c,e,[2,s,C,[0,d]],k),l,D)}var
E=n[1],F=L(d,f,g,c,e,q,j);return X(d,e,hb(d,c,e,function(a,b){return L(d,f,g,c,e,a,b)},k,E,s),r,F)}}}}function
dv(f,e,d,b,c){switch(b[0]){case
0:return[0,a(e,b[1],c)];case
1:var
g=b[1];return ag(g,dv(f,e,d,b[2],c));default:var
h=b[2],i=b[1],j=dv(f,e,d,b[3],c);return X(f,d,dv(f,e,d,i,c),h,j)}}function
dw(d,g,f,c,e,b){return a(c,b,d)?[0,d]:a(c,b,g)?e:dv(d,f,c,e,b)}function
bE(f,j,i,e,g,d,c,h){switch(h[0]){case
0:return ag(c,dw(f,j,i,e,d,h[1]));case
1:var
l=h[2],q=h[1],k=a(p[4],q,c);return typeof
k==="number"?ag(c,a(g,l,d)):0===k[0]?ag(c,a(g,[1,k[1],l],d)):ag(q,bE(f,j,i,e,g,d,k[1],l));default:var
m=h[3],n=h[2],o=h[1];if(typeof
c==="number"){var
r=a(g,m,d);return X(f,e,bE(f,j,i,e,g,d,0,o),n,r)}else{if(0===c[0]){var
s=bE(f,j,i,e,g,d,[1,c[1]],m);return X(f,e,bE(f,j,i,e,g,d,c,o),n,s)}var
t=bE(f,j,i,e,g,d,b(w[4],c[1]),m);return X(f,e,bE(f,j,i,e,g,d,c,o),n,t)}}}function
ap(a,e,f,d,c,g,h){switch(h[0]){case
0:return dw(a,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return bE(a,e,d,c,function(b,g){return ap(a,e,f,d,c,b,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return dw(a,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?ap(a,e,f,d,c,l,i):0===j[0]?ap(a,e,f,d,c,[1,[1,j[1]],l],i):ap(a,e,f,d,c,[1,b(w[4],j[1]),l],i);return X(a,c,ap(a,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=ap(a,e,f,d,c,n,i),u=0,v=bE(a,e,d,c,function(b,g){return ap(a,e,f,d,c,b,g)},i,u,p),x=ap(a,e,f,d,c,ag(0,n),k),y=ap(a,e,f,d,c,p,k),z=X(a,c,v,o,t);return ae(a,f,c,X(a,c,ae(a,f,c,X(a,c,y,o,[0,a]),x),m,[0,a]),z)}}}function
dx(b,e,g,f,c,d){switch(d[0]){case
0:var
h=d[1];return[0,a(f,h,h)];case
1:var
l=d[1];return[1,l,dx(b,e,g,f,c,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=ap(b,e,g,f,c,k,ag(0,dw(b,e,f,c,i,a(g,e,e)))),n=dx(b,e,g,f,c,i);return X(b,c,ae(b,g,c,X(b,c,dx(b,e,g,f,c,k),j,[0,b]),m),j,n)}}function
jB(c,b,a){return jy(a,jA(c,b))}function
dy(h,g,f,e,d,c,n,a,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return b(c,ap(h,g,f,e,d,j,a));else{if(0===i[0]){var
k=i[1];return b(c,ap(h,g,f,e,d,dy(h,g,f,e,d,c,dy(h,g,f,e,d,c,j,a,k),a,k),a))}var
l=i[1],j=dy(h,g,f,e,d,c,j,a,l),i=l;continue}}function
jC(h,a,g,f,e,d,c,b){return b?dy(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
$(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return jB(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=$(a,f,c,g,e,d,b,j[1]);return L(a,c,e,d,b,$(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=$(a,f,c,g,e,d,b,i[1]);return L(a,c,e,d,b,$(a,f,c,g,e,d,b,j),l)}var
k=$(a,f,c,g,e,d,b,i);return ae(a,c,b,$(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=$(a,f,c,g,e,d,b,h[2]);return L(a,c,e,d,b,$(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=$(a,f,c,g,e,d,b,h[2]);return ap(a,f,c,g,b,$(a,f,c,g,e,d,b,p),q);case
5:return aw(d,$(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=$(a,f,c,g,e,d,b,h[1]);return jC(a,f,c,g,b,function(a){return a},s,r)}}function
bF(c,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,b(c,a[1])];case
2:var
d=a[1],e=bF(c,a[2]);return[2,bF(c,d),e];case
3:var
f=a[1],g=bF(c,a[2]);return[3,bF(c,f),g];case
4:return[4,bF(c,a[1])];case
5:var
h=a[2],i=a[1],j=bF(c,a[3]);return[5,bF(c,i),h,j]}return a}function
dz(d,f,e){var
b=f,c=e;for(;;){if(typeof
b!=="number")switch(b[0]){case
1:return a(d,c,b[2]);case
2:var
g=b[1],h=dz(d,b[2],c),b=g,c=h;continue;case
3:var
i=b[1],j=dz(d,b[2],c),b=i,c=j;continue;case
4:var
b=b[1];continue;case
5:var
k=b[1],l=dz(d,b[3],c),b=k,c=l;continue}return c}}function
jD(b,a){return b?[0,b[1],a]:a}function
hc(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];return jD(b,hc(a[3]))}return 0}function
cv(b){var
a=b;for(;;){if(typeof
a!=="number")switch(a[0]){case
1:return[0,a[2],0];case
2:var
c=a[1],d=cv(a[2]);return S(cv(c),d);case
3:var
e=a[1],f=cv(a[2]);return S(cv(e),f);case
4:var
a=a[1];continue;case
5:var
g=a[1],h=cv(a[3]);return S(cv(g),h)}return 0}}function
ba(c,a){if(typeof
a==="number")return 0===a?0:1;else
switch(a[0]){case
0:return[0,a[1]];case
1:var
d=a[2];return[1,b(c,a[1]),d];case
2:var
e=a[1],f=ba(c,a[2]);return[2,ba(c,e),f];case
3:var
g=a[1],h=ba(c,a[2]);return[3,ba(c,g),h];case
4:return[4,ba(c,a[1])];default:var
i=a[2],j=a[1],k=ba(c,a[3]);return[5,ba(c,j),i,k]}}var
cw=0;function
eu(e,d,c,f){if(f){var
h=f[2],g=f[1],i=a(d,c[1],g[1]);if(i){if(b(e,i[1]))return 0;var
j=eu(e,d,c,h);return j?[0,[0,g,j[1]]]:0}var
k=eu(e,d,c,h);return k?[0,[0,g,k[1]]]:0}var
l=a(d,c[1],c[1]);return l?b(e,l[1])?0:[0,[0,c,0]]:[0,[0,c,0]]}function
jE(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=eu(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
jF(e,d,c,a){var
b=0;return ep(function(f,a){var
b=jE(e,d,c,f);return b?[0,b[1],a]:a},b,a)}function
dA(d,c,a,b){if(a){var
e=a[2],f=jF(d,c,a[1],b);return S(dA(d,c,e,b),f)}return cw}function
aG(d,c,f,e,q,p){var
b=q,g=p;for(;;)if(typeof
g==="number")return 0===g?b?cw:cx:b?cx:cw;else
switch(g[0]){case
0:return cx;case
1:var
h=g[2],i=g[1];return b?a(f,i,h):a(e,i,h);case
2:var
j=g[2],k=g[1];if(b){var
r=aG(d,c,f,e,b,j);return S(aG(d,c,f,e,b,k),r)}var
s=aG(d,c,f,e,b,j);return dA(d,c,aG(d,c,f,e,b,k),s);case
3:var
l=g[2],m=g[1];if(b){var
t=aG(d,c,f,e,b,l);return dA(d,c,aG(d,c,f,e,b,m),t)}var
u=aG(d,c,f,e,b,l);return S(aG(d,c,f,e,b,m),u);case
4:var
v=g[1],b=bC(b),g=v;continue;default:var
n=g[3],o=g[1];if(b){var
w=aG(d,c,f,e,b,n);return dA(d,c,aG(d,c,f,e,bC(b),o),w)}var
x=aG(d,c,f,e,b,n);return S(aG(d,c,f,e,bC(b),o),x)}}function
ev(e,d,c,g){if(g){var
j=g[2],f=g[1],k=a(d,c[1],f[1]);if(k){if(b(e,k[1]))return[1,[0,c[2],[0,f[2],0]]];var
h=ev(e,d,c,j);return 0===h[0]?[0,[0,f,h[1]]]:[1,h[1]]}var
i=ev(e,d,c,j);return 0===i[0]?[0,[0,f,i[1]]]:[1,i[1]]}var
l=a(d,c[1],c[1]);return l?b(e,l[1])?[1,[0,c[2],0]]:[0,[0,c,0]]:[0,[0,c,0]]}function
jG(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=ev(g,f,a[1],b);if(0===c[0]){var
a=h,b=c[1];continue}return[1,c[1]]}return[0,b]}}function
jH(g,f,e,a){return ep(function(h,b){var
c=b[2],d=b[1],a=jG(g,f,e,h);return 0===a[0]?[0,[0,a[1],d],c]:[0,d,S(c,a[1])]},oL,a)}function
dB(d,c,a,b){if(a){var
g=a[1],e=dB(d,c,a[2],b),h=e[2],i=e[1],f=jH(d,c,g,b),j=f[1],k=S(h,f[2]);return[0,S(i,j),k]}return[0,cw,0]}function
bG(e,d,g,f,F,E){var
b=F,c=E;for(;;)if(typeof
c==="number")return 0===c?b?[0,cw,0]:[0,cx,0]:b?[0,cx,0]:[0,cw,0];else
switch(c[0]){case
0:return[0,cx,0];case
1:var
h=c[2],i=c[1],G=0,H=b?a(g,i,h):a(f,i,h);return[0,H,G];case
2:var
I=c[2],j=bG(e,d,g,f,b,c[1]),k=j[2],l=j[1],m=bG(e,d,g,f,b,I),n=m[2],o=m[1];if(b){var
J=S(k,n);return[0,S(l,o),J]}var
p=dB(e,d,l,o),K=p[1];return[0,K,S(k,S(n,p[2]))];case
3:var
L=c[2],q=bG(e,d,g,f,b,c[1]),r=q[2],s=q[1],t=bG(e,d,g,f,b,L),u=t[2],v=t[1];if(b){var
w=dB(e,d,s,v),M=w[1];return[0,M,S(r,S(u,w[2]))]}var
N=S(r,u);return[0,S(s,v),N];case
4:var
O=c[1],b=bC(b),c=O;continue;default:var
P=c[3],Q=c[1],x=bG(e,d,g,f,bC(b),Q),y=x[2],z=x[1],A=bG(e,d,g,f,b,P),B=A[2],C=A[1];if(b){var
D=dB(e,d,z,C),R=D[1];return[0,R,S(y,S(B,D[2]))]}var
T=S(y,B);return[0,S(z,C),T]}}function
jI(f,e,d){var
c=e,b=d;for(;;){if(c){var
g=c[2],h=c[1];if(b){var
i=b[2];if(a(f,h,b[1])){var
c=g,b=i;continue}return 0}return 0}return 1}}function
ew(g,f,e,d,c,b,a){return jI(c,aG(g,f,e,d,1,b),a)}function
hd(d,c,b){return bC(a(d,c,b))}function
he(f,e,c,b){var
d=a(e,c,b);return d?hd(f,c,b):d}function
jJ(b,a){switch(b){case
0:return oM;case
1:return 1===a?oN:0===a?oO:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?oP:oQ}}function
jK(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?oR:0;case
2:return 1===a?0:oS;default:return 1===a?0:0===a?oT:[0,a]}}function
cU(c,a){return a?b(c,a[1]):0}function
cV(d,c,b){if(c){var
e=c[1];return b?a(d,e,b[1]):0}return 0}function
jL(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,ap(g,f,e,d,c,b,h),0]]:0}function
jM(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=jJ(b[2],a[2]);return cU(function(a){return[0,[0,ap(g,f,e,d,c,h,i),a]]},j)}function
dC(e,d,c,b,a){var
f=b[1],g=a[1],h=jK(b[2],a[2]);return cU(function(a){return[0,[0,ae(e,d,c,f,g),a]]},h)}function
b5(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,jn(b[1],g,[0,[0,a],0])];case
1:return[0,[0,dx(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=b5(a,f,d,e,c,h,g,b[2]);return cU(function(b){return jL(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=b5(a,f,d,e,c,h,g,b[2]),n=b5(a,f,d,e,c,h,g,l);return cV(function(b,g){return jM(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=b5(a,f,d,e,c,h,g,b[2]),q=b5(a,f,d,e,c,h,g,o);return cV(function(b,e){return dC(a,d,c,b,e)},q,p);default:var
i=b[1];return he(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
dD(b,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
c=g[1];switch(h){case
0:return hd(d,c,b);case
1:return a(d,c,b);case
2:return a(f,c,b);default:return he(d,f,c,b)}}return 0}function
ex(c,i,h,g,b,a,f,e){var
d=b5(c,i,h,g,b,a,f,e);return d?dD(c,b,a,d[1]):0}function
jN(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=$(e,j,d,i,c,b,a,h[1]),g=$(e,j,d,i,c,b,a,k);switch(l){case
0:var
m=[0,[0,L(e,d,c,b,a,g,f),2],0];return[0,[0,L(e,d,c,b,a,f,g),2],m];case
1:return[0,[0,L(e,d,c,b,a,f,g),0],0];case
2:return[0,[0,L(e,d,c,b,a,f,g),2],0];case
3:return[0,[0,L(e,d,c,b,a,g,f),2],0];case
4:return[0,[0,L(e,d,c,b,a,f,g),3],0];default:return[0,[0,L(e,d,c,b,a,g,f),3],0]}}function
hf(i,h,g,f,e,d,c,b,a){var
j=jN(i,h,g,f,e,d,c,b);return bl(function(b){return[0,[0,b,a],0]},j)}function
jO(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=$(e,j,d,i,c,b,a,h[1]),g=$(e,j,d,i,c,b,a,k);switch(l){case
0:return[0,[0,L(e,d,c,b,a,f,g),0],0];case
1:var
m=[0,[0,L(e,d,c,b,a,g,f),2],0];return[0,[0,L(e,d,c,b,a,f,g),2],m];case
2:return[0,[0,L(e,d,c,b,a,g,f),3],0];case
3:return[0,[0,L(e,d,c,b,a,f,g),3],0];case
4:return[0,[0,L(e,d,c,b,a,g,f),2],0];default:return[0,[0,L(e,d,c,b,a,f,g),2],0]}}function
hg(i,h,g,f,e,d,c,b,a){var
j=jO(i,h,g,f,e,d,c,b);return bl(function(b){return[0,[0,b,a],0]},j)}function
ey(f,e){var
d=f,c=e;for(;;)switch(c[0]){case
0:return[0,c[1]];case
1:var
g=c[2],d=a(w[2],c[1],d),c=g;continue;default:var
h=c[3],i=c[2],j=c[1],k=ey(b(w[1],d),h);return[2,[4,ey(d,j),[6,[1,d],[0,i]]],k]}}function
jP(a){return ey(0,a)}function
bb(c,a){switch(a[0]){case
0:return[0,b(c,a[1])];case
1:return[1,a[1]];case
2:var
d=a[1],e=bb(c,a[2]);return[2,bb(c,d),e];case
3:var
f=a[1],g=bb(c,a[2]);return[3,bb(c,f),g];case
4:var
h=a[1],i=bb(c,a[2]);return[4,bb(c,h),i];case
5:return[5,bb(c,a[1])];default:var
j=a[2];return[6,bb(c,a[1]),j]}}function
ez(b,a){var
c=a[2],d=a[1],e=bb(b,a[3]);return[0,bb(b,d),c,e]}function
jQ(q,h,f,g,c){if(typeof
c!=="number")switch(c[0]){case
1:var
m=c[1];if(0===m[0]){var
n=m[1];return a(g,q,n)?0:[5,a(f,n,n)]}return[1,m];case
3:var
b=c[2],d=c[1];if(typeof
d==="number")return 0;else
switch(d[0]){case
3:var
i=d[2],j=d[1];if(typeof
j!=="number"&&5===j[0]){var
s=j[1];return typeof
b==="number"?0:5===b[0]?[3,[5,a(f,b[1],s)],i]:c}if(typeof
i!=="number"&&5===i[0]){var
r=i[1];return typeof
b==="number"?0:5===b[0]?[3,[5,a(f,b[1],r)],j]:c}return typeof
b==="number"?0:5===b[0]?a(g,h,b[1])?d:[3,d,b]:c;case
5:var
e=d[1];if(typeof
b==="number")return 0;else
switch(b[0]){case
3:var
k=b[2],l=b[1];if(typeof
l!=="number"&&5===l[0])return[3,[5,a(f,e,l[1])],k];if(typeof
k!=="number"&&5===k[0])return[3,[5,a(f,e,k[1])],l];return a(g,h,e)?b:[3,d,b];case
4:return[4,[3,[5,e],b[1]],[3,[5,e],b[2]]];case
5:return[5,a(f,e,b[1])];default:return a(g,h,e)?b:[3,d,b]}default:return typeof
b==="number"?0:5===b[0]?a(g,h,b[1])?d:[3,d,b]:c}case
4:var
o=c[2],p=c[1];return typeof
p==="number"?o:typeof
o==="number"?p:[4,p,o]}return c}var
oU=[0];function
ax(c,b){var
d=a(p[8],b[1],[0,c[2]]);return aQ(a(p[8],c[1],[0,b[2]]),d)}function
dE(c,b){var
d=a(p[8],b[1],[0,c[2]]),e=a(p[8],c[1],[0,b[2]]);return a(p[12],e,d)}function
aR(c,b){var
d=a(w[11],c[2],b[2]),e=a(p[8],b[1],[0,c[2]]),f=a(p[8],c[1],[0,b[2]]);return[0,a(p[5],f,e),d]}function
aY(c,b){var
d=a(w[11],c[2],b[2]);return[0,a(p[8],c[1],b[1]),d]}function
bH(a){var
c=a[2];return[0,b(p[6],a[1]),c]}function
b6(b,a){return aR(b,bH(a))}function
hh(b){var
a=b[1];return typeof
a==="number"?oV:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
hi(a,b){return er(aY,a,b)}function
hj(b,a){return typeof
a==="number"?oW:0===a[0]?hi(b,a[1]):hh(hi(b,a[1]))}function
oX(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
cW(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,cW(b,a[1],c)]:[1,cW(b,a[1],c),b,0]}function
eA(d,a,b,c){if(typeof
c==="number")return cW(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,cW(d,a[1],b)]:[1,cW(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,eA(d,a[1],b,e)]:[1,eA(d,a[1],b,f),h,e]}}function
bm(c){switch(c[0]){case
0:return[0,c[1]];case
1:return 0;case
2:var
d=c[1],e=bm(c[2]),f=bm(d);return cV(function(c,b){return[0,a(p[5],c,b)]},f,e);case
3:var
g=c[1],h=bm(c[2]),i=bm(g);return cV(function(c,b){return[0,a(p[7],c,b)]},i,h);case
4:var
j=c[1],k=bm(c[2]),l=bm(j);return cV(function(c,b){return[0,a(p[8],c,b)]},l,k);case
5:var
m=bm(c[1]);return cU(function(a){return[0,b(p[6],a)]},m);default:var
n=c[2],o=bm(c[1]);return cU(function(c){var
d=b(p[19],n);return[0,a(p[10],c,d)]},o)}}var
oY=p[12],oZ=p[8],o0=p[5],o2=0;function
jR(a,b){return ex(o2,o1,o0,oZ,aQ,oY,a,b)}var
o3=p[6],o4=p[7],o5=p[5],o6=0;function
aq(a,b){return L(o6,o5,o4,o3,aQ,a,b)}var
o7=p[5],o8=0;function
bc(a,b){return ae(o8,o7,aQ,a,b)}var
o9=p[6],o_=p[7],o$=p[8],pa=p[5],pc=0;function
cX(a){return $(pc,pb,pa,o$,o_,o9,aQ,a)}function
jS(c){var
d=c[3],e=c[2],a=cX(c[1]),b=cX(d);switch(e){case
0:var
f=[0,[0,aq(b,bc(a,pd)),3],0];return[0,[0,aq(a,bc(b,pe)),3],f];case
1:return[0,[0,aq(a,b),0],0];case
2:return[0,[0,aq(a,bc(b,pf)),3],0];case
3:return[0,[0,aq(b,bc(a,pg)),3],0];case
4:return[0,[0,aq(a,b),3],0];default:return[0,[0,aq(b,a),3],0]}}function
hk(b,a){var
c=jS(b);return bl(function(b){return[0,[0,b,a],0]},c)}function
jT(c){var
d=c[3],e=c[2],a=cX(c[1]),b=cX(d);switch(e){case
0:return[0,[0,aq(a,b),0],0];case
1:var
f=[0,[0,aq(b,bc(a,ph)),3],0];return[0,[0,aq(a,bc(b,pi)),3],f];case
2:return[0,[0,aq(b,a),3],0];case
3:return[0,[0,aq(a,b),3],0];case
4:return[0,[0,aq(b,bc(a,pj)),3],0];default:return[0,[0,aq(a,bc(b,pk)),3],0]}}function
hl(b,a){var
c=jT(b);return bl(function(b){return[0,[0,b,a],0]},c)}var
pl=p[12],pm=0;function
eB(a){return dD(pm,aQ,pl,a)}var
pn=p[5],po=0;function
hm(a,b){return dC(po,pn,aQ,a,b)}function
dF(a){return bG(eB,hm,hk,hl,1,a)}function
jU(e,d){var
b=a(p[21],e,d),c=b[1];return typeof
b[2]==="number"?c:a(p[5],c,pp)}function
hn(c,b){var
d=a(p[23],c,b);return a(p[15],d,pq)}function
dG(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=dG(a[1]),f=b[2],g=b[1],c=dG(e),h=c[2],i=c[1];return[0,hn(hn(g,f),i),h]}}function
dH(b,c){switch(b[0]){case
0:return[0,a(p[22],b[1],c)];case
1:var
d=b[1];return[1,d,dH(b[2],c)];default:var
e=b[2],f=b[1],g=dH(b[3],c);return[2,dH(f,c),e,g]}}function
eC(c){var
e=dG(c),f=e[2],d=e[1];if(a(p[14],d,0)){var
g=jU(b(p[6],f),d),h=b(p[6],g);return[0,dH(cT(p[7],c,f),d),h]}return[0,c,0]}function
eD(d){var
e=d[2],b=d[1];switch(e){case
0:var
f=dG(b),g=f[2],c=f[1];if(a(p[14],c,0))if(bC(aQ(g,0)))if(bC(aQ(a(p[23],c,g),c)))return 0;return[0,[0,eC(b),0]];case
1:return[0,[0,[0,b,0],e]];case
2:return[0,[0,eC(cT(p[7],b,pr)),3]];default:return[0,[0,eC(b),3]]}}function
jV(a){var
b=a[1],c=a[2];return[0,bc(b[1],[0,b[2]]),c]}function
jW(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
ps=p[12],pt=p[8],pu=p[5],pw=0;function
dI(a,b){return b5(pw,pv,pu,pt,aQ,ps,a,b)}function
ho(a){return 0===a?1:3<=a?1:0}var
jX=0;function
cY(a,b){if(b){var
c=b[3],e=b[2],d=b[1];return typeof
a==="number"?[0,d,1,c]:0===a[0]?[0,d,e,cY(a[1],c)]:[0,cY(a[1],d),e,c]}return typeof
a==="number"?px:0===a[0]?[0,0,0,cY(a[1],0)]:[0,cY(a[1],0),0,0]}function
py(a){return cY(a,jX)}function
hp(b,a){if(b){var
c=b[3],d=b[2],e=b[1];if(a){var
f=a[2],g=a[1],h=hp(c,a[3]),i=d?1:f;return[0,hp(e,g),i,h]}return b}return a}function
jY(d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return b;else{if(0===a[0]){var
a=a[1],b=[0,b];continue}var
a=a[1],b=[1,b];continue}}function
jZ(a){return jY(a,0)}function
eE(e,j,i,h){var
c=j,d=i,b=h;for(;;){if(c){var
f=c[3],g=c[1];if(c[2]){var
k=eE(e,g,d,[1,b]),c=f,d=a(e,jZ(b),k),b=[0,b];continue}var
c=f,d=eE(e,g,d,[1,b]),b=[0,b];continue}return d}}var
aZ=[0,jX,cY,py,hp,jY,jZ,eE,function(c,b,a){return eE(c,b,a,0)}];function
bI(d){var
c=d;for(;;)switch(c[0]){case
0:return aZ[1];case
1:return b(aZ[3],c[1]);case
2:var
e=c[2],f=bI(c[1]),g=bI(e);return a(aZ[4],f,g);case
3:var
h=c[2],i=bI(c[1]),j=bI(h);return a(aZ[4],i,j);case
4:var
k=c[2],l=bI(c[1]),m=bI(k);return a(aZ[4],l,m);case
5:var
c=c[1];continue;default:var
c=c[1];continue}}function
j0(b){var
c=b[3],d=bI(b[1]),e=bI(c);return a(aZ[4],d,e)}function
b7(c){var
b=c;for(;;){if(typeof
b!=="number")switch(b[0]){case
1:return j0(b[1]);case
2:var
d=b[2],e=b7(b[1]),f=b7(d);return a(aZ[4],e,f);case
3:var
g=b[2],h=b7(b[1]),i=b7(g);return a(aZ[4],h,i);case
4:var
b=b[1];continue;case
5:var
j=b[3],k=b7(b[1]),l=b7(j);return a(aZ[4],k,l)}return aZ[1]}}function
dJ(a){return[0,[1,a],3,pz]}function
hq(c,b,a){return[0,[1,c],0,[3,[1,b],[1,a]]]}function
j1(d,c,b){var
e=0;function
f(b,g){var
e=[1,a(w[2],c,b)],f=[0,a(w[2],c,b)],i=h(d,c,b,pA),j=[1,dJ(f),i],k=h(d,c,b,pB),l=[2,[1,dJ(e),k],j],m=h(d,c,b,0);return[2,[2,[1,hq(b,e,f),m],l],g]}return h(aZ[8],f,b,e)}function
j2(c,b,a){return[5,j1(c,b,b7(a)),0,a]}function
hr(w,v){var
d=w,c=v;for(;;)if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
x=c[2],g=dI(d,c[1]);if(g){var
h=g[1];if(eB(h))return 1;var
d=[0,h,d],c=x;continue}return 0;case
1:var
y=c[2],i=dI(d,c[1]);if(i){var
j=eD(i[1]);if(j){var
d=[0,jV(j[1]),d],c=y;continue}return 1}return 0;default:var
z=c[3],A=c[2],k=dI(d,c[1]);if(k){var
B=k[1],l=dI(d,A);if(l){var
C=l[1],m=eD(B);if(m){var
n=m[1],o=n[1],q=o[1],D=n[2],E=o[2],r=eD(C);if(r){var
s=r[1],t=s[1],F=s[2],G=t[2],H=t[1];if(ho(D))if(ho(F))if(jW(bc(q,H))){var
f=z,e=b(p[6],E);for(;;){if(f){var
I=f[2],J=f[1],u=hr([0,[0,aq(q,[0,e]),0],d],J);if(u){var
f=I,e=a(p[5],e,pC);continue}return u}return a(p[14],e,G)}}return 0}return 1}return 1}return 0}return 0}}function
pD(b,a){return ew(eB,hm,hk,hl,function(a){var
b=bl(b1,a);return function(a){return hr(b,a)}},b,a)}function
hs(a,b){return ex(pF,pE,aR,aY,ax,dE,a,b)}function
ht(b,a){return hf(pH,pG,aR,aY,b6,bH,ax,b,a)}function
hu(b,a){return hg(pJ,pI,aR,aY,b6,bH,ax,b,a)}function
hv(a){return dD(pK,ax,dE,a)}function
hw(a,b){return dC(pL,aR,ax,a,b)}function
hx(a){return $(pN,pM,aR,aY,b6,bH,ax,a)}function
cZ(a){return bG(hv,hw,ht,hu,1,a)}function
pO(b,a){return ew(hv,hw,ht,hu,function(a){var
b=bl(b1,a);return function(a){return hs(b,a)}},b,a)}function
j3(a){return 0===a[0]?a[1]:b(p[18],a[1])}function
aH(a){if(typeof
a==="number")return 0===a?pP:pQ;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aH(a[2]);return aR(aH(b),c);case
3:var
d=a[1],e=aH(a[2]);return b6(aH(d),e);case
4:var
f=a[1],g=aH(a[2]);return aY(aH(f),g);case
5:var
h=a[1],i=j3(a[2]);return hj(aH(h),i);case
6:return hh(aH(a[1]));default:return bH(aH(a[1]))}}function
j4(a,b){return ex(pS,pR,aR,aY,ax,dE,a,b)}function
j5(b,a){return hf(pU,pT,aR,aY,b6,bH,ax,b,a)}function
j6(b,a){return hg(pW,pV,aR,aY,b6,bH,ax,b,a)}function
j7(a){return dD(pX,ax,dE,a)}function
j8(a,b){return dC(pY,aR,ax,a,b)}aE(977,[0,bC,b1,n_,S,jm,g5,jn,bl,ep,n$,w,js,er,p,aQ,oJ,oK,g$,ag,jy,X,jz,jA,aw,b4,cT,dt,du,ha,hb,ae,L,dv,dw,bE,ap,dx,jB,dy,jC,$,bF,dz,jD,hc,cv,ba,cw,cx,eu,jE,jF,dA,S,aG,ev,jG,jH,dB,bG,jI,ew,hd,he,jJ,jK,cU,cV,jL,jM,dC,b5,dD,ex,$,L,ae,jN,hf,jO,hg,ey,jP,bb,ez,jQ,oU,ax,dE,aR,aY,bH,b6,hh,hi,hj,oX,cW,eA,bm,jR,aq,bc,cX,jS,hk,jT,hl,eB,hm,dF,jU,hn,dG,dH,eC,eD,jV,jW,dI,ho,aZ,bI,j0,b7,dJ,hq,j1,j2,hr,pD,hs,ht,hu,hv,hw,hx,cZ,pO,j3,aH,j4,j5,j6,j7,j8,function(b,a){var
c=ba(function(a){return ez(aH,a)},b);return ew(j7,j8,j5,j6,function(a){var
b=bl(b1,a);return function(a){return j4(b,a)}},c,a)}],"Micromega_plugin__Micromega");var
pZ=bk,G=[0,pZ,function(b,a){return b===a?1:0}],y=b(hy[1],[0,G[1]]),j9=y[13],p0=y[1],p1=y[2],p2=y[3],p3=y[4],p4=y[5],p5=y[6],p6=y[7],p7=y[8],p8=y[9],p9=y[10],p_=y[11],p$=y[12],qa=y[14],qb=y[15],qc=y[16],qd=y[17],qe=y[18],qf=y[19],qg=y[20],qh=y[21],qi=y[22],qj=y[23],qk=y[24],ql=y[25],qm=y[26],qn=y[27],qo=y[28],qp=y[29],qq=y[30],qr=y[31],qs=y[32],qt=y[33],qu=y[34],qv=y[35],qw=y[36],qx=y[37],qy=y[38],qz=y[39],u=[0,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p_,p$,j9,qa,qb,qc,qd,qe,qf,qg,qh,qi,qj,qk,ql,qm,qn,qo,qp,qq,qr,qs,qt,qu,qv,qw,qx,qy,qz,function(c,b){return a(j9,function(a){return h(m[1],c,qA,a)},b)}],z=b(cy[1],[0,G[1]]),j_=z[26],qB=z[1],qC=z[2],qD=z[3],qE=z[4],qF=z[5],qG=z[6],qH=z[7],qI=z[8],qJ=z[9],qK=z[10],qL=z[11],qM=z[12],qN=z[13],qO=z[14],qP=z[15],qQ=z[16],qR=z[17],qS=z[18],qT=z[19],qU=z[20],qV=z[21],qW=z[22],qX=z[23],qY=z[24],qZ=z[25],q0=z[27],q1=z[28],q2=z[29],q3=z[30],q4=z[31],q5=z[32],q6=z[33],q7=z[34],q8=z[35],q9=z[36],q_=z[37],q$=z[38],t=[0,qB,qC,qD,qE,qF,qG,qH,qI,qJ,qK,qL,qM,qN,qO,qP,qQ,qR,qS,qT,qU,qV,qW,qX,qY,qZ,j_,q0,q1,q2,q3,q4,q5,q6,q7,q8,q9,q_,q$,function(c,b){return a(j_,c-1|0,b)[3]}];function
j$(i,d,c,h){var
b=h;for(;;){if(b){var
e=b[2],g=b[1];if(e){a(d,c,g);a(f[55],c,i);var
b=e;continue}return a(d,c,g)}return 0}}function
ra(a,c){try{var
d=b(a,0);b(c,0);return d}catch(a){a=q(a);try{b(c,0)}catch(b){throw a}throw a}}function
rb(e,d){var
a=e;for(;;){if(a){var
f=a[2],c=b(a[1][1],d);if(c)return c;var
a=f;continue}return 0}}function
ka(e,d){var
c=0,b=d;for(;;){if(b){var
i=b[2],j=b[1],f=function(d){return function(c,b){return[0,a(e,d,b),c]}}(j),c=h(g[20],f,c,b),b=i;continue}return c}}function
kb(g,f,e){var
c=f,b=e;for(;;){if(c){var
h=c[2],i=c[1];if(b){var
d=b[2];if(a(g,i,b[1])){var
c=h,b=d;continue}var
b=d;continue}return 0}return 1}}function
bJ(i,a){function
c(e,a){var
c=e[2],d=e[1];if(d)return[0,d,[0,a,c]];var
f=b(i,a);return f?[0,[0,[0,f[1],a]],c]:[0,d,[0,a,c]]}return h(g[20],c,rc,a)}function
rd(j,p,i){var
m=bJ(j,i),n=m[1];if(n){var
o=n[1],g=o[1],f=o[2],c=0,d=m[2];for(;;){if(d){var
h=d[2],e=d[1],k=b(j,e);if(k){var
l=k[1];if(a(p,l,g)){var
g=l,q=[0,f,c],f=e,c=q,d=h;continue}var
c=[0,e,c],d=h;continue}var
c=[0,e,c],d=h;continue}return[0,[0,[0,g,f]],c]}}return[0,0,i]}function
kc(e,d){var
a=d;for(;;){if(a){var
f=a[2],c=b(e,a[1]);if(c)return[0,c[1]];var
a=f;continue}return 0}}function
kd(i,a){function
c(c,a){var
d=c[2],e=c[1],f=b(i,a);return f?[0,[0,[0,f[1],a],e],d]:[0,e,[0,a,d]]}return h(g[20],c,re,a)}function
eF(f,c){function
d(c,a){var
d=c[1],g=c[2],e=b(f,a);return e?[0,[0,e[1],d],1]:[0,[0,a,d],g]}var
a=h(g[20],d,rf,c),e=a[1];return a[2]?[0,e]:0}function
ke(e,c){var
d=0;function
a(a,d){var
c=b(e,d);return c?[0,c[1],a]:a}return h(g[20],a,d,c)}function
kf(j,i,c){function
d(l,k){var
c=l,d=k;for(;;){var
e=bJ(j,d),g=e[1];if(g){var
h=e[2],m=g[1],n=a(f[26],h,c),o=ke(b(i,m),n),c=a(f[26],o,c),d=h;continue}return c}}try{var
e=d(0,c);return e}catch(a){a=q(a);b(eG[4],f[28]);throw a}}function
kg(d,c){var
b=a(n[17],d,c),e=a(n[15],d,b),f=a(n[15],c,b),g=a(n[10],e,f);return a(n[10],b,g)}function
bd(a){return 2===a[0]?b(dK[3],a[1]):n[2]}function
ah(a){switch(a[0]){case
0:return b(n[36],a[1]);case
1:return a[1];default:return b(dK[2],a[1])}}function
eH(e,d){var
a=d;for(;;){var
c=b(e,a);if(c){var
a=c[1];continue}return a}}function
kh(e,d){var
a=e;for(;;){if(a){var
f=a[2],c=b(a[1],d);if(c)return[0,c[1]];var
a=f;continue}return 0}}function
ki(a){return a?ki(a[1])+1|0:0}function
eI(a){return typeof
a==="number"?1:0===a[0]?1+(2*eI(a[1])|0)|0:2*eI(a[1])|0}function
rg(a){return a?eI(a[1]):0}function
hz(a){return typeof
a==="number"?1:0===a[0]?1+(2*hz(a[1])|0)|0:2*hz(a[1])|0}function
eJ(b){if(typeof
b==="number")return n[2];else{if(0===b[0]){var
c=eJ(b[1]),d=a(n[11],2,c);return a(n[7],1,d)}var
e=eJ(b[1]);return a(n[11],2,e)}}function
hA(a){if(typeof
a==="number")return n[1];else{if(0===a[0])return eJ(a[1]);var
c=eJ(a[1]);return b(n[3],c)}}function
rh(b){var
c=b[1],e=[1,hA([0,b[2]])],f=[1,hA(c)];return a(d[9],f,e)}function
kj(a){return 0===a?0:[0,kj(a-1|0)]}function
c0(b){return a(G[2],b,1)?0:a(G[2],b&1,1)?[0,c0(b>>>1|0)]:[1,c0(b>>>1|0)]}function
ri(b){if(0<=b)return a(G[2],b,0)?0:[0,c0(b)];throw[0,a0,rj]}function
hB(b){return a(G[2],b,1)?0:a(G[2],b&1,1)?[0,hB(b>>>1|0)]:[1,hB(b>>>1|0)]}function
rk(a){var
b=bk(a,0);return 0===b?0:1===b?[0,c0(a)]:[1,c0(-a|0)]}function
eK(d){var
f=b(n[36],2);function
c(b){if(a(n[24],b,n[2]))return 0;var
d=a(n[14],b,f),e=d[1];return a(n[24],n[2],d[2])?[0,c(e)]:[1,c(e)]}return c(d)}function
kk(a){var
c=b(n[22],a);return 0===c?0:1===c?[0,eK(a)]:[1,eK(b(n[3],a))]}function
rl(a){var
b=eK(bd(a));return[0,kk(ah(a)),b]}function
rm(e){var
c=e;for(;;){if(c){var
f=c[2],d=b(c[1],0);if(a(G[2],d,0)){var
c=f;continue}return d}return 0}}function
rn(g,f,e){var
c=f,b=e;for(;;){if(c){if(b){var
h=b[2],i=c[2],d=a(g,c[1],b[1]);if(a(G[2],d,0)){var
c=i,b=h;continue}return d}return 1}return b?-1:0}}function
ro(a){return a}function
rp(a){return a+1|0}var
rq=f[1][5];function
rr(d,c){var
e=b(f[22],c);return a(f[55],d,e)}var
rs=G[1];function
rt(a){return a}var
U=b(hy[1],[0,rs]);function
ru(c){for(;;)try{var
d=a(R[15],0,c)[2];return d}catch(a){a=q(a);if(a[1]===R[1]){var
b=a[2];if(typeof
b==="number")if(11===b)continue}throw a}}function
kl(c,t,s){var
e=a(R[67],0,0),i=e[2],j=e[1],k=a(R[67],0,0),l=k[2],n=k[1],o=a(R[67],0,0),p=o[2],u=o[1],v=W(R[69],c,t,j,l,p),r=b(R[31],i);a(f[61],r,s);b(f[52],r);var
d=ru(v);function
w(e){var
c=[0,j,[0,i,[0,n,[0,l,[0,u,[0,p,0]]]]]];function
d(a){try{var
c=b(R[24],a);return c}catch(a){return 0}}return a(g[15],d,c)}return ra(function(p){switch(d[0]){case
0:var
a=d[1];if(0===a){var
e=b(R[30],n);try{var
j=b(eL[3],e);return j}catch(a){a=q(a);var
g=b(eG[1],a),i=h(m[4],rv,c,g);return b(f[3],i)}}var
k=h(m[4],rw,c,a);return b(f[3],k);case
1:var
l=h(m[4],rx,c,d[1]);return b(f[3],l);default:var
o=h(m[4],ry,c,d[1]);return b(f[3],o)}},w)}var
ai=[0,hA,rh,eI,rg,ki,hz],B=[0,c0,kk,ri,kj,rl,hB,rk,eK],cz=[0,U[1],U[2],U[3],U[4],U[5],U[6],U[7],U[8],U[9],U[10],U[11],U[12],U[13],U[15],U[16],U[17],U[18],U[19],U[20],U[21],U[22],U[24],U[26],U[28]],bK=[0,rr,rp,rq,ro,rt],hC=[0,rn,rm];aE(990,[0,G,u,t,ah,bd,hC,bK,cz,j$,B,ai,kg,ka,rb,kb,bJ,kd,rd,kc,eH,eF,kf,ke,kh,kl],"Micromega_plugin__Mutils");function
c1(k,j){var
c=k,b=j;for(;;){if(c){var
e=c[1],l=c[2],m=e[2],n=e[1];if(b){var
f=b[1],o=b[2],p=f[2],g=a(G[2],n,f[1]);if(g){var
h=a(d[26],m,p);if(h){var
c=l,b=o;continue}var
i=h}else
var
i=g;return i}return 0}return b?0:1}}function
km(f){var
c=0,a=f;for(;;){if(a){var
e=a[1],g=a[2],h=e[1],i=[0,h,b(d[56],e[2])],c=c+b(bn[27],i)|0,a=g;continue}return b(bn[27],c)}}var
N=0;function
bL(a){if(a){var
b=a[1];if(0===b[1])var
c=b[2],d=0===c[0]?0===c[1]?a[2]?0:1:0:0;else
var
d=0;if(!d)return 0}return 1}function
kn(g,e,i){var
c=i[2],f=i[1];if(a(G[2],f,0)){if(a(d[32],rz,c))return 0;var
k=b(d[40],c);return h(m[1],e,rA,k)}if(0===c[0]){var
j=c[1]+1|0;if(!(2<j>>>0))switch(j){case
0:return F(m[1],e,rC,g,f);case
1:return 0;default:return a(g,e,f)}}var
l=b(d[40],c);return W(m[1],e,rB,l,g,f)}function
eM(d,c,b){if(b){var
e=b[2],g=b[1];if(e){var
h=function(a,b){return eM(d,a,b)},i=function(a,b){return kn(d,a,b)};return Z(m[1],c,rH,i,g,h,e)}return kn(d,c,g)}return a(f[55],c,rI)}function
eN(b,a){return h(m[1],b,rJ,a)}function
cA(b,a){return eM(eN,b,a)}function
rK(e,c){function
f(e,c){function
f(c){function
f(f,i){var
c=i[2],e=i[1];if(a(G[2],e,0)){if(a(d[32],rD,c))return 0;var
j=b(d[40],c);return h(m[1],f,rE,j)}if(0===c[0]){var
g=c[1]+1|0;if(!(2<g>>>0))switch(g){case
0:return F(m[1],f,rG,eN,e);case
1:return 0;default:return eN(f,e)}}var
k=b(d[40],c);return W(m[1],f,rF,k,eN,e)}return F(m[1],e,rL,f,c)}return a(g[15],f,c)}return F(m[1],e,rM,f,c)}function
cB(b){function
e(i,h){var
c=i,b=h;for(;;){if(b){var
f=b[2],g=b[1];if(a(d[31],g,rN))return[0,[0,c,g],e(c+1|0,f)];var
c=c+1|0,b=f;continue}return 0}}return e(0,b)}function
hE(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,hD,b(c+1|0,a)]}return 0}return b(0,a)}function
c2(e,c,b){return a(d[26],c,rO)?b:[0,[0,e,c],b]}function
hF(e,d,c){if(c){var
h=c[2],i=c[1],j=i[2],g=i[1],k=a(G[1],e,g)+1|0;if(2<k>>>0)return b(f[3],rP);switch(k){case
0:return c2(e,b(d,hD),c);case
1:return c2(g,b(d,j),h);default:return[0,[0,g,j],hF(e,d,h)]}}return c2(e,b(d,hD),0)}function
I(e,d,c){if(c){var
h=c[2],i=c[1],g=i[1],k=i[2],j=a(G[1],e,g)+1|0;if(2<j>>>0)return b(f[3],rQ);switch(j){case
0:return c2(e,d,c);case
1:return c2(g,d,h);default:return[0,[0,g,k],I(e,d,h)]}}return c2(e,d,0)}function
hG(b){return a(d[26],b,rR)?0:[0,[0,0,b],0]}function
ar(b,c){if(0===b[0]){var
e=b[1];if(0===e)return 0;if(1===e)return c}function
f(c){var
e=c[1];return[0,e,a(d[7],b,c[2])]}return a(g[17],f,c)}function
c3(c,b){if(a(d[31],c,rS)){var
e=function(b){var
e=b[1];return[0,e,a(d[9],b[2],c)]};return a(g[17],e,b)}return b}function
c4(c){function
e(a){var
c=a[1];return[0,c,b(d[3],a[2])]}return a(g[17],e,c)}function
a1(q,p){var
c=q,b=p;for(;;){if(c){if(b){var
e=b[2],h=b[1],i=h[2],j=h[1],f=c[2],k=c[1],l=k[2],g=k[1],m=bk(g,j);if(0===m){var
n=a(d[2],l,i);if(a(d[32],rT,n)){var
c=f,b=e;continue}return[0,[0,g,n],a1(f,e)]}return 0<=m?[0,[0,j,i],a1(e,c)]:[0,[0,g,l],a1(f,b)]}var
o=c}else
var
o=b;return o}}function
eO(c,r,b,q){var
f=r,e=q;for(;;){if(f){if(e){var
g=e[2],j=e[1],k=j[2],l=j[1],h=f[2],m=f[1],n=m[2],i=m[1],o=bk(i,l);if(0===o){var
s=a(d[6],b,k),t=a(d[6],c,n),p=a(d[1],t,s);if(a(d[32],rU,p)){var
f=h,e=g;continue}return[0,[0,i,p],eO(c,h,b,g)]}if(0<=o){var
u=eO(c,f,b,g);return[0,[0,l,a(d[6],b,k)],u]}var
v=eO(c,h,b,e);return[0,[0,i,a(d[6],c,n)],v]}return ar(c,f)}return ar(b,e)}}function
hH(f,e,c,b){if(a(d[26],f,rV))if(a(d[26],c,rW))return a1(e,b);return eO(f,e,c,b)}function
rX(e,c){var
f=0,g=[0,function(b){return a(d[37],e[2],c[2])},f],h=[0,function(b){return a(G[1],e[1],c[1])},g];return b(hC[2],h)}var
hI=b(hC[1],rX);function
ay(i,h){var
b=h;for(;;){if(b){var
d=b[1],f=b[2],g=d[2],e=a(G[1],d[1],i);if(-1===e){var
b=f;continue}var
c=0===e?[0,[0,g,b]]:0}else
var
c=0;return c?c[1][1]:rY}}function
ko(a){if(a){var
b=0===a[1][1]?a[2]?0:1:0;if(!b)return 0}return 1}function
dL(a){if(a){var
b=a[1];if(0===b[1])return b[2]}return rZ}function
a2(a){if(a){var
b=a[1];return[0,[0,b[1],b[2],a[2]]]}return 0}function
kp(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}function
hJ(b){var
c=u[1];function
d(c,b){return a(u[4],b[1],c)}return h(g[20],d,c,b)}function
bo(a){if(a){var
b=a[1];if(0===b[1])return[0,b[2],a[2]]}return[0,r0,a]}function
r1(b,f){var
a=f;for(;;){if(a){var
c=a[2],d=a[1],e=d[1],g=d[2];if(_(b,e))return[0,g,c];if(cq(b,e))return[0,r2,a];var
a=c;continue}return[0,r3,N]}}function
kq(a){return a?[0,a[1],a[2]]:r4}function
ab(c,b,a){function
d(b,a){return h(c,b,a[1],a[2])}return h(g[20],d,b,a)}function
r5(g,f,e){var
b=f,a=e;for(;;){if(a){var
c=a[1],i=a[2],d=h(g,b,c[1],c[2]);if(d){var
b=d[1],a=i;continue}return 0}return[0,b]}}function
hK(f,e){var
b=e;for(;;){if(b){var
c=b[1],g=b[2],d=a(f,c[1],c[2]);if(d)return[0,d[1]];var
b=g;continue}return 0}}function
eP(c,b){function
d(b){return a(c,b[1],b[2])}return a(g[27],d,b)}function
kr(c,b){function
d(a){return[0,a[1]-c|0,a[2]]}return a(g[17],d,b)}function
r6(c,b){function
d(a){return[0,a[1]+c|0,a[2]]}return a(g[17],d,b)}function
hL(c){var
d=n[1],b=ab(function(c,h,b){var
d=n[2],e=bd(b),f=a(n[23],e,d);if(a(G[2],f,0)){var
g=ah(b);return a(n[17],c,g)}throw[0,a0,r7]},d,c),e=a(n[23],b,n[1]);return a(G[2],e,0)?n[2]:b}function
c5(b){var
e=n[2],f=ab(function(b,c,a){return kg(b,bd(a))},e,b),h=n[1],c=ab(function(c,e,b){var
d=ah(b);return a(n[17],c,d)},h,b),i=a(n[23],c,n[1]),j=a(G[2],i,0)?n[2]:c;function
k(b){var
c=b[1],e=a(d[6],b[2],[1,f]);return[0,c,a(d[9],e,[1,j])]}return a(g[17],k,b)}function
ks(n,m,l){var
c=m,b=l;for(;;){if(c)if(b){var
e=b[2],f=b[1],g=f[2],h=f[1],i=c[2],j=c[1],k=j[2],d=j[1];if(a(G[2],d,h)){if(a(n,k,g))return[0,[0,d,k,g]];var
c=i,b=e;continue}if(d<h){var
c=i;continue}var
b=e;continue}return 0}}function
eQ(m,l){var
e=r8,c=m,b=l;for(;;){if(c)if(b){var
f=b[2],g=b[1],h=g[1],i=c[2],j=c[1],k=j[1],n=g[2],o=j[2];if(k===h){var
p=a(d[6],o,n),e=a(d[1],e,p),c=i,b=f;continue}if(cq(k,h)){var
c=i;continue}var
b=f;continue}return e}}function
r9(c,b){function
d(b){return a(c,b[1],b[2])}return a(g[17],d,b)}function
r_(c){if(c){var
e=c[1],f=c[2],i=[0,e[1],e[2]],j=function(e,c){var
f=c[2],g=e[2],h=c[1],i=e[1],j=b(d[15],f),k=b(d[15],g);return a(d[27],k,j)?[0,i,g]:[0,h,f]};return[0,h(g[20],j,i,f)]}return 0}function
kt(c){function
d(b){return a(c,b[1],b[2])}return b(g[37],d)}aE(992,[0,km,c1,hI,eM,cA,rK,hJ,dL,bo,r1,kq,hG,ko,N,bL,ay,I,function(a){return I(a,r$,N)},hF,kp,a2,cB,hE,kr,r6,hL,c5,a1,ar,hH,c3,c4,ab,r5,hK,eP,ks,eQ,r9,r_,kt],"Micromega_plugin__Vect");function
be(e,c){if(typeof
c==="number")return a(f[55],e,sa);else
switch(c[0]){case
0:var
l=b(d[40],c[1]);return a(f[55],e,l);case
1:return h(m[1],e,sb,c[1]);case
2:return F(m[1],e,sc,be,c[1]);case
3:var
g=c[1];return Z(m[1],e,sd,be,g[1],be,g[2]);case
4:var
i=c[1];return Z(m[1],e,se,be,i[1],be,i[2]);case
5:var
j=c[1];return Z(m[1],e,sf,be,j[1],be,j[2]);default:var
k=c[1];return W(m[1],e,sg,be,k[1],k[2])}}function
c6(e,c){switch(c[0]){case
0:return h(m[1],e,sh,c[1]);case
1:return h(m[1],e,si,c[1]);case
2:return h(m[1],e,sj,c[1]);case
3:var
f=b(d[40],c[1]);return h(m[1],e,sk,f);case
4:var
g=b(d[40],c[1]);return h(m[1],e,sl,g);case
5:var
i=b(d[40],c[1]);return h(m[1],e,sm,i);case
6:return F(m[1],e,sn,be,c[1]);case
7:return a(m[1],e,so);case
8:return Z(m[1],e,sp,be,c[1],c6,c[2]);case
9:return Z(m[1],e,sq,c6,c[1],c6,c[2]);default:return Z(m[1],e,sr,c6,c[1],c6,c[2])}}aE(993,[0,be,c6],"Micromega_plugin__Sos_types");function
dM(b,a){return 0===gY(b,a)?1:0}function
dN(b,a){return gY(b,a)<0?1:0}function
ss(b,a){return gY(b,a)<=0?1:0}function
aj(d,c,a){return b(d,b(c,a))}function
dO(b){return a(d[14],st,[0,b])}function
eR(b){return a(d[14],su,[0,b])}function
ku(c){var
e=b(d[54],c),a=b(dK[5],e),f=b(dK[3],a),g=b(d[51],f),h=b(dK[2],a);return[0,b(d[51],h),g]}function
sv(a){return a[1]}function
hN(a){return aj(sv,ku,a)}function
sw(a){return a[2]}function
eS(a){return aj(sw,ku,a)}function
eT(e,c){var
f=b(d[52],c),g=b(d[52],e),h=a(n[17],g,f);return b(d[51],h)}function
eU(e,c){if(a(d[26],e,hM))if(a(d[26],c,hM))return hM;var
f=eT(e,c),g=a(d[6],e,c),h=a(d[9],g,f);return b(d[15],h)}function
bq(d,c){if(c){var
e=c[2],g=c[1];return e?a(d,g,bq(d,e)):g}return b(f[3],sx)}function
eV(d,b,c){if(b){var
e=b[1],f=eV(d,b[2],c),i=function(c,b){return[0,a(d,e,c),b]};return h(g[21],i,c,f)}return 0}function
eW(a){return h(g[21],f[17],a,sy)}function
bf(c){var
a=cQ(c)-1|0,b=0;for(;;){if(0<=a){var
d=[0,h(bg[4],c,a,1),b],a=a-1|0,b=d;continue}return b}}function
hO(f,e,d){var
c=f,a=d;for(;;){if(1<=c){var
c=c-1|0,a=b(e,a);continue}return a}}function
T(a,b){return b<a?0:[0,a,T(a+1|0,b)]}function
hP(d,c){var
a=c;for(;;){if(a){var
e=a[2],g=a[1];try{var
h=b(d,g);return h}catch(b){b=q(b);if(b[1]===eX){var
a=e;continue}throw b}}return b(f[3],sz)}}function
kv(d,c){var
a=c;for(;;){if(a){var
e=a[2],b=dM(d,a[1]);if(b)return b;var
a=e;continue}return 0}}function
sA(b,a){return kv(b,a)?a:[0,b,a]}function
hQ(b,a){return h(g[21],sA,b,a)}function
hR(c,b){function
d(a){return 1-kv(a,b)}return a(g[35],d,c)}function
dP(a,d,c){var
e=b(a,c);return dN(b(a,d),e)}function
c7(d,c){var
a=c;for(;;){if(a){var
e=a[2];b(d,a[1]);var
a=e;continue}return 0}}function
aS(d,c){if(c){var
e=c[1],i=c[2],j=b(d,e),h=a(g[37],j,i),k=h[2],l=[0,e,aS(d,h[1])],m=aS(d,k);return a(f[26],m,l)}return 0}function
kw(a){if(a){var
b=a[2];if(b){var
d=a[1],e=b[1],c=kw(b);return dM(d,e)?c:c===b?a:[0,d,c]}}return a}function
c8(a){return kw(aS(ss,a))}var
s=0;function
as(a){return typeof
a==="number"?1:0}function
kx(c,a){if(a){var
d=a[1],e=d[2],f=d[1],g=kx(c,a[2]);return[0,[0,f,b(c,e)],g]}return 0}function
aI(b,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=a[1];return[0,c,kx(b,a[2])]}var
d=a[3],e=a[2],f=a[1],g=aI(b,a[4]);return[1,f,e,aI(b,d),g]}}function
D(f,j,i){var
c=j,a=i;for(;;)if(typeof
a==="number")return c;else{if(0===a[0]){var
d=c,b=a[2];for(;;){if(b){var
e=b[1],g=b[2],d=h(f,d,e[1],e[2]),b=g;continue}return d}}var
k=a[4],c=D(f,c,a[3]),a=k;continue}}function
ky(c,a,b){if(a){var
d=a[1],e=d[2],f=d[1];return h(c,f,e,ky(c,a[2],b))}return b}function
dQ(c,e,d){var
a=e,b=d;for(;;)if(typeof
a==="number")return b;else{if(0===a[0])return ky(c,a[2],b);var
f=a[3],g=dQ(c,a[4],b),a=f,b=g;continue}}function
cC(b,e,g,d){var
c=b^g,a=c&(-c|0),f=b&(a-1|0);return 0===(b&a)?[1,f,a,e,d]:[1,f,a,d,e]}function
kz(a,b){var
c=a[1];if(b){var
d=b[2],e=b[1],f=e[1];return dM(c,f)?[0,a,d]:dN(c,f)?[0,a,b]:[0,e,kz(a,d)]}return[0,a,0]}function
eY(f,e,d,c){if(d){if(c){var
j=c[2],g=c[1],k=g[1],l=d[2],h=d[1],i=h[1],o=g[2],p=h[2];if(dN(i,k))return[0,h,eY(f,e,l,c)];if(dN(k,i))return[0,g,eY(f,e,d,j)];var
m=a(f,p,o),n=eY(f,e,l,j);return b(e,m)?n:[0,[0,i,m],n]}return d}return c}function
H(d,e){var
c=b(bn[27],d);function
g(a){if(typeof
a==="number")return[0,c,[0,[0,d,e],0]];else{if(0===a[0]){var
h=a[1],k=a[2];return h===c?[0,h,kz([0,d,e],k)]:cC(h,a,c,[0,c,[0,[0,d,e],0]])}var
i=a[4],j=a[3],b=a[2],f=a[1];return(c&(b-1|0))!==f?cC(f,a,c,[0,c,[0,[0,d,e],0]]):0===(c&b)?[1,f,b,g(j),i]:[1,f,b,j,g(i)]}}return g}function
aJ(e,d,b,a){if(typeof
b==="number")return a;else
if(0===b[0]){var
m=b[1],F=b[2];if(typeof
a==="number")var
t=0;else{if(0===a[0]){var
w=a[1],G=a[2];if(m===w){var
x=eY(e,d,F,G);return 0===x?0:[0,m,x]}return cC(m,b,w,a)}var
y=a,q=a[4],p=a[3],j=a[2],i=a[1],o=b,n=m,t=1}}else{var
k=b[4],l=b[3],g=b[2],c=b[1];if(typeof
a==="number")var
t=0;else{if(0!==a[0]){var
r=a[4],s=a[3],h=a[2],f=a[1];if(g<h){if((f&(g-1|0))!==c)return cC(c,b,f,a);if(0===(f&g)){var
B=aJ(e,d,l,a);return as(B)?k:[1,c,g,B,k]}var
C=aJ(e,d,k,a);return as(C)?l:[1,c,g,l,C]}if(h<g){if((c&(h-1|0))!==f)return cC(c,b,f,a);if(0===(c&h)){var
D=aJ(e,d,b,s);return as(D)?r:[1,f,h,D,r]}var
E=aJ(e,d,b,r);return as(E)?s:[1,f,h,s,E]}if(c===f){var
u=aJ(e,d,l,s),v=aJ(e,d,k,r);return as(u)?v:as(v)?u:[1,c,g,u,v]}return cC(c,b,f,a)}var
y=b,q=k,p=l,j=g,i=c,o=a,n=a[1],t=1}}if(t){if((n&(j-1|0))===i){if(0===(n&j)){var
z=aJ(e,d,o,p);return as(z)?q:[1,i,j,z,q]}var
A=aJ(e,d,o,q);return as(A)?p:[1,i,j,p,A]}return cC(n,o,i,y)}return b}function
a3(c,a){return b(H(c,a),s)}function
hS(c){var
a=c;for(;;)if(typeof
a==="number")return b(f[3],sB);else{if(0===a[0])return b(g[5],a[2]);var
a=a[3];continue}}function
kA(k,e,c){var
h=b(bn[27],c),a=k;for(;;){if(typeof
a!=="number"){if(0!==a[0]){var
m=a[4],n=a[3],o=0===(h&a[2])?n:m,a=o;continue}var
l=a[2];if(a[1]===h){var
d=l;for(;;){if(d){var
f=d[1],g=f[1],i=d[2],j=f[2];if(dM(c,g))return j;if(0<gY(c,g)){var
d=i;continue}return b(e,c)}return b(e,c)}}}return b(e,c)}}function
eZ(a){function
c(a){return b(f[3],sC)}return function(b){return kA(a,c,b)}}function
bh(c,b,a){return kA(c,function(b){return a},b)}function
kB(b,a){if(a){var
c=a[2],d=a[1],e=d[1];if(dM(b,e))return c;if(dN(b,e))return a;var
f=kB(b,c);return f===c?a:[0,d,f]}return 0}function
hT(k){var
e=b(bn[27],k);function
f(a){if(typeof
a!=="number")if(0===a[0]){var
l=a[2],m=a[1];if(m===e){var
g=kB(k,l);return g===l?a:0===g?0:[0,m,g]}}else{var
b=a[4],c=a[3],d=a[2],h=a[1];if((e&(d-1|0))===h){if(0===(e&d)){var
i=f(c);return i===c?a:as(i)?b:[1,h,d,i,b]}var
j=f(b);return j===b?a:as(j)?c:[1,h,d,c,j]}}return a}return f}function
cD(a){var
b=0;return c8(D(function(c,b,a){return[0,[0,b,a],c]},b,a))}function
e0(a){var
b=0;return c8(D(function(b,a,c){return[0,a,b]},b,a))}var
c9=[aX,sD,aW(0)];function
sE(a){return b0(a,0)}var
sF=a(f[17],kG,kH),sG=a(f[17],kF,sF),sH=a(f[17],kE,sG),sI=a(f[17],kD,sH),sK=bf(a(f[17],kC,sI)),sJ=256,sL=f[6];function
sM(a){return aj(sL,sE,a)}var
b8=E.caml_make_vect(h(g[21],sM,sK,sJ),0),sN=bf(kC);c7(function(b){var
a=b0(b,0);K(b8,a)[1+a]=1;return 0},sN);var
sO=bf(kD);c7(function(b){var
a=b0(b,0);K(b8,a)[1+a]=2;return 0},sO);var
sP=bf(kE);c7(function(b){var
a=b0(b,0);K(b8,a)[1+a]=4;return 0},sP);var
sQ=bf(kF);c7(function(b){var
a=b0(b,0);K(b8,a)[1+a]=8;return 0},sQ);var
sR=bf(kG);c7(function(b){var
a=b0(b,0);K(b8,a)[1+a]=16;return 0},sR);var
sS=bf(kH);c7(function(b){var
a=b0(b,0);K(b8,a)[1+a]=32;return 0},sS);function
hU(b){var
a=b0(b,0);return 1===K(b8,a)[1+a]?1:0}function
kI(b){var
a=b0(b,0);return 32===K(b8,a)[1+a]?1:0}function
e1(a,d,c){try{var
e=b(a,c);return e}catch(a){a=q(a);if(a===c9)return b(d,c);throw a}}function
af(f,e,d){var
a=b(f,d),g=a[1],c=b(e,a[2]);return[0,[0,g,c[1]],c[2]]}function
cE(a,c){try{var
d=b(a,c),f=d[1],e=cE(a,d[2]),g=[0,[0,f,e[1]],e[2]];return g}catch(a){a=q(a);if(a===c9)return[0,0,c];throw a}}function
ak(e,d,c){var
a=b(e,c),f=a[2];return[0,b(d,a[1]),f]}function
sT(e,d,c){try{var
h=b(d,c);return h}catch(c){c=q(c);if(c===c9){var
g=a(f[17],e,sU);return b(f[3],g)}throw c}}function
sV(a,c,b){function
d(a){return[0,a[1],a[2]]}function
e(a){return a[2]}function
f(c){return sT(b,a,c)}function
g(a){return af(c,f,a)}function
h(a){return ak(g,e,a)}function
i(a){return cE(h,a)}function
j(b){return af(a,i,b)}return function(a){return ak(j,d,a)}}function
hV(d,c){try{var
a=b(d,c),e=[0,[0,a[1],0],a[2]];return e}catch(a){a=q(a);if(a===c9)return[0,0,c];throw a}}function
dR(d,a){if(a){var
c=a[1],e=a[2];if(b(d,c))return[0,c,e];throw c9}throw c9}function
br(a){function
b(b){return _(b,a)}return function(a){return dR(b,a)}}function
e2(b,a,d){if(0<b)var
e=function(a){return[0,a[1],a[2]]},f=b-1|0,g=function(b){return e2(f,a,b)},h=function(b){return af(a,g,b)},c=function(a){return ak(h,e,a)};else
var
c=function(b){return cE(a,b)};return c(d)}var
cF=b(bM[16],0);function
sW(e){try{var
j=b(f[1][67],e),d=j}catch(c){c=q(c);if(c[1]!==sX)throw c;var
h=a(f[17],sY,e),d=b(f[3],h)}function
c(e){try{var
a=c([0,b(f[1][71],d),e]);return a}catch(a){a=q(a);if(a===kJ)return b(g[9],e);throw a}}var
i=c(0);b(f[1][81],d);return i}function
e3(b){var
c=sW(b);return a(bg[7],sZ,c)}function
cG(e,d){var
c=b(f[1][48],e);a(f[55],c,d);return b(f[65],c)}function
s0(d,a){var
c=a;for(;;)try{var
e=b(d,c);return e}catch(a){a=q(a);if(a[1]===eX){var
c=c+1|0;continue}throw a}}var
hW=[aX,s1,aW(0)];aE(999,[0,aj,bp,eR,dO,eW,bf,hO,hP,s,as,H,a3,hS,aJ,T,bh,eZ,D,dQ,aI,hT,e0,cD,hQ,hR,aS,c8,dP,eV,eT,eU,hN,eS,bq,ak,af,br,cE,dR,hV,hU,e1,kI,e2,sV,cF,e3,cG,function(d,c,a){var
e=bk(d,0);if(-1===e)return s0(c,a);if(0===e)throw hW;return function(e,c){var
a=c;for(;;)try{var
f=b(e,a);return f}catch(b){b=q(b);if(b[1]===eX){if(a===d)throw hW;var
a=a+1|0;continue}throw b}}(c,a)},hW],"Micromega_plugin__Sos_lib");var
hX=[aX,s2,aW(0)];function
hY(c){var
e=a(d[9],s4,s3),f=b(d[15],c);if(a(d[27],f,e))return hY(a(d[6],s5,c))-1|0;var
g=b(d[15],c);return a(d[30],g,s6)?hY(a(d[9],c,s7))+1|0:0}function
dS(j,c){if(a(d[26],c,s8))return s9;var
h=b(d[15],c),e=hY(h),k=eR(-e|0),l=a(d[6],k,h),m=a(d[1],l,s_),n=eR(j),o=a(d[6],n,m),p=b(d[23],o);if(0===e)var
i=s$;else
var
u=b(f[22],e),i=a(f[17],td,u);var
q=bf(b(d[40],p)),r=eW(b(g[6],q)),s=a(f[17],r,i),t=a(d[27],c,ta)?tb:tc;return a(f[17],t,s)}function
c_(g,f,e,d){var
c=g,a=f,b=d;for(;;){if(a){var
i=a[2],j=h(e,a[1],c,b),c=c+1|0,a=i,b=j;continue}return b}}function
dT(h,g,f){var
c=h,b=f;for(;;){var
e=c[2],d=c[1];if(e<d)return b;var
c=[0,d+1|0,e],b=a(g,d,b);continue}}function
c$(f,e,c){return a(d[26],e,te)?c:b(H(f,e),c)}function
bs(b,a){return bh(b[2],a,tf)}function
hZ(c,a){var
d=a[2],e=a[1];return[0,e,D(function(e,d,a){return c$(d,b(c,a),e)},s,d)]}function
kK(a){return typeof
a[2]==="number"?1:0}function
e4(a){return[0,a,s]}function
kL(c,b){var
e=b[1];if(a(d[26],c,th))return e4(e);var
f=b[2];return[0,e,aI(function(b){return a(d[6],c,b)},f)]}function
ti(a){var
c=b(g[1],a),d=T(1,c);return[0,c,F(g[26],H,d,a,s)]}function
kM(a){var
b=aI(d[3],a[2]);return[0,a[1],b]}function
kN(d,a){var
c=a[1][2],e=a[2];return[0,c,D(function(c,a,e){var
f=a[2];return a[1]===d?b(H(f,e),c):c},s,e)]}function
tm(a){return 0}function
tn(b,a){return b+a|0}function
e5(a,b){return aJ(tn,tm,a,b)}function
kO(b,a){return bh(a,b,0)}function
to(a){var
b=1;return D(function(c,b,e){var
a=_(b,s),d=a?c:a;return d},b,a)}function
cH(b){return a(d[26],b,tq)?s:a3(s,b)}function
kP(b,c){return a(d[26],b,tr)?s:aI(function(c){return a(d[6],b,c)},c)}function
dU(a){return aI(d[3],a)}function
cI(c,b){function
e(b){return a(d[26],b,ts)}return aJ(d[1],e,c,b)}function
h0(b,a){return cI(b,dU(a))}function
bN(c,e){return D(function(g,f,c){var
h=a(d[26],c,tt)?s:_(f,s)?aI(function(b){return a(d[6],c,b)},e):D(function(h,g,e){var
i=a(d[6],c,e);return b(H(e5(f,g),i),h)},s,e);return cI(h,g)},s,c)}function
e6(b,a){if(0===a)return cH(tu);if(1===a)return b;var
d=e6(b,a/2|0),c=bN(d,d);return 1===(a%2|0)?bN(b,c):c}function
e7(b){var
c=0;return D(function(e,d,g){var
b=0,c=D(function(b,c,a){return a+b|0},b,d);return a(f[6],c,e)},c,b)}function
h1(a){var
b=0;return dQ(function(b,c){var
a=e0(b);return function(b){return hQ(a,b)}},a,b)}function
e8(b,a){var
c=a[1],d=b[1],e=cq(d,c),h=a[2],i=b[2];if(e)var
f=e;else
var
g=_(d,c),f=g?E.caml_greaterthan(i,h):g;return f}function
kQ(c){if(_(c,s))return tw;var
d=0,e=aS(e8,cD(c));function
i(c,j){var
d=c[2],e=c[1];if(1===d)var
g=e;else
var
h=b(f[22],d),i=a(f[17],tv,h),g=a(f[17],e,i);return[0,g,j]}var
j=h(g[21],i,e,d);return a(bg[7],tx,j)}function
kR(g){var
c=g[2],e=g[1];if(_(c,s))return b(d[40],e);if(a(d[26],e,ty))return kQ(c);var
h=kQ(c),i=a(f[17],tz,h),j=b(d[40],e);return a(f[17],j,i)}function
tA(e){if(_(e,s))return tB;var
j=cD(e),k=aS(function(o,n){var
i=n[1],j=o[1],h=_(j,i);if(h)return h;var
m=aS(e8,cD(i)),b=aS(e8,cD(j)),a=m;for(;;){if(a){if(b){var
c=a[1],d=b[1],k=a[2],l=b[2],e=e8(d,c);if(e)var
f=e;else{var
g=_(d,c);if(g){var
b=l,a=k;continue}var
f=g}return f}return 0}return 1}},j);function
l(g,e){var
c=e[2],h=e[1];if(a(d[27],c,tD)){var
i=kR([0,b(d[3],c),h]),j=a(f[17],tE,i);return a(f[17],g,j)}var
k=kR([0,c,h]),l=a(f[17],tF,k);return a(f[17],g,l)}var
c=h(g[20],l,tC,k),m=h(bg[4],c,0,3),i=h(bg[4],c,3,cQ(c)-3|0),n=m5(m,tH)?i:a(f[17],tJ,i),o=a(f[17],n,tG);return a(f[17],tI,o)}function
bO(a){if(typeof
a==="number")return s;else
switch(a[0]){case
0:return cH(a[1]);case
1:return a3(a3(a[1],1),tp);case
2:return dU(bO(a[1]));case
3:var
b=a[1],f=b[1],g=bO(b[2]);return cI(bO(f),g);case
4:var
c=a[1],h=c[1],i=bO(c[2]);return h0(bO(h),i);case
5:var
d=a[1],j=d[1],k=bO(d[2]);return bN(bO(j),k);default:var
e=a[1],l=e[2];return e6(bO(e[1]),l)}}function
kS(b){var
c=T(1,b[1]);function
d(a){return bs(b,a)}var
e=20;function
h(a){return dS(e,a)}function
i(a){return aj(h,d,a)}var
j=a(g[17],i,c),k=a(bg[7],tL,j);return a(f[17],k,tK)}function
kT(b){var
c=bf(b),d=a(g[17],br,c);return bq(function(c,b){function
d(b){return a(f[17],b[1],b[2])}function
e(a){return af(c,b,a)}return function(a){return ak(e,d,a)}},d)}function
e9(a){function
b(a){return a[1][2]}function
c(a){return dR(hU,a)}function
d(a){return cE(c,a)}var
e=kT(a);function
f(a){return dR(hU,a)}function
g(a){return cE(f,a)}function
h(a){return af(g,e,a)}function
i(a){return af(h,d,a)}return function(a){return ak(i,b,a)}}function
kU(a){return dR(kI,a)}var
tX=d[43];function
tY(a){return aj(tX,eW,a)}var
tZ=1;function
t0(a){return e2(tZ,kU,a)}function
kV(a){return ak(t0,tY,a)}function
t1(c){var
e=eR(b(g[1],c)),f=eW(c),h=b(d[43],f);return a(d[9],h,e)}var
t2=1;function
t3(a){return e2(t2,kU,a)}function
t4(a){return ak(t3,t1,a)}function
t5(c){var
b=c[2],e=c[1];if(b)if(!b[2])return a(d[1],e,b[1]);return e}function
t6(a){return a[2]}var
t8=br(t7);function
t9(a){return af(t8,t4,a)}function
t_(a){return ak(t9,t6,a)}function
t$(a){return hV(t_,a)}function
ua(a){return af(kV,t$,a)}function
ub(a){return ak(ua,t5,a)}function
kW(a){function
b(a){return a[2]}var
c=br(uc);function
e(b){return af(c,a,b)}function
f(a){return ak(e,b,a)}function
g(b){return e1(f,a,b)}function
h(a){return a[2]}var
i=d[3];function
j(a){return aj(i,h,a)}var
k=br(ud);function
l(b){return af(k,a,b)}function
m(a){return ak(l,j,a)}return function(a){return e1(m,g,a)}}function
ue(a){return a[2]}var
uf=kW(kV),uh=br(ug),uj=br(ui);function
uk(a){return e1(uj,uh,a)}function
ul(a){return af(uk,uf,a)}function
um(a){return ak(ul,ue,a)}function
un(c){var
b=c[2],e=c[1];if(b)if(!b[2]){var
f=a(d[14],uo,b[1]);return a(d[6],e,f)}return e}function
up(a){return hV(um,a)}var
uq=kW(ub);function
ur(a){return af(uq,up,a)}function
kX(a){return ak(ur,un,a)}e9(ut);e9(uu);e9(uv);function
uw(a){return ux}e9(uy);kT(uz);function
uA(a){return a[1]}function
uB(a){return aj(ti,uA,a)}var
uD=br(uC),uF=br(uE);function
uG(a){return af(uF,uD,a)}function
uH(a){return af(uG,uw,a)}function
uI(a){return[0,a[1],a[2]]}function
uJ(a){return a[2]}var
uL=br(uK);function
uM(a){return af(uL,kX,a)}function
uN(a){return ak(uM,uJ,a)}function
uO(a){return cE(uN,a)}function
uP(a){return af(kX,uO,a)}function
uQ(a){return ak(uP,uI,a)}function
uR(a){return af(uQ,uH,a)}function
h2(d){var
a=ak(uR,uB,bf(d)),c=a[1];return 0===a[2]?c:b(f[3],us)}function
kY(b,a){return D(function(b,c,a){return eU(eS(a),b)},a,b)}function
kZ(e,c){return D(function(e,g,c){var
f=b(d[15],c);return a(d[38],e,f)},c,e)}function
k0(c){function
e(g){var
e=a(d[6],c,g),f=b(d[23],e);return a(d[9],f,c)}return function(a){return hZ(e,a)}}function
k2(o,n){function
W(a){return[0,1,a]}var
X=[0,[0,1,n],a(g[17],W,o)];function
Y(b){function
c(a){return-a|0}var
d=a(g[17],c,b);return a(f[26],d,b)}var
Z=a(g[17],Y,X),l=b(g[1],o)+1|0,p=2*(b(g[1],n)+1|0)|0,e=(p+l|0)-1|0,_=dT([0,1,l],function(a){return H([0,p+a|0,a+1|0],u6)},s),$=c_(1,Z,function(b,a){function
c(c,b){return H([0,b,a],[0,c])}var
d=1;return function(a){return c_(d,b,c,a)}},_),U=T(1,l);function
V(f){var
a=D(function(c,a,d){var
e=a[1];return a[2]===f?b(H(e,d),c):c},s,$);return[0,[0,e,e],D(function(d,a,c){return b(H([0,a,a],c),d)},s,a)]}var
i=a(g[17],V,U);if(a(d[26],k1,tg))var
m=e4(e);else
var
q=T(1,e),r=function(a){return H(a,k1)},m=[0,e,h(g[21],r,q,s)];var
c=h(bM[14],0,uT,uS),M=h(bg[4],c,0,cQ(c)-6|0),j=a(f[17],M,uU),N=a(bM[4],cF,uV),t=b(g[1],i)-1|0,u=b(g[5],i)[1][1],v=T(1,b(g[1],i));function
w(q,p,o){var
c=b(f[22],q-1|0),e=a(f[17],c,tM),d=0,i=p[2],j=dQ(function(b,e,a){var
c=b[2],d=b[1];return c<d?a:[0,[0,[0,d,c],e],a]},i,d);function
k(a){return a[1]}var
l=aS(function(a,b){return dP(k,a,b)},j);function
m(c,g){var
d=c[1],h=c[2],i=d[2],j=d[1],k=a(f[17],tO,g),l=dS(20,h),m=a(f[17],l,k),n=a(f[17],tP,m),o=b(f[22],i),p=a(f[17],o,n),q=a(f[17],tQ,p),r=b(f[22],j),s=a(f[17],r,q);return a(f[17],e,s)}var
n=h(g[21],m,l,tN);return a(f[17],n,o)}var
x=F(g[26],w,v,i,tR),y=kS(m),z=a(f[17],y,x),A=a(f[17],tS,z),B=b(f[22],u),C=a(f[17],B,A),E=a(f[17],tT,C),G=a(f[17],tU,E),I=b(f[22],t),J=a(f[17],I,G),K=a(f[17],tV,J),L=a(f[17],uW,K);cG(c,a(f[17],tW,L));cG(N,h3);var
O=a(f[17],j,u0),P=a(f[17],uX,O),Q=a(f[17],c,P),R=a(f[17],uY,Q),S=a(f[17],cF,R),k=i5(a(f[17],uZ,S));h2(e3(j));dj(c);dj(j);if(1!==k)if(2!==k)return 0===k?1:b(f[3],u5);return 0}function
k3(b){if(b){var
c=b[2],d=b[1];return k2(c,d)?c:a(f[26],c,[0,d,0])}throw[0,a0,u7]}function
u8(b,a){return hO(3,k3,[0,b,a])}function
h4(b,c){return a(d[26],b,u9)?0:aI(function(c){return a(d[6],b,c)},c)}function
dV(c,b){function
e(b){return a(d[26],b,u_)}return aJ(d[1],e,c,b)}function
k4(e,c){return D(function(h,g,f){var
c=b(eZ(e),g),i=a(d[6],c,f);return a(d[1],h,i)},u$,c)}function
k5(k){return function(u){var
i=s,e=u;for(;;){if(e){var
m=e[2],c=e[1];if(as(c)){var
e=m;continue}var
j=hS(c)[1];if(_(j,k))var
l=b(hT(j),c),h=as(l)?b(f[3],va):hS(l)[1];else
var
h=j;var
n=b(eZ(c),h),p=b(hT(h),c),q=h4(a(d[9],vb,n),p),o=function(g,h,i){return function(c){var
e=bh(c,h,vc);if(a(d[26],e,vd))return c;var
f=b(d[3],e);return dV(c,h4(a(d[9],f,i),g))}}(c,h,n),r=a(g[17],o,m),t=aI(o,i),i=b(H(h,q),t),e=r;continue}var
v=0;return[0,c8(D(function(c,e,b){var
d=hR(e0(b),[0,k,0]);return a(f[26],d,c)},v,i)),i]}}}function
h5(c){var
g=c[1],e=g[1];if(g[2]!==e)return b(f[3],vf);function
i(l,g){var
c=l;for(;;){if(kK(g))return 0;var
h=bs(g,[0,c,c]);if(a(d[27],h,vg))return b(f[3],vh);if(a(d[26],h,vi)){if(kK(kN(c,g))){var
c=c+1|0;continue}return b(f[3],vj)}var
j=kN(c,g),k=hZ(function(b){return a(d[9],b,h)},j);return[0,[0,h,k],i(c+1|0,[0,[0,e,e],dT([0,c+1|0,e],function(b){function
f(c){var
e=bs(k,c),f=bs(j,b),h=a(d[6],f,e),i=bs(g,[0,b,c]),l=a(d[4],i,h),m=[0,b,c];return function(a){return c$(m,l,a)}}var
h=[0,c+1|0,e];return function(a){return dT(h,f,a)}},s)])]}}return i(1,c)}function
k6(b){if(0===b)return[0,vk,b];function
f(e){var
b=e[2],f=e[1],g=b[2],h=D(function(b,c,a){return eT(b,hN(a))},vl,g),i=b[2],j=D(function(b,c,a){return eU(b,eS(a))},vm,i),c=a(d[9],j,h),k=hZ(function(b){return a(d[6],c,b)},b),l=a(d[6],c,c);return[0,a(d[9],f,l),k]}var
c=a(g[17],f,b);function
i(a){return a[1]}function
j(a){return aj(hN,i,a)}function
k(a){return aj(eT,j,a)}var
l=h(g[21],k,c,vn);function
m(a){return a[1]}function
n(a){return aj(eS,m,a)}function
o(a){return aj(eU,n,a)}var
p=h(g[21],o,c,vo),e=a(d[9],p,l);function
q(b){var
c=b[2];return[0,a(d[6],e,b[1]),c]}var
r=a(g[17],q,c);return[0,a(d[9],vp,e),r]}function
h6(c,d){if(0<=c){if(0===c)return[0,s,0];if(0===d)return[0,s,0];var
e=T(0,c),h=function(e){var
f=h6(c-e|0,b(g[6],d));function
h(a){return 0===e?a:b(H(b(g[5],d),e),a)}return a(g[17],h,f)},i=a(g[17],h,e);return bq(f[26],i)}return 0}function
h7(b,j){var
c=j;for(;;){if(0===b)return[0,[0,cH(bp),[5,bp]],0];if(0<=b){if(c){var
d=c[2],e=c[1],h=e[1],k=e[2],i=e7(h);if(0===i){var
c=d;continue}var
l=h7(b-i|0,d),m=function(a){var
b=[10,k,a[2]];return[0,bN(h,a[1]),b]},n=a(g[17],m,l),o=h7(b,d);return a(f[26],o,n)}return[0,[0,cH(bp),[5,bp]],0]}return 0}}function
k7(d,c,a){return D(function(a,e,d){return D(function(a,g,f){var
c=e5(e,g),h=bh(a,c,s);return b(H(c,dV(h4(d,f),h)),a)},a,c)},a,d)}function
vP(b){return a(d[26],b,vQ)}var
vR=d[1];function
k8(b,c){return a(d[26],b,vS)?s:aI(function(c){return a(d[6],b,c)},c)}function
vU(aC,m,k,r,q){var
aD=0;function
aE(a){return a[1]}var
aF=a(g[17],aE,r),aG=a(f[26],[0,q,k],aF);function
aH(a){return aj(hQ,h1,a)}var
y=h(g[21],aH,aG,aD);if(aC)var
aK=function(a){return e7(a[1])<=m?1:0},aL=a(g[35],aK,r),e=[0,[0,cH(bp),[5,bp]],aL];else
var
e=h7(m,r);var
aM=b(g[1],e);function
aN(e,d){var
c=h6(m-e7(d)|0,y),f=T(1,b(g[1],c)),i=a(g[47],c,f);function
j(a){var
b=a[2],c=a[1];return H(c,a3([0,-e|0,-b|0,b],vV))}return[0,c,h(g[21],j,i,s)]}function
aO(i,e){var
c=h6((m-e7(e[1])|0)/2|0,y),f=T(1,b(g[1],c)),d=a(g[47],c,f);function
j(e){var
c=e[2],h=e[1];function
f(e,a){var
d=e[2],f=e5(h,e[1]);if(d<c)return a;var
g=c===d?vW:vX,j=bh(a,f,s);return b(H(f,dV(a3([0,i,c,d],g),j)),a)}return a(g[21],f,d)}return[0,c,h(g[21],j,d,s)]}var
aP=T(1,b(g[1],e)),aQ=h(g[23],aO,aP,e),z=b(g[46],aQ),A=z[1],aR=z[2],aT=T(1,b(g[1],k)),aU=h(g[23],aN,aT,k),B=b(g[46],aU)[2],t=a(g[17],g[1],A),aV=dU(q),Z=D(function(e,c,a){return b(H(c,a3(vq,b(d[3],a))),e)},s,aV);function
aW(c,b,a){return k7(c[1],b,a)}var
aX=F(g[26],aW,e,aR,Z);function
aY(c,b,a){return k7(c,b,a)}var
aZ=F(g[26],aY,k,B,aX),a0=0,a1=D(function(b,c,a){return[0,a,b]},a0,aZ),C=b(k5(vY),a1),c=C[1],a2=C[2],a4=[0,vZ,c];function
a5(a){return H(a,a3(a,v0))}var
u=h(g[21],a5,c,a2);function
a6(j){return D(function(e,c,k){var
h=c[3],i=c[2],f=c[1];if(0<=f){var
g=bh(k,j,v1);if(a(d[26],g,v2))return e;var
l=b(H([0,f,i,h],g),e);return b(H([0,f,h,i],g),l)}return e},s,u)}var
a7=D(function(b,a,c){var
d=a[3],e=a[2];if(0<a[1])if(e===d)return dV(c,b);return b},s,u),n=a(g[17],a6,a4),a8=c_(1,c,function(b,a){var
c=bh(a7,b,v3);return function(b){return c$(a,c,b)}},s),E=[0,b(g[1],c),a8];if(0===c)var
G=e4(0);else{var
N=h(g[21],kY,n,u1),O=kY(E[2],u2),P=function(b){return a(d[6],N,b)},Q=function(a){return aI(P,a)},w=a(g[17],Q,n),x=kL(O,E),R=h(g[21],kZ,w,u3),S=kZ(x[2],u4),U=dO(20-(Math.log(b(d[56],R))/nS|0)|0),V=dO(20-(Math.log(b(d[56],S))/nS|0)|0),W=function(b){return a(d[6],b,U)},X=function(a){return aI(W,a)},o=a(g[17],X,w),Y=kL(V,x),i=h(bM[14],0,vF,vE),ar=h(bg[4],i,0,cQ(i)-6|0),p=a(f[17],ar,vG),at=a(bM[4],cF,vH),_=b(g[1],o)-1|0,$=T(1,b(g[1],o)),aa=function(p,o,n){var
c=b(f[22],p-1|0),e=a(f[17],c,vr),d=0,i=D(function(b,a,e){var
c=a[3],d=a[2],f=a[1];return c<d?b:[0,[0,[0,f,d,c],e],b]},d,o);function
j(a){return a[1]}var
k=aS(function(a,b){return dP(j,a,b)},i);function
l(d,g){var
c=d[1],h=d[2],i=c[3],j=c[2],k=c[1],l=a(f[17],vt,g),m=dS(20,h),n=a(f[17],m,l),o=a(f[17],vu,n),p=b(f[22],i),q=a(f[17],p,o),r=a(f[17],vv,q),s=b(f[22],j),t=a(f[17],s,r),u=a(f[17],vw,t),v=b(f[22],k),w=a(f[17],v,u);return a(f[17],e,w)}var
m=h(g[21],l,k,vs);return a(f[17],m,n)},ab=F(g[26],aa,$,o,vx),ac=kS(Y),ad=a(f[17],ac,ab),ae=a(f[17],vy,ad),af=a(g[17],f[22],t),ag=a(bg[7],vz,af),ah=a(f[17],ag,ae),ai=a(f[17],vA,ah),ak=b(f[22],aM),al=a(f[17],ak,ai),am=a(f[17],vB,al),an=b(f[22],_),ao=a(f[17],an,am),ap=a(f[17],vC,ao),aq=a(f[17],vI,ap);cG(i,a(f[17],vD,aq));cG(at,h3);var
au=a(f[17],p,vM),av=a(f[17],vJ,au),aw=a(f[17],i,av),ax=a(f[17],vK,aw),ay=a(f[17],cF,ax),j=i5(a(f[17],vL,ay)),az=h2(e3(p));dj(i);dj(p);if(1===j)var
l=0;else
if(2===j)var
l=0;else
if(3===j)var
l=1;else
if(0===j)var
l=1;else{var
aA=b(f[22],j),aB=a(f[17],vO,aA);b(f[3],aB);var
l=1}if(!l)b(f[3],vN);var
G=az}function
I(i){var
c=b(k0(i),G),l=k8(vT,a(g[7],n,0));function
j(b,d){var
e=a(g[7],n,b);return aJ(vR,vP,k8(bs(c,b),e),d)}var
k=dT([0,1,c[1]],j,l),d=T(1,b(g[1],t)),e=a(g[47],t,d);function
f(a){var
c=a[1],d=a[2];return[0,[0,c,c],D(function(c,a,e){var
f=a[3],g=a[2];return a[1]===d?b(H([0,g,f],e),c):c},s,k)]}var
h=a(g[17],f,e);return[0,c,a(g[17],h5,h)]}if(0===c)var
v=I(bp);else
var
bq=T(5,66),br=a(g[17],dO,bq),bt=T(1,31),bu=a(g[17],d[47],bt),v=hP(I,a(f[26],bu,br));var
J=v[1],a9=v[2],a_=a3(v5,v4),a$=T(1,J[1]);function
ba(b){var
d=bs(J,b);return H(a(g[7],c,b-1|0),d)}var
K=h(g[21],ba,a$,a_),bb=D(function(d,c,a){return b(H(c,k4(K,a)),d)},K,u);function
bc(a){return D(function(c,b,a){return c$(b,k4(bb,a),c)},s,a)}function
bd(c){function
d(d){var
e=d[2],f=d[1],i=T(1,b(g[1],c));function
j(b,d){var
f=bs(e,b);return c$(a(g[7],c,b-1|0),f,d)}return[0,f,h(g[21],j,i,s)]}return b(g[17],d)}var
be=h(g[23],bd,A,a9),L=a(g[17],bc,B);function
bf(b,a){return[0,b,a]}var
bi=h(g[23],bf,e,be);function
bj(a){return 0!==a[2]?1:0}var
M=a(g[35],bj,bi),bk=dU(q);function
bl(b,a){var
c=bN(b,a);return function(a){return cI(c,a)}}var
bm=F(g[26],bl,L,k,bk);function
bn(a){var
c=a[2],d=a[1][1];function
b(a){var
b=a[2],c=a[1],d=kP(c,bN(b,b));return function(a){return cI(d,a)}}var
e=bN(d,h(g[21],b,c,s));return function(a){return cI(e,a)}}if(as(h(g[21],bn,M,bm))){var
bo=function(a){return[0,a[1][2],a[2]]};return[0,L,a(g[17],bo,M)]}throw hX}function
h8(a){var
b=cD(a);function
c(a){return a[1]}return aS(function(a,b){return dP(c,a,b)},b)}function
k9(a){if(_(a,s))return[0,bp];var
b=h8(a),c=0;function
d(a,d){var
b=a[2],c=a[1],e=1===b?[1,c]:[6,[0,[1,c],b]];return[0,e,d]}var
e=h(g[21],d,b,c);return bq(function(b,a){return[5,[0,b,a]]},e)}function
v6(e){var
b=e[2],c=e[1];return _(c,s)?[0,b]:a(d[26],b,bp)?k9(c):[5,[0,[0,b],k9(c)]]}function
k_(b){if(_(b,s))return 0;var
c=cD(b),d=aS(function(C,B){var
o=B[1],p=C[1];if(_(o,s))return 1;if(_(p,s))return 0;var
k=h8(p),l=h8(o),t=0;function
u(a){return a[2]}function
v(b,a){return b+a|0}function
w(a){return aj(v,u,a)}var
m=h(g[21],w,k,t),x=0;function
y(a){return a[2]}function
z(b,a){return b+a|0}function
A(a){return aj(z,y,a)}var
n=h(g[21],A,l,x);if(m<n)return 0;if(n<m)return 1;var
b=k,a=l;for(;;){if(b){if(a){var
c=a[1],d=c[2],e=c[1],f=b[1],i=f[2],j=f[1],q=a[2],r=b[2];if(cq(j,e))return 1;if(cq(e,j))return 0;if(cq(i,d))return 0;if(cq(d,i))return 1;var
b=r,a=q;continue}return 0}return a?1:1}},c),e=a(g[17],v6,d);return bq(function(b,a){return[3,[0,b,a]]},e)}function
v7(a){var
b=a[1];return[10,[5,b],[6,k_(a[2])]]}function
v8(b){var
c=b[2],d=b[1];if(0===c)return d;var
e=a(g[17],v7,c);return[10,d,bq(function(b,a){return[9,b,a]},e)]}function
k$(b){if(0===b)return v9;var
c=0;function
d(c,d){var
e=k$(hR(b,[0,c,0]));function
h(a){return[0,c,a]}var
i=a(g[17],h,e);return a(f[26],i,d)}return h(g[21],d,b,c)}function
h9(d,c){return D(function(f,e,c){return b(H(a(g[38],e,d),c),f)},s,c)}aE(1001,[0,to,dU,bN,e6,cH,bO,k_,v8,tA,vU,function(c){var
n=h1(c),o=h1(c),K=e0(c);function
L(b){function
c(a){return kO(a,b)}return a(g[17],c,o)}var
u=a(g[17],L,K);function
M(e){var
b=0;return(D(function(c,b,g){var
d=kO(e,b);return a(f[6],d,c)},b,c)+1|0)/2|0}var
N=a(g[17],M,o);function
O(a){var
b=T(0,a);function
c(b,a){return[0,b,a]}return function(a){return eV(c,b,a)}}var
P=h(g[21],O,N,ve),G=[0,b(g[5],u),0],I=b(g[6],u),t=h(g[21],u8,I,G),J=hO(b(g[1],t),k3,t);function
Q(b){function
c(a){return 2*a|0}return k2(J,a(g[17],c,b))}var
R=a(g[35],Q,P),S=b(g[9],R);function
U(a){function
c(d,c,a){return 0===c?a:b(H(d,c),a)}return F(g[26],c,o,a,s)}var
i=a(g[17],U,S),q=b(g[1],i),aD=k$(n);function
aE(d){var
e=a(g[47],n,d);return as(h0(c,D(function(d,c,a){return b(H(h9(e,c),a),d)},s,c)))}var
aF=a(g[35],aE,aD),aG=T(1,b(g[1],i)),v=a(g[47],i,aG),aH=eV(function(b,a){return[0,[0,b[1],a[1]],[0,b[2],a[2]]]},v,v);function
aK(b){var
a=b[2];return a[1]<=a[2]?1:0}var
aL=a(g[35],aK,aH);function
aM(b){var
c=b[2],d=b[1],e=c[2],f=c[1],h=d[2],i=d[1];function
j(b){var
c=h9(a(g[47],n,b),h);return[0,[0,h9(a(g[47],n,b),i),c],[0,f,e]]}return a(g[17],j,aF)}var
aN=a(g[17],aM,aL),w=bq(f[26],aN);function
aO(a){return a[1]}var
aP=c8(a(g[17],aO,w));function
aQ(b){function
c(a){return _(a[1],b)}return a(g[35],c,w)}var
aR=a(g[17],aQ,aP);function
aT(a){return a[2]}var
aU=b(g[17],aT);function
aV(a){return aj(c8,aU,a)}var
aW=a(g[17],aV,aR);function
aX(c,d){if(c){var
e=c[2],h=c[1];if(e){var
i=function(a){var
c=a3(h,ww);return b(H(a,wx),c)},j=a(g[17],i,e);return a(f[26],j,d)}return d}throw hX}var
aY=h(g[21],aX,aW,0),aZ=D(function(d,c,a){return b(H(c,a3(wy,a)),d)},s,c),a0=c_(1,i,function(f,a){function
c(g,d,c){var
e=e5(f,g);if(d<a)return c;var
h=a===d?wz:wA,i=bh(c,e,s);return b(H(e,b(H([0,a,d],h),i)),c)}var
d=1;return function(a){return c_(d,i,c,a)}},aZ),a1=0,a2=D(function(b,c,a){return[0,a,b]},a1,a0),a4=a(f[26],a2,aY),x=b(k5(wB),a4),j=x[1],a5=x[2];function
a6(a){return H(a,a3(a,wC))}var
y=h(g[21],a6,j,a5),a7=[0,wD,j],a8=T(1,q);function
a9(a){return b(eZ(y),[0,a,a])}var
a_=bq(dV,a(g[17],a9,a8));function
a$(i){return[0,[0,q,q],D(function(f,e,j){var
g=e[2],h=e[1],c=bh(j,i,wE);if(a(d[26],c,wF))return f;var
k=b(H([0,h,g],c),f);return b(H([0,g,h],c),k)},s,y)]}var
e=a(g[17],a$,a7),ba=c_(1,j,function(b,a){var
c=bh(a_,b,wG);return function(b){return c$(a,c,b)}},s),z=[0,b(g[1],j),ba];if(0===j)var
A=e4(0);else{var
k=h(bM[14],0,wm,wl),at=h(bg[4],k,0,cQ(k)-6|0),p=a(f[17],at,wn),au=a(bM[4],cF,wo),ac=b(g[1],e)-1|0,ad=b(g[5],e)[1][1],ae=T(1,b(g[1],e)),af=function(q,p,o){var
c=b(f[22],q-1|0),e=a(f[17],c,wa),d=0,i=p[2],j=dQ(function(b,e,a){var
c=b[2],d=b[1];return c<d?a:[0,[0,[0,d,c],e],a]},i,d);function
k(a){return a[1]}var
l=aS(function(a,b){return dP(k,a,b)},j);function
m(c,g){var
d=c[1],h=c[2],i=d[2],j=d[1],k=a(f[17],wc,g),l=dS(20,h),m=a(f[17],l,k),n=a(f[17],wd,m),o=b(f[22],i),p=a(f[17],o,n),q=a(f[17],we,p),r=b(f[22],j),s=a(f[17],r,q);return a(f[17],e,s)}var
n=h(g[21],m,l,wb);return a(f[17],n,o)},ag=F(g[26],af,ae,e,wf),V=T(1,z[1]),W=function(a){return bs(z,a)},X=20,Y=function(a){return dS(X,a)},Z=function(a){return aj(Y,W,a)},$=a(g[17],Z,V),aa=a(bg[7],v$,$),ab=a(f[17],aa,v_),ah=a(f[17],ab,ag),ai=a(f[17],wg,ah),ak=b(f[22],ad),al=a(f[17],ak,ai),am=a(f[17],wh,al),an=a(f[17],wi,am),ao=b(f[22],ac),ap=a(f[17],ao,an),aq=a(f[17],wj,ap),ar=a(f[17],wp,aq);cG(k,a(f[17],wk,ar));cG(au,h3);var
av=a(f[17],p,wt),aw=a(f[17],wq,av),ax=a(f[17],k,aw),ay=a(f[17],wr,ax),az=a(f[17],cF,ay),l=i5(a(f[17],ws,az)),aA=h2(e3(p));dj(k);dj(p);if(1===l)var
m=0;else
if(2===l)var
m=0;else
if(3===l)var
m=1;else
if(0===l)var
m=1;else{var
aB=b(f[22],l),aC=a(f[17],wv,aB);b(f[3],aC);var
m=1}if(!m)b(f[3],wu);var
A=aA}function
bb(c){var
l=b(k0(c),A),h=kM(a(g[7],e,0));function
i(n,m){var
o=a(g[7],e,n),p=bs(l,n),h=o[1],i=h[2],j=h[1];if(a(d[26],p,tj))var
c=[0,[0,j,i],s];else
var
q=o[2],c=[0,[0,j,i],aI(function(b){return a(d[6],p,b)},q)];var
k=c[1];if(E.caml_notequal(k,m[1]))return b(f[3],tk);var
r=m[2],t=c[2];function
u(b){return a(d[26],b,tl)}return[0,k,aJ(d[1],u,t,r)]}return k6(h5(dT([0,1,l[1]],i,h)))}if(0===j)var
r=k6(h5(kM(a(g[7],e,0))));else
var
bf=T(5,66),bi=a(g[17],dO,bf),bj=T(1,31),bk=a(g[17],d[47],bj),r=hP(bb,a(f[26],bk,bi));var
B=r[1],bc=r[2];function
bd(c){var
d=c[1],e=c[2][2];return[0,d,D(function(e,d,c){return b(H(a(g[7],i,d-1|0),c),e)},s,e)]}var
C=a(g[17],bd,bc);function
be(a){var
b=a[1],c=e6(a[2],2);return bN(cH(b),c)}if(as(h0(kP(B,bq(cI,a(g[17],be,C))),c)))return[0,B,C];throw hX}],"Micromega_plugin__Sos");var
h_=[0,f[8]],wI=d[2],wJ=d[7],aa=b(cy[1],[0,G[1]]),wH=0;function
la(d){try{var
e=b(aa[24],d),f=e[1],i=e[2],g=a(aa[26],f,d),j=g[3];if(b(aa[2],g[1]))if(b(aa[2],j))var
h=[0,[0,f,i]],c=1;else
var
c=0;else
var
c=0;if(!c)var
h=0;return h}catch(a){a=q(a);if(a===O)return 0;throw a}}function
e_(e,a){function
c(b,a){var
c=a[2],d=a[1];return 1===c?h(m[1],b,wK,d):F(m[1],b,wL,d,c)}function
d(b,a){if(a){var
e=a[2],f=a[1];return e?Z(m[1],b,wM,c,f,d,e):c(b,f)}return 0}return d(e,b(aa[19],a))}var
dW=aa[1];function
lb(a){var
b=0;function
c(c,b,a){return a+b|0}return h(aa[13],c,a,b)}function
e$(c,b){var
d=lb(c),e=lb(b);return a(G[2],d,e)?h(aa[10],G[1],c,b):a(G[1],d,e)}function
fa(a){return _(a,aa[1])}function
fb(a){return h(aa[4],a,1,aa[1])}function
lc(b){var
a=la(b);return a?1===a[1][2]?1:0:0}function
h$(c){var
a=la(c);if(a){var
b=a[1],d=b[1];return 1===b[2]?[0,d]:0}return 0}function
ld(a){if(fa(a))return 0;try{var
b=function(c,a,b){var
d=a/2|0;if(0===(a%2|0))return h(aa[4],c,d,b);throw O},c=[0,h(aa[13],b,a,dW)];return c}catch(a){a=q(a);if(a===O)return 0;throw a}}function
ia(c,b){try{var
d=a(aa[27],c,b);return d}catch(a){a=q(a);if(a===O)return 0;throw a}}function
ib(b,a){function
c(b,c,a){var
d=ia(b,a)+c|0;return h(aa[4],b,d,a)}return h(aa[13],c,b,a)}function
le(c,b){var
e=f[8];function
g(e,d,b){var
g=E.caml_div(ia(e,c),d);return a(f[5],b,g)}var
d=h(aa[13],g,b,e),i=aa[1];function
j(c,f,a){var
e=f-gZ(ia(c,b),d)|0;return 0===e?a:h(aa[4],c,e,a)}return[0,h(aa[13],j,c,i),d]}function
lf(b){var
c=u[1];function
d(c,d,b){return a(u[4],c,b)}return h(aa[13],d,b,c)}var
lg=aa[13],C=b(cy[1],[0,e$]),lh=C[8],wN=C[1],wO=C[2],wP=C[3],wQ=C[4],wR=C[5],wS=C[6],wT=C[7],wU=C[10],wV=C[11],wW=C[12],wX=C[13],wY=C[14],wZ=C[15],w0=C[16],w1=C[17],w2=C[18],w3=C[19],w4=C[20],w5=C[21],w6=C[22],w7=C[23],w8=C[24],w9=C[25],w_=C[26],w$=C[27],xa=C[28],xb=C[29],xc=C[30],xd=C[31],xe=C[32],xf=C[33],xg=C[34],xh=C[35],xi=C[36],xj=C[37],xk=C[38],da=[0,wN,wO,wP,wQ,wR,wS,wT,lh,wU,wV,wW,wX,wY,wZ,w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w_,w$,xa,xb,xc,xd,xe,xf,xg,xh,xi,xj,xk,function(e){return b(lh,function(f,b,a){if(b){var
c=b[1];if(a)return h(e,f,c,a[1]);var
d=c}else{if(!a)return 0;var
d=a[1]}return[0,d]})}];function
xl(e,g){var
c=g[2],f=g[1];if(fa(f)){if(a(d[32],xm,c))return 0;var
j=b(d[40],c);return h(m[1],e,xn,j)}if(0===c[0]){var
i=c[1]+1|0;if(!(2<i>>>0))switch(i){case
0:return F(m[1],e,xp,e_,f);case
1:return 0;default:return e_(e,f)}}var
k=b(d[40],c);return W(m[1],e,xo,k,e_,f)}var
al=b(cy[1],[0,e$]);function
li(c,b){function
d(b,a){return F(m[1],c,xq,xl,[0,b,a])}return a(al[12],d,b)}function
lj(c,b){try{var
d=a(al[27],c,b);return d}catch(a){a=q(a);if(a===O)return xr;throw a}}function
xs(a){var
b=al[1],c=fb(a);return h(al[4],c,xt,b)}function
dX(a){return h(al[4],dW,a,al[1])}function
dY(e,f,c){if(0===b(d[25],f))return c;var
g=a(wI,lj(e,c),f);return 0===b(d[25],g)?a(al[7],e,c):h(al[4],e,g,c)}function
lk(b,a){function
c(c,b,a){return dY(c,b,a)}return h(al[13],c,b,a)}function
ll(c,i){var
e=al[1];function
f(k,c,j){if(0===b(d[25],c))var
e=dX(xu);else
var
f=al[1],g=function(e,d,b){var
f=a(wJ,c,d),g=ib(k,e);return h(al[4],g,f,b)},e=h(al[13],g,i,f);return lk(e,j)}return h(al[13],f,c,e)}function
xv(c){function
e(a){return b(d[3],a)}return a(al[33],e,c)}var
lm=al[13],fc=[aX,xy,aW(0)];function
ln(a){return 2===a[2]?1:0}function
fd(a){switch(a){case
0:return d[26];case
1:return d[30];default:return d[28]}}function
dZ(a){switch(a){case
0:return xz;case
1:return xA;default:return xB}}function
xC(c,a){var
e=a[2],f=a[1],g=b(d[40],a[3]),h=dZ(e);return Z(m[1],c,xD,cA,f,h,g)}function
ic(c,b){switch(c){case
2:if(2<=b)return 2;var
a=0;break;case
1:var
a=0;break;default:var
a=1}if(!a)if(0!==b)return 1;return 0}function
fe(c,a){switch(c){case
0:var
d=a,b=1;break;case
1:if(1===a)return 1;var
b=0;break;default:var
b=0}if(!b){if(0!==a)return 2;var
d=c}return d}var
ff=b(cy[1],[0,e$]),fg=b(cy[1],[0,G[1]]),fh=[0,ff[1]],fi=[0,fg[1]],id=[0,0];function
xE(a){fh[1]=ff[1];fi[1]=fg[1];id[1]=0;return 0}function
d0(b){try{var
d=a(ff[27],b,fh[1]);return d}catch(a){a=q(a);if(a===O){var
c=id[1];fh[1]=h(ff[4],b,c,fh[1]);fi[1]=h(fg[4],c,b,fi[1]);id[1]++;return c}throw a}}function
bP(b){return a(fg[27],b,fi[1])}d0(dW);function
lo(a){return I(d0(fb(a)),xF,N)}function
xG(a){return I(d0(a),xH,N)}function
fj(a){return h(lm,function(c,b,a){return I(d0(c),b,a)},a,N)}function
d1(a){var
b=dX(xI);return ab(function(c,b,a){return dY(bP(b),a,c)},b,a)}function
ie(a,c){var
d=[0,b(a,xK)];return ab(function(g,f,e){var
i=bP(f),c=[0,b(a,xJ)],d=h(lg,function(d,c,a){var
e=b(B[3],c);return[4,[6,[1,b(B[1],d)],e],a]},i,c);return[2,[4,[0,b(a,e)],d],g]},d,c)}function
lp(c,b){try{var
a=e_(c,bP(b));return a}catch(a){a=q(a);if(a===O)return h(m[1],c,xL,b);throw a}}function
fk(b,a){return eM(lp,b,a)}function
cJ(a){return 0===b(d[25],a)?N:I(0,a,N)}function
xM(a){return eP(function(c,d){var
a=bP(c),b=lc(a);return b?b:fa(a)},a)}function
xN(e){var
b=kq(e),c=b[1],f=c[2],g=c[1];if(bL(b[2]))if(a(d[28],f,xO))return h$(bP(g));return 0}function
fl(g,f){var
i=d1(f),c=fb(g),b=dX(xw),d=[0,dX(xx),b];function
e(f,e,d){var
g=d[2],h=d[1],i=le(f,c),j=i[2],k=i[1];if(0===j)return[0,h,dY(f,e,g)];var
b=dW,a=j-1|0;for(;;){if(0===a)return[0,dY(ib(k,b),e,h),g];var
b=ib(b,c),a=a-1|0;continue}}var
a=h(al[13],e,i,d),j=a[1],k=fj(a[2]);return[0,fj(j),k]}function
lq(b,a){return ko(fl(b,a)[1])}function
lr(f,c){var
a=0;return ab(function(a,h,g){if(b(f,g)){var
d=h$(bP(h));if(d){var
e=d[1];return lq(e,c)?[0,e,a]:a}return a}return a},a,c)}function
ls(c,b){var
a=lr(c,b);return a?[0,h(g[20],f[1][4],a[1],a[2])]:0}function
bQ(b,a){var
c=d1(a);return fj(ll(d1(b),c))}function
ig(b,a){return a1(b,a)}function
xP(a){return ab(function(c,b,a){var
d=cJ(a);return ig(bQ(lo(b),d),c)},N,a)}function
ih(b){var
c=u[1];return ab(function(c,b,e){var
d=lf(bP(b));return a(u[7],d,c)},c,b)}function
xQ(e,b,c){var
f=u[1];function
i(c,b){var
d=ih(b[1]);return a(u[7],c,d)}var
d=h(g[20],i,f,c);function
j(b,f){function
c(a){return F(m[1],b,xR,a,e)}return a(u[13],c,d)}F(m[1],b,xS,j,d);function
k(c,a){var
d=a[1],e=dZ(a[2]);return Z(m[1],b,xT,c,fk,d,e)}a(g[16],k,c);return a(m[1],b,xU)}function
xV(a){var
b=da[1];return ab(function(a,d,e){var
b=bP(d),c=ld(b);return c?h(da[4],c[1],b,a):a},b,a)}function
aK(e,c){if(typeof
c==="number")return a(m[1],e,xW);else
switch(c[0]){case
0:return W(m[1],e,xX,aK,c[2],c[1]);case
1:return h(m[1],e,xY,c[1]);case
2:return h(m[1],e,xZ,c[1]);case
3:var
f=b(d[40],c[1]);return h(m[1],e,x0,f);case
4:var
g=d1(c[1]);return F(m[1],e,x1,li,g);case
5:var
i=c[2],j=d1(c[1]);return Z(m[1],e,x2,li,j,aK,i);case
6:var
k=c[2],l=b(n[33],c[1]);return W(m[1],e,x3,aK,k,l);case
7:return Z(m[1],e,x4,aK,c[1],aK,c[2]);case
8:return Z(m[1],e,x5,aK,c[1],aK,c[2]);default:return F(m[1],e,x6,aK,c[1])}}function
ii(c,b){if(typeof
b==="number")return a(m[1],c,x7);else{if(0===b[0])return g0(m[1],c,x8,b[1],aK,b[2],ii,b[3]);var
d=b[5],e=b[4],f=b[3],g=b[2],h=b[1],i=function(a,b){return j$(x9,ii,a,b)};return Iv(m[1],c,x_,h,aK,g,cA,f,aK,e,i,d)}}function
fm(c){var
b=c;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
1:return ya;case
2:return yb;case
3:return b[1];case
5:var
b=b[2];continue;case
6:var
e=[1,b[1]],f=fm(b[2]);return a(d[9],f,e);case
9:var
b=b[1];continue;case
4:break;default:var
g=b[1],h=fm(b[2]),i=fm(g);return a(d[1],i,h)}return x$}}function
db(e){var
b=e;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
0:var
b=b[2];continue;case
9:var
d=b[1],c=1;break;case
1:case
2:return b[1];case
5:case
6:var
d=b[2],c=1;break;case
7:case
8:var
g=b[1],h=db(b[2]),i=db(g);return a(f[6],i,h);default:var
c=0}if(c){var
b=d;continue}return-1}}function
fn(b){if(typeof
b==="number")return-1;else{if(0===b[0]){var
c=b[2],d=b[1],e=fn(b[3]),i=db(c),j=a(f[6],i,e);return a(f[6],d,j)}var
k=b[5],l=b[2],m=b[1],n=db(b[4]),o=db(l),p=a(f[6],o,n),q=a(f[6],m,p),r=function(c,b){var
d=fn(b);return a(f[6],c,d)};return h(g[20],r,q,k)}}function
bR(c,n){var
b=n;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
5:var
o=b[1],d=bR(c,b[2]);return[0,d[1],d[2],[5,o,d[3]]];case
6:var
e=bR(c,b[2]),g=e[2];return[0,[0,[0,g,e[3]],e[1]],g+1|0,[2,g]];case
7:var
p=b[2],h=bR(c,b[1]),q=h[3],r=h[1],i=bR(h[2],p),s=i[2],t=[7,q,i[3]];return[0,a(f[26],i[1],r),s,t];case
8:var
u=b[2],j=bR(c,b[1]),v=j[3],w=j[1],k=bR(j[2],u),x=k[2],y=[8,v,k[3]];return[0,a(f[26],k[1],w),x,y];case
9:var
l=bR(c,b[1]),m=l[2];return[0,[0,[0,m,l[3]],l[1]],m+1|0,[2,m]]}return[0,0,c,b]}}function
ij(c,a){if(typeof
a!=="number"&&9===a[0]){var
b=bR(c,a[1]);return[0,b[1],b[2],[9,b[3]]]}return bR(c,a)}function
lt(b){var
a=b;for(;;){if(typeof
a!=="number"&&9===a[0]){var
a=a[1];continue}return a}}function
dc(e){var
b=e;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
0:var
b=b[2];continue;case
9:var
d=b[1],c=1;break;case
1:case
2:return a(u[4],b[1],u[1]);case
5:case
6:var
d=b[2],c=1;break;case
7:case
8:var
f=b[1],g=dc(b[2]),h=dc(f);return a(u[7],h,g);default:var
c=0}if(c){var
b=d;continue}return u[1]}}function
ik(e,o){var
c=o;for(;;)if(typeof
c==="number")return[0,e,0];else{if(0===c[0]){var
d=c[2],l=c[1];if(typeof
d!=="number"&&6===d[0])if(typeof
c[3]==="number"){var
c=[0,l,d[2],0];continue}var
p=c[3],i=ij(e,d),q=i[3],r=i[1],m=ik(i[2],p),s=m[1],t=[0,l,q,m[2]],u=function(b,a){return[0,a[1],[9,a[2]],b]};return[0,s,h(g[20],u,t,r)]}var
v=c[5],w=c[4],x=c[3],y=c[1],j=ij(e,lt(c[2])),z=j[3],A=j[2],B=j[1],k=ij(A,lt(w)),C=k[3],D=k[2],E=k[1],F=function(a){return ik(D,a)},G=a(g[17],F,v),n=b(g[46],G),H=n[2],I=n[1],J=a(f[26],E,B),K=[1,y,z,x,C,H],L=function(b,a){return[0,a[1],[9,a[2]],b]},M=h(g[20],L,K,J);return[0,h(g[20],f[6],0,I),M]}}function
lu(d,c){function
e(c){if(typeof
c==="number")return[0,0,u[1]];else{if(0===c[0]){var
j=c[3],f=c[2],d=c[1];if(typeof
j==="number"){var
q=dc(f);return[0,c,a(u[4],d,q)]}var
k=e(j),i=k[2],l=k[1];if(a(u[3],d,i)){var
r=dc(f),s=a(u[7],r,i);return[0,[0,d,f,l],a(u[4],d,s)]}return[0,l,i]}var
m=c[4],n=c[2],o=c[1],t=c[3],v=a(g[17],e,c[5]),p=b(g[46],v),w=p[1],x=h(g[20],u[7],u[1],p[2]),y=dc(m),z=dc(n),A=a(u[7],z,y),B=a(u[7],A,x);return[0,[1,o,n,t,m,w],a(u[4],o,B)]}}return ik(d,e(c)[1])}function
lv(a){if(typeof
a==="number")return 4;else
switch(a[0]){case
0:return 0;case
1:return 1;case
2:return 2;case
3:return 3;case
4:return 5;case
5:return 6;case
6:return 7;case
7:return 8;case
8:return 9;default:return 10}}function
fo(f,e,c,b){var
g=b[2],h=c[2],d=a(f,c[1],b[1]);return 0===d?a(e,h,g):d}function
cK(h,g){var
c=h,b=g;for(;;){if(typeof
c==="number"){if(typeof
b==="number")return 0}else
switch(c[0]){case
0:if(typeof
b!=="number"&&0===b[0]){var
e=b[1],f=c[1],j=b[2],k=c[2];if(m5(f,e)){var
c=k,b=j;continue}return E.caml_string_compare(f,e)}break;case
1:if(typeof
b!=="number"&&1===b[0])return bk(c[1],b[1]);break;case
2:if(typeof
b!=="number"&&2===b[0])return bk(c[1],b[1]);break;case
3:if(typeof
b!=="number"&&3===b[0])return a(d[37],c[1],b[1]);break;case
4:if(typeof
b!=="number"&&4===b[0])return a(hI,c[1],b[1]);break;case
5:if(typeof
b!=="number"&&5===b[0])return fo(hI,cK,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
6:if(typeof
b!=="number"&&6===b[0])return fo(n[23],cK,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
7:if(typeof
b!=="number"&&7===b[0])return fo(cK,cK,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
8:if(typeof
b!=="number"&&7===b[0])return fo(cK,cK,[0,c[1],c[2]],[0,b[1],b[2]]);break;default:if(typeof
b!=="number"&&9===b[0]){var
c=c[1],b=b[1];continue}}var
i=lv(b);return bk(lv(c),i)}}function
fp(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[8,b,a];var
c=b}return c}function
dd(e,c){if(typeof
c!=="number")switch(c[0]){case
0:var
g=c[1];return[0,g,dd(e,c[2])];case
5:var
h=c[2];return[5,ar(e,c[1]),h]}var
f=b(d[25],e)+1|0;if(2<f>>>0)throw[0,a0,yc];switch(f){case
0:return[5,cJ(e),c];case
1:return 0;default:return a(d[32],yd,e)?c:[7,[3,e],c]}}function
lw(b,a){var
c=bo(b),d=c[1];return bL(c[2])?dd(d,a):[5,b,a]}function
lx(b,a){if(typeof
b!=="number")if(typeof
a!=="number"){if(typeof
b==="number")var
c=0;else
if(3===b[0])var
f=a,e=b[1],c=1;else
var
c=0;if(!c){if(typeof
a==="number")var
d=0;else
if(3===a[0])var
f=b,e=a[1],d=1;else
var
d=0;if(!d)return[7,b,a]}return dd(e,f)}return 0}var
cL=b(cy[1],[0,cK]);function
il(a){var
b=0;function
c(c,b,a){return fp(lw(b,c),a)}return h(cL[13],c,a,b)}function
de(e){var
b=e;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
5:var
g=b[1],i=de(b[2]),j=function(a){return bQ(g,a)};return a(cL[33],j,i);case
7:var
l=b[2],m=de(b[1]),d=de(l),c=il(m),n=il(d);if(typeof
c!=="number"&&3===c[0]){var
p=c[1],q=function(a){return ar(p,a)};return a(cL[33],q,d)}var
o=cJ(yg);return a(cL[6],[7,c,n],o);case
8:var
r=b[1],s=de(b[2]),t=de(r),u=function(e,b,a){if(b){var
c=b[1];if(a)return[0,ig(c,a[1])];var
d=c}else{if(!a)return 0;var
d=a[1]}return[0,d]};return h(cL[8],u,t,s);case
6:case
9:var
k=cJ(yf);return a(cL[6],b,k)}var
f=cJ(ye);return a(cL[6],b,f)}}function
yh(c,b){var
d=0;return ab(function(e,d,b){return fp(dd(b,a(t[27],d,c)),e)},d,b)}function
ly(a){if(a){var
b=a[1],c=ly(a[2]);return h(m[4],yi,b,c)}return yj}function
lz(k,g,j,a){function
c(q){var
a=q;for(;;)if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
a=a[2];continue;case
3:return[5,b(g,a[1])];case
4:return[1,b(k,ie(g,a[1]))];case
5:var
r=a[2],s=b(k,ie(g,a[1]));return[2,s,c(r)];case
7:var
t=a[1],u=c(a[2]);return[3,c(t),u];case
8:var
v=a[1],w=c(a[2]);return[4,c(v),w];case
1:case
2:var
l=a[1],e=0,d=j;for(;;){if(d){var
n=d[2];if(l!==d[1]){var
e=e+1|0,d=n;continue}var
i=e}else
var
o=ly(j),p=h(m[4],yk,l,o),i=b(f[3],p);return[0,b(B[4],i)]}default:return b(f[3],yl)}}return c(a)}function
fq(c,a){return lz(cX,function(a){var
c=ah(a);return b(B[2],c)},c,a)}function
fr(c,b){if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[3],d=b[2],f=b[1];if(typeof
d!=="number"&&9===d[0]){var
i=d[1],j=fr([0,f,c],e);return[1,fq(c,i),j]}var
h=fr([0,f,c],e);return[0,fq(c,d),h]}var
k=b[5],l=b[4],m=b[2],n=[0,b[1],c],o=function(a){return fr(n,a)},p=a(g[17],o,k),q=fq(c,l);return[2,fq(c,m),q,p]}}function
ym(b,a){return fr(b,lu(1+fn(a)|0,a)[2])}function
bt(e,u){var
c=u;for(;;)if(typeof
c==="number")return[0,N,1];else
switch(c[0]){case
0:var
c=c[2];continue;case
3:var
g=c[1],h=a(d[37],g,yn),v=0===h?1:1===h?2:b(f[3],yo);return[0,I(0,g,N),v];case
4:var
i=c[1];return[0,bQ(i,i),1];case
5:var
j=c[2],k=c[1],l=bt(e,j),n=l[2],o=l[1];if(0===n)return[0,bQ(k,o),0];var
w=dZ(n);Iw(m[1],f[28],yp,cA,k,aK,j,cA,o,w);return b(f[3],yq);case
6:var
x=c[1],p=bt(e,c[2]),y=p[2];return[0,c3([1,x],p[1]),y];case
7:var
z=c[2],q=bt(e,c[1]),A=q[2],B=q[1],r=bt(e,z),C=r[1],D=ic(A,r[2]);return[0,bQ(B,C),D];case
8:var
E=c[2],s=bt(e,c[1]),F=s[2],G=s[1],t=bt(e,E),H=t[1],J=fe(F,t[2]);return[0,ig(G,H),J];case
9:var
c=c[1];continue;default:return b(e,c[1])}}function
ys(o,n){var
d=o,c=n;for(;;)if(typeof
c==="number")return b(f[3],yt);else{if(0===c[0]){var
j=c[3],p=c[2],q=c[1],k=bt(function(c){return function(b){return a(t[27],b,c)}}(d),p),e=k[2],g=k[1],i=bo(g),l=i[1],r=bL(i[2])?1-a(fd(e),l,yr):0;if(r)return 1;if(0===j){var
s=dZ(e);W(m[1],f[28],yu,fk,g,s);return 0}var
d=h(t[4],q,[0,g,e],d),c=j;continue}var
u=c[4],v=c[2];bt(function(b){return a(t[27],b,d)},v);bt(function(b){return a(t[27],b,d)},u);return b(f[3],yv)}}function
yw(b,a){return[0,a[1],[0,b,a[2]]]}function
lA(c,a){var
b=a[1],d=a[2],e=b[1],f=dZ(b[2]);return g0(m[1],c,yx,fk,e,f,aK,d)}function
yy(c,b){var
d=h(m[1],c,yz,lA);return a(g[15],d,b)}var
lB=[aX,yA,aW(0)],yB=[0,[0,N,0],0];function
yC(a){return[0,[0,cJ(a),1],[3,a]]}function
yD(c){var
a=c[1],e=c[2],f=a[2],g=a[1];return[0,[0,I(0,b(d[3],a[3]),g),f],e]}function
fs(b,a){var
c=a[1],d=b[1],e=c[2],f=c[1],g=d[2],h=d[1],i=lx(b[2],a[2]),j=ic(g,e);return[0,[0,bQ(h,f),j],i]}function
ft(b,a){var
c=a[1],d=b[1],e=c[2],f=c[1],g=d[2],h=d[1],i=fp(b[2],a[2]),j=fe(g,e);return[0,[0,a1(h,f),j],i]}function
fu(b,e){var
f=e[2],g=e[1],c=g[2],h=g[1];if(0===c){var
k=lw(b,f);return[0,[0,bQ(b,h),c],k]}var
i=bo(b),j=i[1];if(bL(i[2]))if(a(d[28],j,yE)){var
l=dd(j,f);return[0,[0,bQ(b,h),c],l]}throw lB}function
yF(g){var
h=g[2],i=g[1],j=i[2],k=i[1],l=bo(k),e=l[1],c=hL(l[2]);if(!a(n[24],n[2],c))if(!a(d[26],e,yG)){var
p=n[2],q=bd(e);if(a(n[24],q,p)){var
m=a(d[9],e,[1,c]),o=b(d[22],m);if(a(d[26],m,o))return 0;switch(j){case
0:return[0,[0,[0,I(0,yH,N),0],[6,c,h]]];case
1:return[0,[0,[0,I(0,o,c3([1,c],k)),j],[6,c,h]]];default:return b(f[3],yI)}}}return 0}function
yJ(f){var
c=bo(f),a=c[1];if(bL(c[2])){var
e=b(d[25],a),g=0===e?yK:1===e?[0,1,2,[3,a]]:[0,0,2,[3,b(d[3],a)]];return[0,g]}return 0}function
im(d,c){var
b=yJ(c);if(b)return[0,b[1]];try{var
k=function(a){return c1(c,a[1][1])},f=a(g[33],k,d),l=[0,[0,1,f[1][2],f[2]]];return l}catch(b){b=q(b);if(b===O){var
h=c4(c);try{var
i=function(a){return c1(h,a[1][1])},e=a(g[33],i,d),j=[0,[0,0,e[1][2],e[2]]];return j}catch(a){a=q(a);if(a===O)return 0;throw a}}throw b}}function
io(f,a){var
b=a[2],c=a[1],d=c[2],e=c[1];return f?[0,[0,e,d],b]:[0,[0,c4(e),d],b]}function
fv(q,I,H,G){var
J=G[1],K=I[1],j=[0,[0,K[1],K[2]],I[2]],i=[0,[0,J[1],J[2]],G[2]];for(;;){var
m=i[2],s=i[1],d=s[2],k=s[1],e=j[2],t=j[1],a=t[2],c=t[1],l=fl(H,c)[1],g=fl(H,k)[1];if(bL(g))var
h=[0,[0,[0,k,d],m]];else
if(0===a){if(0===d){var
M=fu(c4(g),[0,[0,c,a],e]),u=ft(fu(l,[0,[0,k,d],m]),M),v=u[1],j=[0,[0,c,a],e],i=[0,[0,v[1],v[2]],u[2]];continue}var
w=im(q,l);if(w){var
n=w[1],x=n[1],N=io(x,[0,[0,l,n[2]],n[3]]),O=x?c4(g):g,P=fu(O,[0,[0,c,a],e]),y=ft(fs(N,[0,[0,k,d],m]),P),z=y[1],j=[0,[0,c,a],e],i=[0,[0,z[1],z[2]],y[2]];continue}var
h=0}else
if(0===d)var
h=b(f[3],yL);else{var
A=im(q,l),B=im(q,g);if(A)if(B){var
o=B[1],C=o[1],p=A[1],D=p[1],Q=o[3],R=o[2],S=p[3],T=p[2];if(D!==C){var
U=fs(io(C,[0,[0,g,R],Q]),[0,[0,c,a],e]),E=ft(fs(io(D,[0,[0,l,T],S]),[0,[0,k,d],m]),U),F=E[1],j=[0,[0,c,a],e],i=[0,[0,F[1],F[2]],E[2]];continue}var
h=0,r=1}else
var
r=0;else
var
r=0;if(!r)var
h=0}if(h){var
L=h[1],V=L[1];return[0,[0,V,il(de(L[2]))]]}return 0}}function
fw(e,c){var
b=c[1],f=b[2],g=b[1];function
h(b){if(e){var
c=a(d[26],b,yM);return c?c:a(d[26],b,yN)}return 1}return 0===f?ls(h,g):0}function
yO(a){var
f=1,b=bJ(function(a){return fw(f,a)},a),c=b[1],g=b[2];if(c){var
d=c[1],h=d[2],i=d[1],e=eF(function(b){return fv(a,h,i,b)},g);return e?e[1]:a}return a}function
yP(a){return eH(function(e){var
f=1,b=bJ(function(a){return fw(f,a)},e),c=b[1],g=b[2];if(c){var
d=c[1],h=d[2],i=d[1];return eF(function(b){return fv(a,h,i,b)},g)}return 0},a)}var
az=[0,lB,yw,yD,lA,yy,yB,yC,fs,ft,fu,yF,fv,yP,yO,function(c,b){function
d(a){return fw(c,a)}return kf(d,function(d,c){var
e=c[1],f=e[1],g=d[1],h=c[2],i=e[2],j=d[2],k=ih(f);return a(u[3],g,k)?fv(b,j,g,[0,[0,f,i],h]):0},b)},fw],V=[0,fm,db,fn,lu,aK,ii,fp,dd,lx,ym,lz,yh,bt,ys],am=[0,[0,xE,bP,d0],fj,lo,ie,xG,xP,ih,xN,xM,lq,cJ,ls,lr,bQ,fl,xV,lp,fk,xQ],bu=[0,dX,xs,lk,ll,xv,lj,lm,dY],fx=[0,lg,dW,fa,fb,ld,lc,h$,le,e$,lf];aE(1003,[0,wH,h_,fx,da,bu,fd,fe,ln,fc,am,V,xC,ic,az],"Micromega_plugin__Polynomial");function
ip(c,b){var
a=b[2];return a?c===a[1]?1:0:0}function
d2(b,a){var
c=a[1]<=b?1:0,d=c?1-ip(b,a):c;return d}function
fy(a){return[0,a,0]}function
lC(b,a){return[0,a[1],[0,b]]}function
yR(c,b){return eP(function(e,b){return a(d[28],b,yS)?0:d2(e,c)},b)}function
yT(c,b){try{var
a=bo(b),d=a[1],e=yR(c,a[2])?[0,d]:0;return e}catch(a){a=q(a);if(a===O)return 0;throw a}}function
iq(e,d,c){try{var
g=a(t[27],d,c);return g}catch(a){a=q(a);if(a===O)return b(f[3],e);throw a}}function
lD(j,e,c){var
k=iq(y6,e,j),i=ay(c,k);if(a(d[26],i,yY))var
g=b(f[3],yZ);else
var
l=a(d[9],y0,i),g=ar(l,I(e,y2,I(c,y1,k)));var
o=a(t[7],e,j);function
m(b){var
e=ay(c,b);return a(d[26],e,y3)?b:hH(e,g,y5,I(c,y4,b))}var
n=a(t[33],m,o);return h(t[4],c,g,n)}function
y8(b,a){return 0}var
y9=[0,b(t[10],y8)];b(hy[1],y9);function
ir(D,g,k,C){var
c=C;for(;;){if(!D){var
G=dL(a(t[27],g,c));if(a(d[30],G,y$))return[0,c,0]}var
r=iq(yX,g,c),s=yT(k,r);if(s)var
e=[0,[0,s[1]]];else{var
p=bo(r)[2];for(;;){var
q=a2(p);if(q){var
l=q[1],m=l[1],x=l[3];if(a(d[27],l[2],yU)){if(d2(m,k)){var
p=x;continue}var
j=[0,m,-1]}else
var
j=[0,m,1]}else
var
j=b(f[3],yV);var
n=j[1],z=j[2],A=a(t[7],g,c),y=0,v=a(t[39],k[1],A),w=function(p,q){return function(e,g,c){if(ip(e,k))return c;var
j=ay(q,g),m=a(d[6],[0,p],j);if(a(d[27],m,yW)){var
n=dL(g),o=a(d[9],n,j),f=b(d[15],o);if(c){var
h=c[1],i=h[2],l=h[1];return a(d[27],i,f)?c:a(d[27],f,i)?[0,[0,e,f]]:cq(l,e)?c:[0,[0,e,f]]}return[0,[0,e,f]]}return c}}(z,n),o=h(t[13],w,v,y);if(o)var
u=o[1],e=[1,u[1],n,u[2]];else
var
e=[0,[1,n]];break}}if(0===e[0]){var
i=e[1];if(typeof
i==="number")throw[0,lE,y_];else{if(0===i[0])return[0,c,i];var
E=i[1],B=dL(a(t[27],g,c)),F=a(d[30],B,y7)?c:lD(c,g,E);return[0,F,i]}}var
c=lD(c,e[1],e[2]);continue}}function
is(d,e,c){var
b=ab(function(d,c,b){try{var
f=a1(ar(b,a(t[27],c,e)),d);return f}catch(a){a=q(a);if(a===O)return a1(I(c,b,N),d);throw a}},N,c);return h(t[4],d,b,e)}function
lF(j,b,i,h,g){var
f=ir(j,b,h,is(b,g,i)),c=f[2],e=f[1];return typeof
c==="number"?[1,e,0]:0===c[0]?a(d[30],c[1],za)?[1,e,0]:[0,I(b,ze,I(0,zd,ar(zc,iq(zb,b,e))))]:[1,e,[0,c[1]]]}function
fz(d){try{var
e=u[1],f=function(c,b){var
d=hJ(b[1]);return a(u[7],c,d)},i=h(g[20],f,e,d),j=b(u[24],i),c=j}catch(a){a=q(a);if(a!==O)throw a;var
c=0}return 1+c|0}function
fA(m,l){var
c=0,a=m,g=t[1],f=l,e=0;for(;;){if(f){var
j=f[2],i=f[1];switch(i[2]){case
0:var
n=i[1],k=I(0,b(d[3],i[3]),n),o=ar(zf,k),p=h(t[4],a+1|0,[0,c,0],g),q=h(t[4],a,[0,c,1],p),c=c+1|0,r=[0,[0,a,k],[0,[0,a+1|0,o],e]],a=a+2|0,g=q,f=j,e=r;continue;case
1:var
s=i[1],u=[0,[0,a,I(0,b(d[3],i[3]),s)],e],v=h(t[4],a,[0,c,1],g),c=c+1|0,a=a+1|0,g=v,f=j,e=u;continue;default:throw fc}}return[0,a,g,e]}}function
it(c,a){function
b(b,d,a){return d2(b,c)?a:I(b,dL(d),a)}return h(t[13],b,a,N)}function
fB(E,D,f,C){var
h=D,g=C;for(;;){var
n=I(0,zg,it(f,g));if(h){var
t=h[1],u=t[2],A=h[2],B=t[1],c=A,o=eQ(n,u),e=[0,B,u],b=0;for(;;){var
j=e[2],k=e[1];if(c){var
p=c[2],q=c[1],l=q[2],r=q[1],s=eQ(n,l);if(a(d[29],s,o)){var
c=p,o=s,e=[0,r,l],b=[0,[0,k,j],b];continue}var
c=p,e=[0,k,j],b=[0,[0,r,l],b];continue}var
m=[0,[0,[0,k,j],b]];break}}else
var
m=0;if(m){var
v=m[1],w=v[2],x=v[1],y=x[1],F=x[2],i=lF(E,y,F,lC(y,f),g);if(0===i[0])return[1,i[1]];var
z=i[1],G=i[2];if(w){var
h=w,g=z;continue}return[0,[0,f,z,G]]}return[0,[0,f,g,0]]}}function
lG(c){var
e=fz(c),f=fA(e,c),h=f[3],j=f[2],i=t[1],g=fB(0,h,fy(e),i);if(0===g[0])return 0;var
k=g[1];return[0,c5(ab(function(g,f,c){var
e=a(t[27],f,j),h=e[1],i=e[2]?c:b(d[3],c);return I(h,i,g)},N,k))]}function
lH(a){var
b=fz(a),e=fA(b,a)[3],f=t[1],c=fB(0,e,fy(b),f);if(0===c[0]){var
d=c[1];return[0,it(d[1],d[2])]}return 0}function
lI(e,c){var
a=fz(c),k=fA(a+1|0,c)[3];function
f(f,e){var
a=e[2];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=a[1],g=f?c:b(d[3],c);return[0,g]}return 0}}var
l=t[1],g=fB(0,k,fy(a),l);if(0===g[0]){var
h=g[1],i=h[2],j=h[1],m=f(1,ir(1,a,j,is(a,i,e)));return[0,[0,f(0,ir(1,a,j,is(a,i,c4(e)))),m]]}return 0}function
fC(c){var
e=b(d[22],c);return a(d[4],c,e)}var
lJ=[aX,zh,aW(0)];function
zi(c,d,b){var
e=I(c,zj,N);try{var
f=function(a,f){var
b=d2(a,d);if(b){if(c1(e,f))throw[0,lJ,a];var
c=0}else
var
c=b;return c};a(t[12],f,b);var
g=0;return g}catch(a){a=q(a);if(a[1]===lJ)return[0,a[2]];throw a}}function
lK(i,z,n,r,h,m,f){var
j=f[2],k=f[1],c=fC(bo(j)[1]);if(a(d[26],c,zm))return 0;var
o=a(d[9],zo,zn);if(a(d[27],c,o))var
e=a(d[9],zp,c),p=b(d[20],e)?a(d[4],e,zq):b(d[22],e),l=p;else
var
l=zw;function
s(e){var
b=fC(e),f=a(d[4],zr,c);if(a(d[29],b,f)){var
g=a(d[4],zs,c);return a(d[9],b,g)}var
h=a(d[4],zt,b);return a(d[9],h,c)}var
u=[0,s,[0,function(b){return fC(a(d[6],l,b))},0]];function
v(n){var
k=I(0,zk,j),c=b(kt(function(e,c){var
b=1-d2(e,h);if(b){var
f=fC(c);return a(d[31],f,zl)}return b}),k),e=c[2],f=c[1],g=bL(f)?[0,e]:[0,ab(function(a,d,c){var
b=zi(d,h,m);return b?I(b[1],c,a):a},e,f)];if(g)var
l=g[1],i=ab(function(d,c,a){return I(c,b(n,a),d)},N,l);else
var
i=N;return c5(i)}var
w=a(g[17],v,u);function
x(e){var
c=az[6];return ab(function(h,e,c){try{var
g=a(t[27],e,r),l=g[1],m=g[2]?c:b(d[3],c),n=a(t[27],l,i),o=hG(m),p=a(az[10],o,n),f=p}catch(b){b=q(b);if(b!==O)throw b;var
j=a(t[27],e,i),k=hG(c),f=a(az[10],k,j)}return a(az[9],h,f)},c,e)}var
y=a(g[17],x,w);return kc(function(g){var
d=b(az[11],g);if(d){var
e=d[1],f=e[2],c=e[1];if(0===c[2])return[0,[0,k,[0,c,f]]];var
h=I(0,zu,n),i=eQ(c[1],h);return a(fd(1),i,zv)?0:[0,[0,k,[0,c,f]]]}return 0},y)}function
lL(l){var
e=b(g[46],l)[1],i=fz(e),c=fA(i,e),k=c[2],p=c[3],u=c[1],v=a(g[17],az[3],l),n=[0,0,t[1]];function
o(a,c){var
b=a[1];return[0,b+1|0,h(t[4],b,c,a[2])]}var
r=[0,0],w=h(g[20],o,n,v)[2];function
s(g,u,c,l){r[1]++;if(0===l[0]){var
v=l[1],e=v[2],j=v[1],w=it(j,e);if(0===(r[1]%2|0))var
H=0,I=function(c,b,a){return a?[0,a[1]]:lK(g,u,w,k,j,e,[0,c,b])},n=h(t[13],I,e,H);else
var
J=0,K=function(t,s,c){var
h=lK(g,u,w,k,j,e,[0,t,s]);if(h){var
f=h[1];if(c){var
i=c[1],l=i[2],m=l[2],n=l[1],o=f[2],p=o[2],q=o[1],v=n[2],x=n[1],y=i[1],z=q[2],A=q[1],B=f[1],C=b(V[1],m),D=b(V[1],p),E=a(d[27],D,C)?[0,B,[0,[0,A,z],p]]:[0,y,[0,[0,x,v],m]];return[0,E]}var
r=f}else{if(!c)return 0;var
r=c[1]}return[0,r]},n=h(t[13],K,e,J);if(n){var
x=n[1],y=x[2],z=y[2],A=y[1],B=A[2],C=A[1],N=x[1];if(0===B)return[0,[0,c,[9,z],0]];var
o=lC(c,j),i=lF(1,c,C,o,e);if(0===i[0])var
D=[1,i[1]];else{var
L=i[2],M=i[1];if(ip(c,o))var
p=[0,o[1],0];else
var
F=a(m[4],yQ,c),p=b(f[3],F);var
D=[0,[0,p,M,L]]}var
E=s(h(t[4],c,[0,[0,C,B],[2,c]],g),[0,N],c+1|0,D);return E?[0,[0,c,[9,z],E[1]]]:0}return 0}var
P=0,Q=c5(l[1]),G=0;return[0,[0,c,ab(function(i,e,c){try{var
h=a(t[27],e,k),l=h[2],m=a(t[27],h[1],g)[2],n=l?c:b(d[3],c),o=a(V[8],n,m),f=o}catch(b){b=q(b);if(b!==O)throw b;var
j=a(t[27],e,g)[2],f=a(V[8],c,j)}return a(V[7],i,f)},G,Q),P]]}var
x=t[1],j=s(w,0,u,fB(1,p,fy(i),x));return j?[0,j[1]]:0}aE(1005,[0,lI,lH,lG,lL],"Micromega_plugin__Simplex");function
fD(p){var
c=b(bn[25],p),l=[aX,zx,aW(0)],m=[aX,zy,aW(0)];function
r(a,c){try{var
d=b(a,0);b(c,0);return d}catch(a){a=q(a);try{b(c,0)}catch(b){throw a}throw a}}function
s(a){try{var
c=[0,b(eL[3],a)];return c}catch(a){a=q(a);if(a===kJ)return 0;if(b(df[18],a))throw l;throw a}}function
t(c,a){var
d=h(R[34],a,0,1);try{h(R[34],a,0,0);var
e=0===c?4:1;h(R[83],a,e,1);var
f=1,b=f}catch(a){a=q(a);if(a[1]!==R[1])throw a;var
b=0}h(R[34],a,d,0);return b}function
u(a){var
c=h(R[34],a,0,1);try{h(R[34],a,0,0);var
b=h(R[83],a,0,1);return b}catch(b){b=q(b);if(b[1]===R[1]){h(R[34],a,c,0);return 0}throw b}}function
g(d,c,a){return t(d,c)?r(a,function(a){return u(c)}):b(a,0)}function
d(i){var
e=h(R[23],i,zz,i_),j=b(R[30],e),d=b(c[1],100);function
n(e){for(;;){var
a=s(j);if(a){var
b=a[1];h(c[10],d,b[1],b[2]);continue}return 0}}try{g(0,e,n);b(f[83],j);var
o=h(R[23],i,zC,i_),p=[0,b(R[31],o),1,d];return p}catch(e){e=q(e);if(e===l){b(f[83],j);var
m=h(R[23],i,zA,i_),k=b(R[31],m);g(1,m,function(g){function
e(b,a){return h(eL[1],k,[0,b,a],zB)}a(c[12],e,d);return b(f[52],k)});return[0,k,1,d]}throw e}}function
n(a,i,e){var
d=a[1],j=a[3];if(0===a[2])throw m;var
k=b(R[33],d);h(c[10],j,i,e);return g(1,k,function(a){h(eL[1],d,[0,i,e],zD);return b(f[52],d)})}function
o(b,d){var
e=b[3];if(0===b[2])throw m;return a(c[7],e,d)}return[0,d,o,n,function(c,f){var
a=[e,function(b){try{var
a=[0,d(c)];return a}catch(a){return 0}}];return function(c){var
d=j(a),g=k===d?a[1]:e===d?b(i[2],a):a;if(g){var
h=g[1];try{var
m=o(h,c);return m}catch(a){a=q(a);if(a===O){var
l=b(f,c);n(h,c,l);return l}throw a}}return b(f,c)}}]}aE(1008,[0,fD],"Micromega_plugin__Persistent_cache");function
lM(c,e){var
g=e[2],i=e[1];if(i){var
j=b(d[40],i[1]);h(m[1],c,zE,j)}else
a(f[55],c,zI);a(f[55],c,zF);if(g){var
k=b(d[40],g[1]);return h(m[1],c,zG,k)}return a(f[55],c,zH)}function
iu(b){var
c=b[1];if(c){var
e=b[2];if(e)return a(d[29],c[1],e[1])?[0,b]:0}return[0,b]}function
iv(c,b){var
f=b[2],g=b[1],h=c[2],i=c[1];function
e(d,c,b){if(c){var
e=c[1];return b?[0,a(d,e,b[1])]:c}return b?b:0}var
j=e(d[39],h,f);return iu([0,e(d[38],i,g),j])}function
fE(c){var
e=c[1];if(e){var
f=c[2];if(f){var
g=f[1],h=b(d[24],e[1]),i=b(d[22],g),j=a(d[4],i,h);return[0,a(d[1],j,zJ)]}}return 0}function
lN(f,e){var
b=fE(f),c=fE(e);return b?c?a(d[29],b[1],c[1]):1:0}function
iw(e,b){var
c=e[2],f=e[1];if(f){var
g=f[1];if(c){var
i=c[1],h=a(d[29],g,b);return h?a(d[29],b,i):h}return a(d[29],g,b)}return c?a(d[29],b,c[1]):1}aE(1009,[0,lM,iv,fE,lN,iw,iu],"Micromega_plugin__Itv");var
d3=b(o[20][1],[0,bk]),aT=b(bn[25],[0,c1,km]),d4=[aX,zK,aW(0)];function
d5(b,a){switch(a[0]){case
0:return h(m[1],b,zL,a[1]);case
1:return g0(m[1],b,zM,a[1],d5,a[2],d5,a[3]);default:return Z(m[1],b,zN,d5,a[1],d5,a[2])}}function
zO(b,a){var
c=b[4],d=b[3],f=a[4],g=a[2],h=a[1],i=b[2],j=b[1];if(d===a[3])if(c===f){var
e=iv(j,h);return e?[0,[0,e[1],[2,i,g],d,c]]:0}throw[0,a0,zP]}function
zQ(e,b,d){try{var
c=a(aT[7],d,e),f=zO(b,c[1]);if(f){c[1]=f[1];var
g=0;return g}throw[0,d4,[2,b[2],c[1][2]]]}catch(a){a=q(a);if(a===O)return h(aT[10],d,e,[0,b]);throw a}}var
fF=[aX,zR,aW(0)];function
ix(d,c,a){var
e=h_[1];if(b(aT[15],a)<e)return zQ(d,c,a);throw fF}function
iy(i,c){var
j=iu(c[1]);if(j){var
k=j[1],f=k[2],g=k[1],l=a2(i);if(l){var
h=l[1][2],e=function(b){return a(d[9],b,h)};if(1===b(d[25],h))var
n=c[4],o=c[3],p=c[2],q=a(a4[16],e,f),m=[0,[0,a(a4[16],e,g),q],p,o,n];else
var
r=c[3],s=c[4],t=c[2],u=a(a4[16],e,g),m=[0,[0,a(a4[16],e,f),u],t,s,r];return[0,c3(h,i),m]}return iw([0,g,f],zS)?1:0}return 0}function
lO(a){return ab(function(a,h,g){var
c=a[2],e=a[1],f=b(d[25],g);if(0===f)throw[0,a0,zU];return 1===f?[0,e,c+1|0]:[0,e+1|0,c]},zT,a)}function
lP(a,f){var
b=a[3],c=a[1],g=a[2],d=lO(c),h=d[2],i=d[1],j=[0,f];switch(g){case
0:var
e=[0,[0,b],[0,b]];break;case
1:var
e=[0,[0,b],0];break;default:throw fc}return iy(c,[0,e,j,h,i])}function
zV(d){var
c=b(aT[1],1000);function
e(b,a){return[0,a,b]}var
f=a(o[17][13],e,d),g=d3[1];function
i(e,d){var
f=d[2],g=d[1],b=lP(g,f);if(typeof
b==="number"){if(0===b)throw[0,d4,[0,f]];return e}ix(b[1],b[2],c);var
h=g[1];return ab(function(c,b,d){return a(d3[4],b,c)},e,h)}return[0,c,h(o[17][15],i,g,f)]}function
lQ(a){var
b=a[1],c=0;function
d(c,b,a){return[0,[0,c,b[1]],a]}return h(aT[14],d,b,c)}function
iz(e,c){var
f=c[2],g=e[2],i=c[1],j=e[1];if(a(d[31],g,zW))if(a(d[31],f,zX)){var
h=a(d[9],zY,f),b=hH(a(d[9],zZ,g),j,h,i);return[0,b,lO(b)]}throw[0,a0,z0]}function
z1(i,c){var
g=c[1];function
j(k,q,j){var
a=q[1],f=j[3],g=j[2],h=j[1],c=ay(i,k);if(0===c[0])if(0===c[1])return[0,h,[0,[0,k,a],g],f];function
e(d,b){return b?[0,[0,c,k,[0,[0,[0,b[1]],0],a[2],a[3],a[4]]],d]:d}var
l=a[1],m=l[2],n=l[1];if(1===b(d[25],c)){var
o=e(f,m);return[0,e(h,n),g,o]}var
p=e(f,n);return[0,e(h,m),g,p]}var
e=h(aT[14],j,g,z2),k=e[3],l=e[2],m=e[1],n=b(aT[15],c[1]),f=b(aT[1],n);function
p(a){return h(aT[10],f,a[1],[0,a[2]])}a(o[17][11],p,l);function
q(e){function
c(g){var
h=g[3],j=g[1],k=e[3],l=e[1],p=g[2],q=e[2],r=h[1],s=b(a4[7],k[1][1]),t=b(a4[7],r[1]),u=b(d[3],j),v=a(d[9],t,u),w=a(d[9],s,l),x=a(d[1],w,v),m=iz([0,q,l],[0,p,b(d[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,i,k[2],h[2]],n[2],n[1]],c=iy(m[1],o);if(typeof
c==="number"){if(0===c)throw[0,d4,o[2]];return 0}return ix(c[1],c[2],f)}return a(o[17][11],c,k)}a(o[17][11],q,m);return[0,f,a(d3[6],i,c[2])]}function
z4(e,q,C,B,c){var
r=ay(e,q),f=b(aT[15],c[1]),s=b(aT[1],f),g=c[1];function
h(g,D){var
h=D[1],c=ay(e,g);if(0===c[0])if(0===c[1])var
i=[0,g,h],j=1;else
var
j=0;else
var
j=0;if(!j)var
k=a(d[30],c,z3)?b(d[3],r):r,l=b(d[15],c),m=iz([0,q,k],[0,g,l]),n=m[2],u=n[2],v=n[1],w=m[1],x=a(d[9],C,k),o=function(b){var
c=a(d[9],b,l);return a(d[1],x,c)},p=h[1],y=p[1],z=a(a4[16],o,p[2]),A=[0,a(a4[16],o,y),z],i=[0,w,[0,A,[1,e,B,h[2]],u,v]];var
t=i[2],f=iy(i[1],t);if(typeof
f==="number"){if(0===f)throw[0,d4,t[2]];return 0}return ix(f[1],f[2],s)}a(aT[12],h,g);return[0,s,a(d3[6],e,c[2])]}var
J=b(z5[1],[0,bk]);function
lR(x,w,c){function
e(o,n,y){var
t=[0,z6,N],i=ab(function(e,c,b){var
f=e[2],g=e[1];try{var
h=a(J[23],c,x),i=a(d[6],h,b),j=[0,a(d[1],g,i),f];return j}catch(a){a=q(a);if(a===O)return[0,g,I(c,b,f)];throw a}},t,o),p=i[2],e=i[1],r=ay(w,p),g=n[1][1];function
c(b){var
c=a(d[4],b,e);return a(d[9],c,r)}var
j=g[2],k=g[1],l=b(d[25],r);if(0===l)var
h=iw(g,e)?z7:b(f[3],z8);else
if(1===l)var
u=a(a4[16],c,j),h=[0,a(a4[16],c,k),u];else
var
v=a(a4[16],c,k),h=[0,a(a4[16],c,j),v];var
s=iv(y,h);if(s)return s[1];var
z=b(d[40],e);g0(m[1],f[28],z_,cA,o,z,cA,p);F(m[1],f[28],z$,lM,n[1][1]);return b(f[3],Aa)}return h(aT[14],e,c,z9)}function
Aj(g,k,j,d,c){function
e(c,f){var
l=b(k,c);try{var
r=function(a){return a[1][1]!==g?1:0},d=a(o[17][27],r,l)[1],i=d[1],s=e(z4(i,d[2],d[3],d[4],c),[0,[0,i,c],f]);return s}catch(d){d=q(d);if(d===O){var
m=b(j,c);try{var
n=function(a){return a[1]!==g?1:0},h=a(o[17][27],n,m)[1],p=e(z1(h,c),[0,[0,h,c],f]);return p}catch(a){a=q(a);if(a===O)return[0,[0,c,f]];throw a}}throw d}}return e(d,c)}function
lS(d,c,b,a){try{var
e=Aj(d,c,b,zV(a),0);return e}catch(a){a=q(a);if(a[1]===d4)return[1,a[2]];throw a}}function
lT(c){var
e=c[2],f=[0,0,lQ(c)];function
g(x,w){var
e=w[2],c=0,g=0,i=0,f=0,G=w[1];for(;;){if(e){var
j=e[2],m=e[1],a=m[2],n=m[1],p=a2(n);if(p){var
l=p[1],q=l[3],y=l[2];if(x===l[1]){var
k=function(b){return function(a,c){return c?[0,b[4]+b[3]|0,a]:a}}(a),r=a[1],s=r[2],t=r[1];if(1===b(d[25],y)){var
z=k(f,s),e=j,c=[0,[0,q,a],c],g=k(g,t),f=z;continue}var
A=k(f,t),e=j,c=[0,[0,q,a],c],g=k(g,s),f=A;continue}var
e=j,c=[0,[0,n,a],c],i=(a[4]+a[3]|0)+i|0;continue}var
e=j,c=[0,[0,N,a],c],i=(a[4]+a[3]|0)+i|0;continue}var
u=b(o[17][1],g),B=0,C=function(b,a){return b+a|0},D=h(o[17][15],C,B,g),v=b(o[17][1],f),E=0,F=function(b,a){return b+a|0};return[0,[0,[0,x,i+v*D+u*h(o[17][15],F,E,f)-v*u],G],c]}}var
i=h(d3[15],g,e,f)[1];function
j(b,a){return E.caml_float_compare(b[2],a[2])}return a(o[17][39],j,i)}function
lU(b){var
c=b[1];if(c){var
e=b[2];if(e)return a(d[26],c[1],e[1])}return 0}function
lV(b,g){var
a=g;for(;;){var
c=a2(a);if(c){var
d=c[1],e=d[3],f=d[1];if(f===b)return[0,1,e];if(f<b){var
a=e;continue}return[0,0,a]}return[0,0,N]}}function
lW(I){var
l=lQ(I),J=0;function
K(c,e){var
b=e[2],f=b[1],g=f[1],j=e[1];if(g){var
h=f[2];if(h){var
i=g[1];return a(d[26],i,h[1])?[0,[0,j,i,b[2],b[4]+b[3]|0],c]:c}}return c}var
m=h(o[17][15],K,J,l),b=m;for(;;){if(b){var
n=b[2],c=b[1],p=c[1],x=c[4],y=c[3],z=c[2],q=a2(p);if(!q){var
b=n;continue}var
r=q[1],A=r[1];if(!bL(r[3])){var
b=n;continue}var
k=[0,[0,A,p,z,y,x]]}else
var
k=0;if(k)var
g=[0,k[1]];else{var
e=m;a:for(;;){if(e){var
f=e[1],w=f[1],s=w,E=e[2],F=f[4],G=f[3],H=f[2];for(;;){var
t=a2(s);if(t){var
u=t[1],v=u[1],D=u[3],B=0,C=function(d){return function(a,b){var
c=b[2];return lV(d,b[1])[1]?lU(c[1])?a+1|0:a:a}}(v);if(2!==h(o[17][15],C,B,l)){var
s=D;continue}var
j=[0,v]}else
var
j=0;if(!j){var
e=E;continue a}var
g=[0,[0,j[1],w,H,G,F]];break}}else
var
g=0;break}}if(g){var
i=g[1];return[0,[0,[0,i[1],i[2],i[3],i[4]],0],0]}var
L=0,M=function(u,e){var
r=e[1],n=r,m=l,i=u,v=e[4],w=e[3],x=e[2];b:for(;;){var
o=a2(n);if(o){var
p=o[1],q=p[1],c=m,b=0,a=0,s=p[3],t=v-1|0;for(;;){if(c){var
f=c[2],j=c[1],d=j[2],g=d[3]+d[4]|0,k=lV(q,j[1]),h=k[2];if(0===k[1]){var
c=f,b=b+g|0,a=[0,[0,h,d],a];continue}if(lU(d[1])){var
c=f,b=b+g|0,a=[0,[0,h,d],a];continue}var
c=f,b=(b+g|0)+t|0,a=[0,[0,h,d],a];continue}var
n=s,m=a,i=[0,[0,[0,q,r,x,w],b],i];continue b}}return i}},N=h(o[17][15],M,L,m),O=function(b,a){return bk(b[2],a[2])};return a(o[17][39],O,N)}}function
Ak(g,d){var
i=0;function
j(c,b){var
d=kp(b[1]);return a(f[1][5],c,d)}var
c=h(o[17][15],j,i,d),e=lS(c,lW,lT,[0,[0,I(c,Am,g),0,Al],d]);if(0===e[0]){var
k=e[1][1];try{var
n=[0,lR(J[1],c,k[1])];return n}catch(c){c=q(c);if(b(df[18],c)){var
l=b(eG[1],c);a(m[2],An,l);return 0}throw c}}return 0}function
Ao(w){var
j=lS(f[8],lW,lT,w);if(0===j[0]){var
i=j[1][2],g=J[1];for(;;){if(i){var
r=i[1],s=r[1],x=i[2],k=lR(g,s,r[2][1]),m=k[1];if(m){var
n=k[2],c=m[1];if(n){var
o=n[1];if(a(d[29],c,Ab))if(a(d[29],Ac,o))var
e=Ad,l=1;else
var
l=0;else
var
l=0;if(!l)var
t=b(d[22],o),u=b(d[24],c),e=a(d[29],u,t)?b(d[24],c):c}else
var
e=a(d[29],c,Ae)?Af:b(d[24],c)}else{var
p=k[2];if(p)var
q=p[1],v=b(d[22],q),e=a(d[29],Ag,v)?Ah:b(d[22],q);else
var
e=Ai}var
i=x,g=h(J[4],s,e,g);continue}var
y=function(c,b,a){return I(c,b,a)};return[0,h(J[12],y,g,N)]}}return[1,j[1]]}function
cM(b,a){return iz(b,a)[1]}function
fG(b,a){if(0===b)if(0===a)return 0;return 1}function
lX(r,q,p){var
j=p[2],k=p[1],l=q[2],m=q[1],n=j[3],f=j[2],g=j[1],o=l[3],h=l[2],i=l[1],c=ay(r,i),e=ay(r,g),U=0===c[0]?0===c[1]?1:0:0;if(!U){var
V=0===e[0]?0===e[1]?1:0:0;if(!V){var
s=b(d[25],e);if(-1===gZ(b(d[25],c),s)){var
t=b(d[15],e),u=a(d[9],n,t),v=b(d[15],c),w=a(d[9],o,v),x=a(d[1],w,u),y=fG(h,f),z=[0,g,b(d[15],e)],A=[0,cM([0,i,b(d[15],c)],z),y,x],B=[0,k,b(d[15],e)];return[0,[0,cM([0,m,b(d[15],c)],B),A]]}if(0===h){var
C=a(d[9],n,Ap),D=a(d[9],c,e),E=b(d[3],D),F=a(d[9],o,E),G=a(d[1],F,C),H=fG(h,f),I=a(d[9],c,e),J=[0,cM([0,i,b(d[3],I)],[0,g,Aq]),H,G],K=a(d[9],c,e);return[0,[0,cM([0,m,b(d[3],K)],[0,k,Ar]),J]]}if(0===f){var
L=a(d[9],o,As),M=a(d[9],e,c),N=b(d[3],M),O=a(d[9],n,N),P=a(d[1],O,L),Q=fG(h,f),R=a(d[9],e,c),S=[0,cM([0,g,b(d[3],R)],[0,i,At]),Q,P],T=a(d[9],e,c);return[0,[0,cM([0,k,b(d[3],T)],[0,m,Au]),S]]}return 0}}return 0}var
lY=[0,function(y,c){function
e(c){switch(c[0]){case
0:var
k=c[1],z=a(o[17][7],y,k);return[0,[0,I(k,Az,N),z],0];case
1:var
A=c[3],B=c[1],C=e(c[2]),D=e(A),v=0,w=function(a,c){function
b(a,d){var
b=lX(B,c,d);return b?[0,b[1],a]:a}return h(o[17][15],b,a,D)};return h(o[17][15],w,v,C);default:var
E=c[2],F=e(c[1]),G=e(E),H=a(o[18],F,G),x=function(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=lP(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b},i=h(o[17][15],x,Av,H);if(0===i[0]){var
J=i[1],K=function(k,j){if(0===k[0]){var
t=k[1],l=j[2],m=j[1],n=j[4][1],x=t[2],y=t[1],u=n[2],v=n[1],o=function(e,c,b){if(c){var
f=c[1][3];if(b){var
d=b[1];return a(e,f,d)?[0,[0,m,l,d]]:c}return c}return b?[0,[0,m,l,b[1]]]:0},c=o(d[29],y,v),e=o(d[30],x,u);if(c)if(e){var
g=e[1],h=g[2],p=g[1],i=c[1],q=i[1],w=i[2];if(a(d[29],i[3],g[3]))return[0,[0,c,e]];var
r=a2(h[1]);if(r){var
s=lX(r[1][1],[0,q,w],[0,p,h]);return s?[1,s[1]]:b(f[3],Aw)}return[1,[0,cM([0,q,Ay],[0,p,Ax]),h]]}return[0,[0,c,e]]}return k},j=h(o[17][15],K,AA,J);if(0===j[0]){var
l=j[1],g=l[2],m=l[1];if(m){var
n=m[1],p=n[2],q=n[1];if(g){var
r=g[1];return[0,[0,q,p],[0,[0,r[1],r[2]],0]]}var
t=p,s=q}else{if(!g)return 0;var
u=g[1],t=u[2],s=u[1]}return[0,[0,s,t],0]}return[0,j[1],0]}return[0,i[1],0]}}return e(c)},fG],fH=[0,Ao,Ak];aE(1014,[0,[0,J[1],J[2],J[3],J[4],J[5],J[6],J[7],J[8],J[9],J[10],J[11],J[12],J[13],J[14],J[15],J[16],J[17],J[18],J[19],J[20],J[21],J[22],J[23],J[24],J[25],J[26]],fH,d5,lY,fF],"Micromega_plugin__Mfourier");var
b9=[0,1],AB=0,AC=p[8],AE=0;function
AF(a){return[1,b(ai[1],a)]}var
fI=[0,B[2],AF,AE,AD,AC,aQ],AI=ai[2],iA=[0,function(a){return[0,b(B[2],a),0]},AI,AH,AG,aY,ax];function
lZ(a,i){var
b=i;for(;;){var
d=a[6],e=a[5],f=a[4],g=a[3],h=function(b,c,d,e){return function(a){return jQ(e,d,c,b,a)}}(d,e,f,g),c=function(c){function
b(a){if(typeof
a!=="number")switch(a[0]){case
3:var
d=a[1],e=b(a[2]);return c([3,b(d),e]);case
4:var
f=a[1],g=b(a[2]);return c([4,b(f),g])}return c(a)}return b}(h)(b);if(_(c,b))return c;var
b=c;continue}}function
fJ(a){var
e=a[2],c=bo(a[1]),f=c[2];return[0,f,e,b(d[3],c[1])]}function
AL(c){var
p=u[1];function
q(d,c){var
b=hJ(c[1]);return a(u[7],d,b)}var
r=h(g[20],q,p,c),s=0;function
t(k,j){var
a=0;function
d(b,a){return[0,ay(k,a[1]),b]}var
e=h(g[20],d,a,c),f=[1,n[1]],i=b(g[9],e);return[0,[0,cB([0,[1,n[1]],[0,[1,n[1]],i]]),0,f],j]}var
v=h(u[15],t,r,s),i=0;function
j(c,a){return[0,b(d[3],a[3]),c]}var
k=h(g[20],j,i,c),l=[1,n[1]],m=b(g[9],k),o=[0,cB([0,[1,n[1]],[0,[1,n[2]],m]]),0,l],w=[1,n[2]],x=1;function
y(a){return ln(a)?[1,n[2]]:[1,n[1]]}var
z=a(g[17],y,c),A=[0,cB([0,[1,n[1]],[0,[1,n[2]],z]]),x,w],B=[0,o,v];function
e(f,d){var
b=f,a=d;for(;;){if(a){var
c=a[2];if(0===a[1][2]){var
b=b+1|0,a=c;continue}var
g=e(b+1|0,c),h=1;return[0,[0,hF(b+1|0,function(a){return AK},N),h,AJ],g]}return 0}}var
C=[0,A,e(1,c)],D=a(f[26],C,B),E=[1,n[1]];return[0,[0,cB([0,[1,n[1]],[0,[1,n[2]],0]]),1,E],D]}function
iB(c){if(b9[1])return lG(c);var
d=b(fH[1],c);if(0===d[0])return 0;var
e=a(lY[1],c,d[1]);return[0,c5(b(o[17][5],e)[1])]}function
AM(a){if(b9[1])return lH(a);var
c=b(fH[1],a);return 0===c[0]?[0,c[1]]:0}function
AP(g){try{var
a=iB(g);return a}catch(a){a=q(a);if(a===fc){var
h=AL(g);try{var
c=AM(h);if(c)var
d=c[1],i=a2(d)?[0,c5(kr(2,I(1,AN,d)))]:b(f[3],AO),e=i;else
var
e=0;return e}catch(a){a=q(a);if(b(df[18],a))return 0;throw a}}throw a}}function
iC(a){var
b=[0,0,t[1]];function
c(a,c){var
b=a[1];return[0,b+1|0,h(t[4],b,c,a[2])]}return h(o[17][15],c,b,a)[2]}function
iD(e){var
c=b(o[17][nJ],e),f=c[2],d=AP(c[1]);if(d){var
g=d[1],h=iC(f);return[0,a(V[12],h,g)]}return 0}var
d6=[aX,AR,aW(0)];function
iE(k){var
e=k[2],g=k[1],h=g[3],i=g[2],j=g[1];if(a2(j)){var
l=hL(j),c=[1,l];if(a(d[32],c,AS))return[2,g,e];var
m=a(d[12],h,c),n=b(d[25],m);if(a(G[2],n,0)){if(1<=b(d[25],c)){var
o=a(d[9],h,c);return[2,[0,c3(c,j),i,o],[6,l,e]]}throw[0,a0,AT]}switch(i){case
0:return[0,[9,e]];case
1:var
p=a(d[9],h,c),q=b(d[24],p);return[1,[0,c3(c,j),i,q],[9,e]];default:return b(f[3],AU)}}return a(fd(i),AV,h)?0:[0,e]}function
l0(g,f,a){var
c=0;function
d(c,d){var
e=b(f,d);if(e){var
a=b(g,e[1]);if(typeof
a==="number")return c;else
switch(a[0]){case
0:throw[0,d6,a[1]];case
1:return[0,[0,a[1],a[2]],c];default:return[0,[0,a[1],a[2]],c]}}return[0,d,c]}return h(o[17][15],d,c,a)}function
l1(c){return eH(function(f){var
e=bJ(function(l){var
c=l[1],g=c[2],h=c[1];function
i(b){var
c=a(d[26],b,AW);return c?c:a(d[26],b,AX)}if(0===g){var
j=a(am[13],i,h),k=function(e){function
c(d){var
c=b(am[9],d[1][1]);return c?c:a(am[10],e,d[1][1])}return a(o[17][21],c,f)},e=a(o[17][61],k,j);return e?[0,e[1]]:0}return 0},f),g=e[1],j=e[2];if(g){var
i=g[1];return eF(h(az[12],c,i[2],i[1]),j)}return 0},c)}function
iF(b){return a(az[15],0,b)}function
l2(c,b){if(1===c)return b;var
d=l2(c-1|0,b);return a(az[8],b,d)}function
AY(d,c){if(!b(fx[6],c))if(!b(fx[3],c))try{var
e=b(az[7],AZ),f=function(e,c,b){var
f=l2(c,a(t[27],e,d));return a(az[8],f,b)},g=[0,h(fx[1],f,c,e)];return g}catch(a){a=q(a);if(a===O)return 0;throw a}return 0}function
d7(g,e,d){b(am[1][1],0);var
c=b(o[17][1],d),h=a(f[6],g,gZ(c,g));h_[1]=a(f[6],c,h);function
i(f){var
g=f[1];switch(f[2]){case
0:var
c=0;break;case
1:throw[0,a0,AQ];case
2:var
c=2;break;default:var
c=1}function
d(c){switch(c[0]){case
0:var
g=b(e[2],c[1]);return b(bu[1],g);case
1:var
h=b(ai[3],c[1]);return b(bu[2],h);case
2:var
i=c[1],j=d(c[2]),k=d(i);return a(bu[3],k,j);case
3:var
l=c[1],m=d(c[2]),n=b(bu[5],m),o=d(l);return a(bu[3],o,n);case
4:var
p=c[2],q=d(c[1]),r=d(p);return a(bu[4],q,r);case
5:var
s=d(c[1]);return b(bu[5],s);default:var
t=c[2],u=d(c[1]),v=b(ai[4],t),f=function(c){if(a(G[2],c,0)){var
d=b(e[2],e[4]);return b(bu[1],d)}var
g=f(c-1|0);return a(bu[4],u,g)};return f(v)}}return[0,d(g),c]}var
j=a(o[17][68],i,d);function
k(c,a){var
d=a[2];return[0,[0,b(am[2],a[1]),d],[1,c]]}return a(o[17][13],k,j)}function
l3(c){function
f(a){return b(am[9],a[1][1])}if(a(o[17][21],f,c))return c;var
g=da[1];function
i(c,a){var
d=b(am[16],a[1][1]);function
e(c,a,b){return[0,a]}return h(da[38],e,c,d)}var
j=h(o[17][15],i,g,c);function
k(d,c,a){var
e=b(am[5],d);return[0,[0,[0,b(am[5],c),1],[4,e]],a]}var
d=h(da[12],k,j,c),l=u[1];function
m(d,c){var
e=b(am[7],c[1][1]);return a(u[7],d,e)}var
n=h(o[17][15],m,l,d);function
p(e,d){var
c=b(am[3],e);return[0,[0,[0,a(am[14],c,c),1],[4,c]],d]}var
e=h(u[15],p,n,d),q=ka(az[8],e),r=a(o[18],e,q),s=b(az[2],A0);return a(o[17][68],s,r)}function
l4(i,g){var
c=l1(d7(i,iA,g)),j=iF(c),k=l3(c),l=a(o[18],k,j);function
m(a){var
b=a[1],c=a[2];return[0,fJ([0,b[1],b[2]]),c]}var
d=a(o[17][68],m,l),n=0;function
p(d,c){var
e=b(V[2],c[2]);return a(f[6],d,e)}var
q=h(o[17][15],p,n,d),r=a(iG[53],0,q),e=iD(d);return e?[0,F(V[11],hx,B[5],r,e[1])]:0}function
iH(e,d){var
f=d7(e,iA,d);function
g(a){var
b=a[2];return[0,fJ(a[1]),b]}var
b=a(o[17][68],g,f),c=iD(b);if(c){var
h=c[1],i=function(a,b){return a},j=a(o[17][13],i,b);return[0,F(V[11],hx,B[5],j,h)]}return 0}function
cN(c){if(typeof
c==="number")return[0,n[2],0];else
switch(c[0]){case
0:var
e=c[1],y=[0,[1,ah(e)]];return[0,bd(e),y];case
1:return[0,n[2],[1,c[1]]];case
2:var
g=cN(c[1]);return[0,g[1],[2,g[2]]];case
3:var
h=c[1],z=h[2],i=cN(h[1]),j=i[2],k=i[1],l=cN(z),m=l[2],o=l[1],d=a(n[17],k,o),p=a(n[15],k,d),q=a(n[15],o,d),A=a(n[10],p,q),r=a(n[10],d,A),B=a(n[23],r,n[2]);return a(G[2],B,0)?[0,n[2],[3,[0,j,m]]]:[0,r,[3,[0,[5,[0,[0,[1,q]],j]],[5,[0,[0,[1,p]],m]]]]];case
4:return b(f[3],A1);case
5:var
s=c[1],C=s[2],t=cN(s[1]),D=t[2],E=t[1],u=cN(C),F=[5,[0,D,u[2]]];return[0,a(n[10],E,u[1]),F];default:var
v=c[1],w=v[2],x=cN(v[1]),H=[6,[0,x[2],w]];return[0,a(n[19],x[1],w),H]}}function
l5(b){var
a=cN(b);return[0,a[1],a[2]]}function
dg(b){switch(b[0]){case
0:return[0,n[2],[0,b[1]]];case
1:return[0,n[2],[1,b[1]]];case
2:return[0,n[2],[2,b[1]]];case
3:var
d=b[1],t=[3,[1,ah(d)]];return[0,bd(d),t];case
4:var
e=b[1],u=[4,[1,ah(e)]];return[0,bd(e),u];case
5:var
f=b[1],v=[5,[1,ah(f)]];return[0,bd(f),v];case
6:var
g=l5(b[1]),h=g[1],w=[6,g[2]];return[0,a(n[10],h,h),w];case
7:return[0,n[2],[7,b[1]]];case
8:var
x=b[2],i=l5(b[1]),y=i[2],z=i[1],j=dg(x),A=[8,y,j[2]];return[0,a(n[10],z,j[1]),A];case
9:var
B=b[2],k=dg(b[1]),l=k[1],C=k[2],m=dg(B),o=m[1],D=m[2],c=a(n[17],l,o),p=a(n[15],l,c),q=a(n[15],o,c),E=a(n[10],p,q);return[0,a(n[10],c,E),[9,[10,[4,[1,q]],C],[10,[4,[1,p]],D]]];default:var
F=b[2],r=dg(b[1]),G=r[2],H=r[1],s=dg(F),I=[10,G,s[2]];return[0,a(n[10],H,s[1]),I]}}function
bS(a){if(typeof
a==="number")return[0,b(B[5],A2)];else
switch(a[0]){case
0:return[0,b(B[5],a[1])];case
1:var
c=a[1],i=m6(h(o[15][4],c,1,cQ(c)-1|0));return[1,b(B[6],i)];case
2:return[5,bS(a[1])];case
3:var
d=a[1],j=d[1],k=bS(d[2]);return[2,bS(j),k];case
4:var
e=a[1],l=e[1],m=bS(e[2]);return[3,bS(l),m];case
5:var
f=a[1],n=f[1],p=bS(f[2]);return[4,bS(n),p];default:var
g=a[1],q=g[1],r=b(B[3],g[2]);return[6,bS(q),r]}}function
l6(a){var
c=bS(a),d=b(B[5],A3);return $(b(B[5],A4),d,aR,aY,b6,bH,ax,c)}function
iI(a){if(a){var
c=a[2],d=a[1];if(c){var
e=iI(c);return[3,[0,b(B[4],d)],e]}return[0,b(B[4],d)]}return 0}function
l7(c){function
e(c){switch(c[0]){case
0:return[0,b(B[4],c[1])];case
1:return[0,b(B[4],c[1])];case
2:return[0,b(B[4],c[1])];case
6:return[1,l6(c[1])];case
7:return iI(c[1]);case
8:var
h=c[1],i=e(c[2]);return[2,l6(h),i];case
9:var
j=c[1],k=e(c[2]);return[4,e(j),k];case
10:var
l=c[1],m=e(c[2]);return[3,e(l),m];default:var
f=c[1],g=a(d[37],f,A5);return a(G[2],g,0)?0:[5,b(B[5],f)]}}return lZ(iA,e(c))}function
bT(a){if(typeof
a==="number")return A6;else
switch(a[0]){case
0:var
j=b(d[52],a[1]);return[0,b(B[2],j)];case
1:var
c=a[1],k=m6(h(o[15][4],c,1,cQ(c)-1|0));return[1,b(B[6],k)];case
2:return[5,bT(a[1])];case
3:var
e=a[1],l=e[1],m=bT(e[2]);return[2,bT(l),m];case
4:var
f=a[1],n=f[1],p=bT(f[2]);return[3,bT(n),p];case
5:var
g=a[1],q=g[1],r=bT(g[2]);return[4,bT(q),r];default:var
i=a[1],s=i[1],t=b(B[3],i[2]);return[6,bT(s),t]}}function
l8(a){var
c=bT(a),d=p[6],e=p[7],f=p[8],g=p[5],h=b(B[7],1);return $(b(B[7],0),h,g,f,e,d,aQ,c)}function
l9(c){var
f=dg(c)[2];function
e(k){var
c=k;for(;;)switch(c[0]){case
0:return[0,b(B[4],c[1])];case
1:return[0,b(B[4],c[1])];case
2:return[0,b(B[4],c[1])];case
6:return[1,l8(c[1])];case
7:return iI(c[1]);case
8:var
i=c[2],f=c[1];if(typeof
f==="number")var
g=0;else
if(0===f[0])var
j=a(d[26],f[1],A8),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
c=i;continue}var
n=e(i);return[2,l8(f),n];case
9:var
o=c[1],p=e(c[2]);return[4,e(o),p];case
10:var
q=c[1],r=e(c[2]);return[3,e(q),r];default:var
h=c[1],l=a(d[37],h,A7);if(a(G[2],l,0))return 0;var
m=b(d[52],h);return[5,b(B[2],m)]}}return lZ(fI,e(f))}function
A9(a){var
b=0;function
c(b,c){var
a=iE([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,d6,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return h(o[17][15],c,b,a)}function
iJ(g,c){var
h=b(n[22],c);if(a(G[2],h,0))return[0,n[2],n[1]];var
d=a(n[14],g,c),i=d[1],e=iJ(c,d[2]),f=e[2],j=e[1],k=a(n[10],i,f);return[0,f,a(n[8],j,k)]}function
l_(n,m,c){return l0(iE,function(o){var
f=o[1],g=m[1],i=f[2],j=f[1],k=g[2],l=g[1],p=o[2],q=m[2],r=f[3],s=g[3];function
h(c,b){var
e=a(V[8],b,p),f=a(V[8],c,q),g=a(V[7],f,e),h=a(d[6],r,b),m=a(d[6],s,c),n=a(d[1],m,h),o=fe(k,i),t=ar(b,j);return[0,[0,a1(ar(c,l),t),o,n],g]}var
c=ay(n,l),e=ay(n,j),C=0===c[0]?0===c[1]?1:0:0;if(!C){var
D=0===e[0]?0===e[1]?1:0:0;if(!D){var
t=b(d[25],e),u=gZ(b(d[25],c),t);if(a(G[2],u,-1)){var
v=b(d[15],e);return[0,h(v,b(d[15],c))]}if(0===k){var
w=[0,b(d[25],c)],x=a(d[6],e,w),y=b(d[3],x);return[0,h(y,b(d[15],c))]}if(0===i){var
z=b(d[15],e),A=[0,b(d[25],e)],B=a(d[6],c,A);return[0,h(z,b(d[3],B))]}return 0}}return 0},c)}function
A_(z){var
c=0,b=z;for(;;){if(b){var
l=b[2],e=b[1],m=bJ(function(g){return function(f){var
b=f[1],c=g[1];if(0===c[2])if(0===b[2]){var
d=b[1],e=c[1];return ks(function(c,b){var
d=n[2],e=ah(b),f=ah(c),g=a(n[17],f,e),h=a(n[23],g,d);return a(G[2],h,0)},e,d)}return 0}}(e),l),p=m[1];if(!p){var
c=[0,e,c],b=l;continue}var
q=p[1],x=q[2],y=q[1],f=[0,[0,[0,y,e,x]],a(o[17][10],c,m[2])]}else
var
f=[0,0,c];var
r=f[1],A=f[2];if(r){var
g=r[1],s=g[3],t=s[1],u=g[2],v=u[2],h=u[1],i=g[1],B=s[2],C=i[2],D=i[1],E=ah(i[3]),w=iJ(ah(C),E),j=[1,w[1]],k=[1,w[2]],F=a(d[6],k,t[3]),H=a(d[6],j,h[3]),I=a(d[1],H,F),J=ar(k,t[1]),K=[0,a1(ar(j,h[1]),J),0,I],L=a(V[8],k,B),M=a(V[8],j,v);return[0,l_(D,[0,K,a(V[7],M,L)],[0,[0,h,v],A])]}return 0}}function
A$(f){var
b=bJ(function(c){var
b=c[1];if(0===b[2]){var
e=b[1];return hK(function(c,b){if(!a(d[26],b,Ba))if(!a(d[26],b,Bb))return 0;return[0,c]},e)}return 0},f),c=b[1],g=b[2];if(c){var
e=c[1];return[0,l_(e[1],e[2],g)]}return 0}function
Bc(m){var
c=bJ(function(k){var
i=k[1];if(0===i[2]){var
c=i[1];for(;;){var
d=a2(c);if(d){var
b=d[1],e=b[3],j=b[1],f=ah(b[2]),g=hK(function(g){return function(d,c){var
b=ah(c),e=n[2],f=a(n[17],g,b);return a(n[24],f,e)?[0,[0,d,b]]:0}}(f),e);if(g){var
h=g[1];return[0,[0,[0,j,f],[0,h[1],h[2]]]]}var
c=e;continue}return 0}}return 0},m),e=c[1],o=c[2];if(e){var
f=e[1],g=f[2],h=g[1],i=f[1],j=i[2],k=i[1],p=g[2],q=j[1],r=k[1],l=iJ(k[2],j[2]),s=[1,l[2]],t=[1,l[1]];return[0,l0(iE,function(g){var
c=g[1],e=c[1],i=g[2],j=c[3],k=c[2],l=ay(r,e),m=ay(q,e),n=a(d[6],m,s),o=a(d[6],l,t),u=a(d[1],o,n),f=b(d[3],u),v=a(V[8],f,p),w=a(V[7],v,i),x=a(d[6],f,h[3]),y=a(d[1],x,j);return[0,[0,[0,a1(ar(f,h[1]),e),k,y],w]]},o)]}return 0}function
iK(a){var
b=[0,A$,[0,A_,[0,Bc,0]]];return eH(function(a){return kh(b,a)},a)}function
l$(c){function
e(a){var
c=a[1][1];return eP(function(c,a){return 0!==b(d[25],a)?1:0},c)}return a(o[17][21],e,c)}function
Bg(k,j,g){function
l(F,m){if(l$(m)){var
G=b(o[17][nJ],m),H=G[2],c=G[1],J=function(a){return 0===a[2]?1:0},u=a(o[17][30],J,c),v=u[2],w=u[1];if(w)var
K=0,L=function(c,b){function
d(a){return c1(b[1],a[1])}return a(o[17][22],d,w)?c:[0,b[1],c]},x=h(o[17][15],L,K,v);else
var
M=function(a){return a[1]},x=a(o[17][14],M,v);var
O=[0,N,Be],P=function(b,e){var
f=fE(b[2]),k=f?a(d[29],f[1],Bd):0;if(k)return b;var
h=b9[1]?lI(e,c):a(fH[2],e,c);if(h){var
i=h[1],g=b[2],j=b[1];return lN(i,g)?[0,e,i]:[0,j,g]}return b},y=h(o[17][15],P,O,x),z=y[2],A=z[1],Q=y[1];if(A){var
B=z[2];if(B)var
g=[0,[0,A[1],Q,B[1]]],s=1;else
var
s=0}else
var
s=0;if(!s)var
g=0;if(g){var
i=g[1],j=i[3],k=i[2],l=i[1],R=bd(l),S=n[2],T=ah(l),U=a(n[8],T,S),W=bd(j),X=ah(j),Y=[1,a(n[5],n[2],X)],C=iB([0,[0,ar([1,W],k),1,Y],c]),Z=b(d[3],[1,U]),D=iB([0,[0,ar(b(d[3],[1,R]),k),1,Z],c]);if(C)if(D)var
_=D[1],$=hE(C[1]),aa=b(o[17][6],$),ab=hE(_),e=[0,[0,b(o[17][6],ab),[0,l,k,j],aa]],t=1;else
var
t=0;else
var
t=0;if(!t)var
e=b(f[3],Bf)}else
var
e=0;if(e){var
p=e[1],q=p[2],I=q[2],ac=p[3],ad=q[1],ae=p[1],af=b(d[22],q[3]),r=E(F,I,b(d[24],ad),af,m);if(typeof
r!=="number"&&0===r[0]){var
ag=r[1],ai=cB(ac),aj=iC(H),ak=a(V[12],aj,ai),al=cB(ae),am=iC(H);return[0,[1,F,a(V[12],am,al),I,ak,ag]]}return 0}return 0}throw[0,a0,Bh]}function
E(c,j,b,h,g){if(a(d[28],b,h))return Bi;var
e=i(c+1|0,[0,[0,[0,j,0,b],[2,c]],g]);if(typeof
e!=="number"&&0===e[0]){var
k=e[1],f=E(c,j,a(d[1],b,Bj),h,g);if(typeof
f!=="number"&&0===f[0])return[0,[0,k,f[1]]];return 0}return 0}function
i(c,a){if(l$(a))try{var
d=b(j,a),e=iD(d),f=e?[0,[0,c,e[1],0]]:k?l(c,d):0;return f}catch(a){a=q(a);if(a[1]===d6)return[0,[0,c,a[2],0]];throw a}throw[0,a0,Bk]}var
m=0;function
p(d,c){var
e=b(V[2],c[2]);return a(f[6],d,e)}var
e=1+h(o[17][15],p,m,g)|0;try{var
t=i(e,A9(g)),c=t}catch(a){a=q(a);if(a[1]!==d6)throw a;var
c=[0,[0,e,a[2],0]]}if(typeof
c!=="number"&&0===c[0]){var
r=c[1],s=a(iG[53],0,e-1|0);return[0,a(V[10],s,r)]}return 0}function
Bl(k,i,c){function
d(d,c){var
e=0;function
g(d,c){var
e=b(V[2],c[2]);return a(f[6],d,e)}var
i=(1+h(o[17][15],g,e,d)|0)-1|0,j=a(iG[53],0,i);return[0,a(V[10],j,c)]}try{var
e=b(i,c),g=lL(e),j=g?d(e,g[1]):0;return j}catch(a){a=q(a);if(a[1]===d6)return d(c,[0,0,a[2],0]);throw a}}function
iL(d,c,b,a){return b9[1]?Bl(d,b,a):Bg(c,b,a)}var
fK=[0,0];function
ma(i,l,g,e){var
j=i[1],d=h(i[2],l,g,e),k=fK[1];if(k){var
n=k[1],p=[0,E.caml_sys_getcwd(0)],q=h(bM[14],p,n,Bm),c=b(f[49],q),r=d7(g,fI,e);a(m[1],c,Bn);var
s=a(o[17][68],b1,r),t=b(am[19],Bo);F(m[1],c,Bp,t,s);var
u=typeof
d==="number"?0:0===d[0]?(h(m[1],c,Br,j),1):0;if(!u)h(m[1],c,Bq,j);b(f[52],c);b(f[65],c)}return d}function
Bs(s,r,q){var
u=d7(r,fI,q),c=b(az[13],u),e=kd(function(a){return b(am[8],a[1][1])},c)[1],f=t[1];function
g(b,a){return h(t[4],a[1],a[2],b)}var
i=h(o[17][15],g,f,e),j=t[1];function
k(c,a){var
d=a[1][1];return ab(function(c,a,e){var
d=AY(i,b(am[1][2],a));return d?h(t[4],a,d[1],c):c},c,d)}var
l=h(o[17][15],k,j,c),m=0;function
n(c,b,a){return[0,b,a]}var
p=h(t[13],n,l,m),v=iF(c),w=a(o[18],v,c),d=a(o[18],p,w);function
x(a){var
b=a[1],c=a[2];return[0,fJ([0,b[1],b[2]]),c]}var
y=a(o[17][68],x,d);return iL(a(o[17][68],b1,d),s,iK,y)}function
mb(b){function
c(a){var
b=a[1],c=a[2];return[0,fJ([0,b[1],b[2]]),c]}return a(o[17][68],c,b)}function
Bt(d,g,f){var
c=d7(g,fI,f);function
h(a){return b(am[9],a[1][1])}if(a(o[17][21],h,c)){var
i=mb(c);return iL(a(o[17][68],b1,c),d,iK,i)}var
e=l1(c),j=iF(e),k=mb(l3(a(o[18],e,j)));return iL(a(o[17][68],b1,c),d,iK,k)}function
mc(c,b,a){return ma([0,Bu,Bs],c,b,a)}function
md(c,b,a){return ma([0,Bv,Bt],c,b,a)}aE(1016,[0,AB,b9,fK,l7,l9,mc,md,iH,l4],"Micromega_plugin__Certificate");var
iM=f[8],me=[0,iM],iN=[0,1],mf=[0,iM];function
mg(a){return[0,b9[1],iN[1],mf[1]]}function
iO(a){return me[1]}function
mh(b,a){function
c(b){var
c=b?b[1]:iM;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,0,h(g[21],f[17],b,Bw),b,d,c]}function
Bx(a){iN[1]=a;return 0}var
BA=[0,0,Bz,By,function(a){return iN[1]},Bx];function
BB(a){b9[1]=a;return 0}var
BE=[0,0,BD,BC,function(a){return b9[1]},BB];function
BF(a){fK[1]=a;return 0}var
BI=[0,0,BH,BG,function(a){return fK[1]},BF];a(d8[4],0,BE);a(d8[6],0,BI);var
BK=mh(BJ,me);a(d8[3],0,BK);var
BM=mh(BL,mf);a(d8[3],0,BM);a(d8[4],0,BA);var
BO=a(f[26],d9[20],mi),BP=a(f[26],d9[19],BO),BQ=a(f[26],[0,BN,0],BP),BR=a(f[26],d9[21],BQ);function
Y(d,c,a){var
e=h(d9[18],d,c,a),f=b(BV[23],e);return b(l[9],f)}var
BW=d9[21];function
aA(a){return Y(BX,BW,a)}function
v(a){return Y(BY,BR,a)}function
bU(a){return Y(BZ,BS,a)}function
aU(a){return Y(B0,BT,a)}function
bV(a){return Y(B1,BU,a)}function
bi(a){return Y(B2,mi,a)}var
a5=[e,function(a){return aA(B3)}],a6=[e,function(a){return aA(B4)}],b$=[e,function(a){return aA(B5)}],fL=[e,function(a){return aA(B6)}],ca=[e,function(a){return aA(B7)}],a7=[e,function(a){return aA(B8)}],fM=[e,function(a){return v(B9)}],fN=[e,function(a){return v(B_)}],cb=[e,function(a){return v(B$)}],fO=[e,function(a){return aA(Ca)}],fP=[e,function(a){return aA(Cb)}],cc=[e,function(a){return aA(Cc)}],a8=[e,function(a){return aA(Cd)}],fQ=[e,function(a){return aA(Ce)}],fR=[e,function(a){return aA(Cf)}],fS=[e,function(a){return aA(Cg)}],fT=[e,function(a){return aA(Ch)}],fU=[e,function(a){return bU(Ci)}],fV=[e,function(a){return bU(Cj)}],fW=[e,function(a){return bU(Ck)}],fX=[e,function(a){return bU(Cl)}],fY=[e,function(a){return bU(Cm)}],x=[e,function(a){return bU(Cn)}],fZ=[e,function(a){return bU(Co)}],f0=[e,function(a){return bU(Cp)}],f1=[e,function(a){return bU(Cq)}],aL=[e,function(a){return v(Cr)}],at=[e,function(a){return v(Cs)}],cd=[e,function(a){return v(Ct)}],ce=[e,function(a){return v(Cu)}],f2=[e,function(a){return bi(Cv)}],f3=[e,function(a){return bi(Cw)}],f4=[e,function(a){return bi(Cx)}],f5=[e,function(a){return bi(Cy)}],f6=[e,function(a){return bi(Cz)}],f7=[e,function(a){return bi(CA)}],f8=[e,function(a){return bi(CB)}],f9=[e,function(a){return bi(CC)}],f_=[e,function(a){return bi(CD)}],f$=[e,function(a){return bi(CE)}],cf=[e,function(a){return v(CF)}],cg=[e,function(a){return v(CG)}],ch=[e,function(a){return v(CH)}],ga=[e,function(a){return v(CI)}],gb=[e,function(a){return v(CJ)}],gc=[e,function(a){return v(CK)}],gd=[e,function(a){return v(CL)}],CN=[e,function(a){return bV(CM)}],CP=[e,function(a){return bV(CO)}],CR=[e,function(a){return bV(CQ)}],CT=[e,function(a){return bV(CS)}],a9=[e,function(a){return aA(CU)}],d_=[e,function(a){return bV(CV)}],d$=[e,function(a){return bV(CW)}],ea=[e,function(a){return bV(CX)}],eb=[e,function(a){return bV(CY)}],ec=[e,function(a){return bV(CZ)}],C1=[e,function(a){return v(C0)}],C3=[e,function(a){return v(C2)}],C5=[e,function(a){return v(C4)}],ed=[e,function(a){return v(C6)}],ee=[e,function(a){return v(C7)}],ef=[e,function(a){return v(C8)}],eg=[e,function(a){return v(C9)}],eh=[e,function(a){return v(C_)}],Da=[e,function(a){return aU(C$)}],Dc=[e,function(a){return aU(Db)}],De=[e,function(a){return aU(Dd)}],Dg=[e,function(a){return aU(Df)}],bv=[e,function(a){return aU(Dh)}],bw=[e,function(a){return aU(Di)}],bW=[e,function(a){return aU(Dj)}],bx=[e,function(a){return aU(Dk)}],ci=[e,function(a){return aU(Dl)}],aV=[e,function(a){return aU(Dm)}],ge=[e,function(a){return aU(Dn)}],cj=[e,function(a){return aU(Do)}],ck=[e,function(a){return aU(Dp)}],gf=[e,function(a){return v(Dq)}],gg=[e,function(a){return v(Dr)}],gh=[e,function(a){return v(Ds)}],gi=[e,function(a){return v(Dt)}],gj=[e,function(a){return v(Du)}],gk=[e,function(a){return v(Dv)}],gl=[e,function(a){return v(Dw)}],gm=[e,function(a){return v(Dx)}],gn=[e,function(a){return v(Dy)}],go=[e,function(a){return v(Dz)}],gp=[e,function(a){return v(DA)}],gq=[e,function(a){return v(DB)}],gr=[e,function(a){return v(DC)}],gs=[e,function(a){return v(DD)}],gt=[e,function(a){return v(DE)}],gu=[e,function(a){return v(DF)}],gv=[e,function(a){return v(DG)}],gw=[e,function(a){return v(DH)}],gx=[e,function(a){return v(DI)}],gy=[e,function(a){return v(DJ)}],gz=[e,function(a){return v(DK)}],gA=[e,function(a){return v(DL)}],gB=[e,function(a){return v(DM)}],gC=[e,function(a){return bi(DN)}],DR=[e,function(a){return Y(DQ,DP,DO)}],DV=[e,function(a){return Y(DU,DT,DS)}],DZ=[e,function(a){return Y(DY,DX,DW)}],D3=[e,function(a){return Y(D2,D1,D0)}],D7=[e,function(a){return Y(D6,D5,D4)}],D$=[e,function(a){return Y(D_,D9,D8)}],Ed=[e,function(a){return Y(Ec,Eb,Ea)}],Eh=[e,function(a){return Y(Eg,Ef,Ee)}],cl=[e,function(a){return Y(Ek,Ej,Ei)}],a_=[e,function(a){return Y(En,Em,El)}],gD=[e,function(a){return Y(Eq,Ep,Eo)}],cm=[e,function(a){return Y(Et,Es,Er)}],P=[aX,Eu,aW(0)];function
iP(c,e){var
b=a(l[3],c,e);switch(b[0]){case
9:var
f=b[2],d=a(l[3],c,b[1]);if(12===d[0])return[0,d[1][1][2],f];throw P;case
12:return[0,b[1][1][2],[0]];default:throw P}}function
iQ(a,d){var
b=iP(a,d),c=b[1],e=b[2];if(1===c)return 0;if(2===c)return[0,iQ(a,K(e,0)[1])];throw P}function
Ev(c,a){var
d=b(ai[5],a);return h(m[1],c,Ew,d)}function
ei(a){if(a){var
f=[0,ei(a[1])],c=j(fP),g=k===c?fP[1]:e===c?b(i[2],fP):fP;return b(l[23],[0,g,f])}var
d=j(fO);return k===d?fO[1]:e===d?b(i[2],fO):fO}function
ej(a,e){var
b=iP(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw P;switch(d){case
0:return[0,ej(a,K(c,0)[1])];case
1:return[1,ej(a,K(c,0)[1])];default:return 0}}function
bX(a){if(typeof
a==="number"){var
c=j(fW);return k===c?fW[1]:e===c?b(i[2],fW):fW}else{if(0===a[0]){var
g=[0,bX(a[1])],d=j(fY),h=k===d?fY[1]:e===d?b(i[2],fY):fY;return b(l[23],[0,h,g])}var
m=[0,bX(a[1])],f=j(fX),n=k===f?fX[1]:e===f?b(i[2],fX):fX;return b(l[23],[0,n,m])}}function
iR(c,a){var
d=b(ai[3],a);return h(m[1],c,Ex,d)}function
mj(f,d,c){switch(a(l[3],d,c)[0]){case
10:case
12:var
h=W(mk[2],0,0,f,d,c);try{var
g=j(gC),m=[0,h,c],n=k===g?gC[1]:e===g?b(i[2],gC):gC,o=b(l[23],[0,n,m]);F(Ey[24],0,f,d,o);var
p=1;return p}catch(a){a=q(a);if(a===O)return 0;throw a}default:return 0}}function
iS(c,b,e){var
d=a(l[3],b,e);switch(d[0]){case
9:var
g=d[2],f=mj(c,b,d[1]);if(f){var
h=function(a){return iS(c,b,a)};return a(ml[21],h,g)}return f;case
10:case
12:return mj(c,b,e);default:return 0}}function
mm(a,e){var
b=iP(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw P;switch(d){case
0:return 0;case
1:return[0,ej(a,K(c,0)[1])];default:return[1,ej(a,K(c,0)[1])]}}function
au(a){if(typeof
a==="number"){var
c=j(fZ);return k===c?fZ[1]:e===c?b(i[2],fZ):fZ}else{if(0===a[0]){var
g=[0,bX(a[1])],d=j(f0),h=k===d?f0[1]:e===d?b(i[2],f0):f0;return b(l[23],[0,h,g])}var
m=[0,bX(a[1])],f=j(f1),n=k===f?f1[1]:e===f?b(i[2],f1):f1;return b(l[23],[0,n,m])}}function
cn(c,a){var
d=b(ai[1],a),e=b(n[33],d);return h(m[1],c,Ez,e)}function
cO(a){var
d=bX(a[2]),f=[0,au(a[1]),d],c=j(cd),g=k===c?cd[1]:e===c?b(i[2],cd):cd;return b(l[23],[0,g,f])}function
EA(c,m){var
d=a(l[3],c,m);if(9===d[0]){var
f=d[2],n=d[1],g=j(cd),o=k===g?cd[1]:e===g?b(i[2],cd):cd;if(h(l[an],c,n,o)){var
p=ej(c,K(f,1)[2]);return[0,mm(c,K(f,0)[1]),p]}throw P}throw P}function
by(a){if(typeof
a==="number"){if(0===a){var
d=j(f2);return k===d?f2[1]:e===d?b(i[2],f2):f2}var
f=j(f3);return k===f?f3[1]:e===f?b(i[2],f3):f3}else
switch(a[0]){case
0:var
A=[0,cO(a[1])],g=j(f4),B=k===g?f4[1]:e===g?b(i[2],f4):f4;return b(l[23],[0,B,A]);case
1:var
C=[0,au(a[1])],h=j(f5),D=k===h?f5[1]:e===h?b(i[2],f5):f5;return b(l[23],[0,D,C]);case
2:var
E=a[1],F=by(a[2]),G=[0,by(E),F],m=j(f6),H=k===m?f6[1]:e===m?b(i[2],f6):f6;return b(l[23],[0,H,G]);case
3:var
I=a[1],J=by(a[2]),K=[0,by(I),J],n=j(f7),L=k===n?f7[1]:e===n?b(i[2],f7):f7;return b(l[23],[0,L,K]);case
4:var
M=a[1],N=by(a[2]),O=[0,by(M),N],o=j(f8),P=k===o?f8[1]:e===o?b(i[2],f8):f8;return b(l[23],[0,P,O]);case
5:var
c=a[2],Q=a[1];if(0===c[0])var
R=au(c[1]),p=j(cc),S=k===p?cc[1]:e===p?b(i[2],cc):cc,q=j(x),T=k===q?x[1]:e===q?b(i[2],x):x,r=j(fS),U=[0,T,S,R],V=k===r?fS[1]:e===r?b(i[2],fS):fS,s=b(l[23],[0,V,U]);else
var
Y=ei(c[1]),u=j(cc),Z=k===u?cc[1]:e===u?b(i[2],cc):cc,v=j(x),_=k===v?x[1]:e===v?b(i[2],x):x,w=j(fT),$=[0,_,Z,Y],aa=k===w?fT[1]:e===w?b(i[2],fT):fT,s=b(l[23],[0,aa,$]);var
W=[0,by(Q),s],t=j(f9),X=k===t?f9[1]:e===t?b(i[2],f9):f9;return b(l[23],[0,X,W]);case
6:var
ab=[0,by(a[1])],y=j(f_),ac=k===y?f_[1]:e===y?b(i[2],f_):f_;return b(l[23],[0,ac,ab]);default:var
ad=[0,by(a[1])],z=j(f$),ae=k===z?f$[1]:e===z?b(i[2],f$):f$;return b(l[23],[0,ae,ad])}}function
iT(c,d,a){if(a){var
h=a[1],m=iT(c,d,a[2]),n=[0,c,b(d,h),m],f=j(fM),o=k===f?fM[1]:e===f?b(i[2],fM):fM;return b(l[23],[0,o,n])}var
g=j(fN),p=[0,c],q=k===g?fN[1]:e===g?b(i[2],fN):fN;return b(l[23],[0,q,p])}function
mn(d,w,a){function
c(a){switch(a[0]){case
0:var
x=[0,d,b(w,a[1])],h=j(gg),y=k===h?gg[1]:e===h?b(i[2],gg):gg;return b(l[23],[0,y,x]);case
1:var
z=[0,d,bX(a[1])],m=j(gf),A=k===m?gf[1]:e===m?b(i[2],gf):gf;return b(l[23],[0,A,z]);case
2:var
B=a[1],C=c(a[2]),D=[0,d,c(B),C],n=j(gh),E=k===n?gh[1]:e===n?b(i[2],gh):gh;return b(l[23],[0,E,D]);case
3:var
F=a[1],G=c(a[2]),H=[0,d,c(F),G],o=j(gk),I=k===o?gk[1]:e===o?b(i[2],gk):gk;return b(l[23],[0,I,H]);case
4:var
J=a[1],K=c(a[2]),L=[0,d,c(J),K],p=j(gj),M=k===p?gj[1]:e===p?b(i[2],gj):gj;return b(l[23],[0,M,L]);case
5:var
N=[0,d,c(a[1])],q=j(gi),O=k===q?gi[1]:e===q?b(i[2],gi):gi;return b(l[23],[0,O,N]);default:var
r=a[2],P=a[1];if(r)var
u=[0,bX(r[1])],f=j(fV),v=k===f?fV[1]:e===f?b(i[2],fV):fV,s=b(l[23],[0,v,u]);else
var
g=j(fU),s=k===g?fU[1]:e===g?b(i[2],fU):fU;var
Q=[0,d,c(P),s],t=j(gl),R=k===t?gl[1]:e===t?b(i[2],gl):gl;return b(l[23],[0,R,Q])}}return c(a)}function
mo(d,m,a){function
c(a){switch(a[0]){case
0:var
n=[0,d,b(m,a[1])],f=j(gn),o=k===f?gn[1]:e===f?b(i[2],gn):gn;return b(l[23],[0,o,n]);case
1:var
p=a[1],q=c(a[2]),r=[0,d,bX(p),q],g=j(go),s=k===g?go[1]:e===g?b(i[2],go):go;return b(l[23],[0,s,r]);default:var
t=a[2],u=a[1],v=c(a[3]),w=bX(t),x=[0,d,c(u),w,v],h=j(gm),y=k===h?gm[1]:e===h?b(i[2],gm):gm;return b(l[23],[0,y,x])}}return c(a)}function
bz(d,c,a){function
b(c,a){switch(a[0]){case
0:return F(m[1],c,EE,d,a[1]);case
1:return Z(m[1],c,EF,iR,a[1],b,a[2]);default:return m7(m[1],c,EG,b,a[1],iR,a[2],b,a[3])}}return b(c,a)}function
dh(a,f,h){var
g=j(a),c=k===g?a[1]:e===g?b(i[2],a):a;function
d(a){if(typeof
a==="number"){var
g=j(gB),r=[0,c],s=k===g?gB[1]:e===g?b(i[2],gB):gB;return b(l[23],[0,s,r])}else
switch(a[0]){case
0:var
t=[0,c,ei(a[1])],h=j(gv),u=k===h?gv[1]:e===h?b(i[2],gv):gv;return b(l[23],[0,u,t]);case
1:var
v=[0,c,mo(c,f,a[1])],m=j(gw),w=k===m?gw[1]:e===m?b(i[2],gw):gw;return b(l[23],[0,w,v]);case
2:var
x=a[1],y=d(a[2]),z=[0,c,mo(c,f,x),y],n=j(gy),A=k===n?gy[1]:e===n?b(i[2],gy):gy;return b(l[23],[0,A,z]);case
3:var
B=a[1],C=d(a[2]),D=[0,c,d(B),C],o=j(gx),E=k===o?gx[1]:e===o?b(i[2],gx):gx;return b(l[23],[0,E,D]);case
4:var
F=a[1],G=d(a[2]),H=[0,c,d(F),G],p=j(gz),I=k===p?gz[1]:e===p?b(i[2],gz):gz;return b(l[23],[0,I,H]);default:var
J=[0,c,b(f,a[1])],q=j(gA),K=k===q?gA[1]:e===q?b(i[2],gA):gA;return b(l[23],[0,K,J])}}return d(h)}function
bY(e,c,b){function
d(c,b){if(typeof
b==="number")return a(m[1],c,EH);else
switch(b[0]){case
0:return F(m[1],c,EI,Ev,b[1]);case
1:var
f=b[1],g=function(a,b){return bz(e,a,b)};return F(m[1],c,EJ,g,f);case
2:var
h=b[2],i=b[1],j=function(a,b){return bz(e,a,b)};return Z(m[1],c,EK,j,i,d,h);case
3:return Z(m[1],c,EL,d,b[1],d,b[2]);case
4:return Z(m[1],c,EM,d,b[1],d,b[2]);default:return F(m[1],c,EN,e,b[1])}}return d(c,b)}function
ek(d,p,c){var
r=c[2],s=c[1],t=mn(d,p,c[3]);switch(r){case
0:var
f=j(gp),a=k===f?gp[1]:e===f?b(i[2],gp):gp;break;case
1:var
g=j(gq),a=k===g?gq[1]:e===g?b(i[2],gq):gq;break;case
2:var
h=j(gr),a=k===h?gr[1]:e===h?b(i[2],gr):gr;break;case
3:var
m=j(gt),a=k===m?gt[1]:e===m?b(i[2],gt):gt;break;case
4:var
n=j(gs),a=k===n?gs[1]:e===n?b(i[2],gs):gs;break;default:var
o=j(gu),a=k===o?gu[1]:e===o?b(i[2],gu):gu}var
u=[0,d,mn(d,p,s),a,t],q=j(gD),v=k===q?gD[1]:e===q?b(i[2],gD):gD;return b(l[23],[0,v,u])}function
gE(f,d,c){try{var
m=function(g){var
a=g[1],c=j(a),m=k===c?a[1]:e===c?b(i[2],a):a;return h(l[an],f,d,m)},n=a(g[33],m,c)[2];return n}catch(a){a=q(a);if(a===O)throw P;throw a}}var
mp=[0,[0,CN,5],[0,[0,CP,3],[0,[0,CT,4],[0,[0,CR,2],0]]]],mq=[0,[0,Da,5],[0,[0,Dc,3],[0,[0,Dg,4],[0,[0,De,2],0]]]],mr=[0,[0,C3,4],[0,[0,C1,2],[0,[0,C5,0],0]]];function
ms(a,c,b){return W(EO[81],0,a[1],a[2],c,b)}function
EP(d,c){var
a=c[2],f=c[1],g=d[2],m=a.length-1;if(2===m){var
p=a[1],q=a[2];return[0,gE(g,f,mp),p,q]}if(3===m){var
r=a[1],n=j(a9),s=k===n?a9[1]:e===n?b(i[2],a9):a9;if(h(l[an],g,f,s)){var
o=j(x),t=k===o?x[1]:e===o?b(i[2],x):x;if(ms(d,r,t)){var
u=K(a,2)[3];return[0,0,K(a,1)[2],u]}}throw P}throw P}function
EQ(d,c){var
a=c[2],f=c[1],g=d[2],m=a.length-1;if(2===m){var
p=a[1],q=a[2];return[0,gE(g,f,mq),p,q]}if(3===m){var
r=a[1],s=a[2],t=a[3],n=j(a9),u=k===n?a9[1]:e===n?b(i[2],a9):a9;if(h(l[an],g,f,u)){var
o=j(at),v=k===o?at[1]:e===o?b(i[2],at):at;if(ms(d,r,v))return[0,0,s,t]}throw P}throw P}function
ER(c,b){var
a=b[2],d=b[1];if(2===a.length-1){var
e=K(a,1)[2],f=K(a,0)[1];return[0,gE(c[2],d,mr),f,e]}throw P}function
gF(a){return[0,0,a]}function
mt(c,g,f){var
d=c[2],e=F(l[107],c[1],d,g,f);if(e){var
i=e[1],j=b(mu[152],d),k=h(ET[4],0,j,i);try{var
m=a(mu[37],d,k)}catch(a){a=q(a);if(a[1]===EU[26])return 0;throw a}return[0,[0,c[1],m]]}return 0}function
gG(c,d){function
f(d,a,c,b){if(a){var
g=a[1],i=a[2],h=mt(d,g,b);if(h)return[0,h[1],a,c];var
e=f(d,i,c+1|0,b);return[0,e[1],[0,g,e[2]],e[3]]}return[0,d,[0,b,0],c]}var
a=f(c[2],c[1],1,d),e=a[2],g=a[1];return[0,[0,e,g],b(B[1],a[3])]}function
mv(c,d){var
a=c[1],b=1,e=c[2];for(;;){if(a){var
f=a[2];if(mt(e,a[1],d))return b;var
a=f,b=b+1|0;continue}throw[0,lE,EV]}}function
iU(o,p,B,A,d,c){function
s(c,b){var
a=gG(c,b);return[0,[1,a[2]],a[1]]}function
f(c,d){function
C(g,e,b){var
h=b[2],c=f(g,b[1]),i=c[1],d=f(c[2],h),j=d[2];return[0,a(e,i,d[1]),j]}try{var
J=[0,[0,a(p,o,d)],c];return J}catch(p){p=q(p);if(p===P){var
r=a(l[3],o[2],d);if(9===r[0]){var
m=r[2],t=r[1];if(10===a(l[3],o[2],t)[0]){var
D=o[2];try{var
y=function(d){var
a=d[1],c=j(a),f=k===c?a[1]:e===c?b(i[2],a):a;return h(l[an],D,t,f)},z=a(g[33],y,A)[2],n=z}catch(a){a=q(a);if(a!==O)throw a;var
n=ES}if(typeof
n==="number"){if(0===n){var
u=f(c,K(m,0)[1]);return[0,[5,u[1]],u[2]]}try{var
w=f(c,K(m,0)[1]),E=w[2],F=w[1],G=[0,a(B,F,K(m,1)[2]),E];return G}catch(a){a=q(a);if(a===P){var
v=gG(c,d);return[0,[1,v[2]],v[1]]}throw a}}else{if(0===n[0]){var
H=n[1],I=K(m,1)[2];return C(c,H,[0,K(m,0)[1],I])}var
x=gG(c,d);return[0,[1,x[2]],x[1]]}}return s(c,d)}return s(c,d)}throw p}}return f(d,c)}var
EW=[0,[0,ea,0],[0,[0,ec,1],0]],EX=[0,[0,eb,[0,function(b,a){return[4,b,a]}]],EW],EY=[0,[0,d$,[0,function(b,a){return[3,b,a]}]],EX],EZ=[0,[0,d_,[0,function(b,a){return[2,b,a]}]],EY],E0=[0,[0,ef,0],[0,[0,eh,1],0]],E1=[0,[0,eg,[0,function(b,a){return[4,b,a]}]],E0],E2=[0,[0,ee,[0,function(b,a){return[3,b,a]}]],E1],E3=[0,[0,ed,[0,function(b,a){return[2,b,a]}]],E2],E4=[0,[0,bW,0],[0,[0,aV,1],0]],E5=[0,[0,bx,[0,function(b,a){return[4,b,a]}]],E4],E6=[0,[0,bw,[0,function(b,a){return[3,b,a]}]],E5],E7=[0,[0,bv,[0,function(b,a){return[2,b,a]}]],E6];function
iV(d,c,b){return a(d,c[2],b)}function
mw(e,b,d){try{var
c=a(e,b,d);return c}catch(c){c=q(c);if(c===P){if(iS(b[1],b[2],d))return a(e,b,h(E8[6],b[1],b[2],d));throw P}throw c}}function
iW(a,b){return iV(mm,a,b)}function
mx(a,b){return iV(EA,a,b)}function
E9(a,b){return iV(iQ,a,b)}function
my(c){function
d(g,f){var
h=gF(c),e=bm(a(my(c),h,f)[1]);if(e){var
d=e[1];if(typeof
d!=="number"&&1===d[0])return E_;return[6,g,b(p[17],d)]}throw P}return function(a,b){return iU(c,iW,d,EZ,a,b)}}var
E$=0,Fa=[0,[0,bx,function(b,a){return[4,b,a]}],E$],Fb=[0,[0,bw,function(b,a){return[3,b,a]}],Fa],Fc=[0,[0,bv,function(b,a){return[2,b,a]}],Fb];function
Fe(m,d){var
c=m[2];function
f(n){var
o=a(l[3],c,n);switch(o[0]){case
9:var
d=o[2],g=o[1];try{var
C=gE(c,g,Fc),D=f(K(d,0)[1]),E=a(C,D,f(K(d,1)[2]));return E}catch(a){a=q(a);if(a===P){var
p=j(ci),x=k===p?ci[1]:e===p?b(i[2],ci):ci;if(h(l[an],c,g,x)){var
r=f(K(d,0)[1]);if(ax(aH(r),Fd))throw P;return[6,r]}var
s=j(aV),y=k===s?aV[1]:e===s?b(i[2],aV):aV;if(h(l[an],c,g,y)){var
z=[1,mw(E9,m,K(d,1)[2])];return[5,f(K(d,0)[1]),z]}var
t=j(ck),A=k===t?ck[1]:e===t?b(i[2],ck):ck;if(h(l[an],c,g,A))return[0,mx(m,K(d,0)[1])];var
u=j(cj),B=k===u?cj[1]:e===u?b(i[2],cj):cj;if(h(l[an],c,g,B))return[1,mw(iW,m,K(d,0)[1])];throw P}throw a}case
10:var
v=j(cf),F=k===v?cf[1]:e===v?b(i[2],cf):cf;if(h(l[an],c,n,F))return 0;var
w=j(cg),G=k===w?cg[1]:e===w?b(i[2],cg):cg;if(h(l[an],c,n,G))return 1;throw P;default:throw P}}return f(d)}function
Ff(d){function
a(c,e){var
a=iW(d,e);if(typeof
a!=="number"&&1===a[0]){if(0===c[0])return[0,hj(c[1],a)];throw P}return[6,c,b(p[17],a)]}return function(b,c){return iU(d,mx,a,E3,b,c)}}function
Fg(a){function
c(d,c){var
e=iQ(a[2],c);return[6,d,b(js[1],e)]}return function(b,d){return iU(a,Fe,c,E7,b,d)}}function
iX(n,g,m,k,c){var
d=a(l[3],c[2],k);if(9===d[0]){var
e=a(n,c,[0,d[1],d[2]]),o=e[3],p=e[1],i=h(g,c,m,e[2]),q=i[1],j=h(g,c,i[2],o);return[0,[0,q,p,j[1]],j[2]]}return b(f[3],Fh)}function
gH(a,b,c){return iX(EP,my,a,b,c)}function
gI(a,b,c){return iX(ER,Ff,a,b,c)}function
Fi(a,b,c){return iX(EQ,Fg,a,b,c)}function
Fj(b,a){return[2,b,a]}function
Fk(b,a){return[3,b,a]}function
Fl(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
Fm(b,a){return[5,b,0,a]}function
gJ(e,d,c,b){if(typeof
c!=="number"&&0===c[0])if(typeof
b!=="number"&&0===b[0])return[0,d];return a(e,c,b)}function
mz(o,n,g,f,d){var
m=o[2];function
L(e,d,c){try{var
a=h(n,e,c,o),f=a[2],g=a[1],i=[0,[1,g,[0,d,c]],f,b(bK[2],d)];return i}catch(a){a=q(a);if(b(df[18],a))return[0,[0,c],e,d];throw a}}function
c(g,f,d){var
n=a(l[3],m,d);switch(n[0]){case
6:var
E=n[3],Q=n[2],ag=0===n[1][1]?0:h(l[g1][13],m,1,E)?0:1;if(!ag){var
r=c(g,f,Q),R=r[1],s=c(r[2],r[3],E),S=s[3],T=s[2];return[0,gJ(Fm,d,R,s[1]),T,S]}break;case
9:var
p=n[2],q=n[1],G=p.length-1;if(!(3<=G))switch(G){case
0:break;case
1:var
U=p[1],H=j(b$),V=k===H?b$[1]:e===H?b(i[2],b$):b$;if(h(l[an],m,q,V)){var
t=c(g,f,U);return[0,[4,t[1]],t[2],t[3]]}break;default:var
u=p[1],v=p[2],I=j(a5),W=k===I?a5[1]:e===I?b(i[2],a5):a5;if(h(l[an],m,q,W)){var
w=c(g,f,u),X=w[1],x=c(w[2],w[3],v),Y=x[3],Z=x[2];return[0,gJ(Fj,d,X,x[1]),Z,Y]}var
J=j(a6),_=k===J?a6[1]:e===J?b(i[2],a6):a6;if(h(l[an],m,q,_)){var
y=c(g,f,u),$=y[1],z=c(y[2],y[3],v),aa=z[3],ab=z[2];return[0,gJ(Fk,d,$,z[1]),ab,aa]}var
K=j(fL),ac=k===K?fL[1]:e===K?b(i[2],fL):fL;if(h(l[an],m,q,ac)){var
A=c(g,f,u),ad=A[1],B=c(A[2],A[3],v),ae=B[3],af=B[2];return[0,gJ(Fl,d,ad,B[1]),af,ae]}}return L(g,f,d)}var
C=j(ca),N=k===C?ca[1]:e===C?b(i[2],ca):ca;if(h(l[an],m,d,N))return[0,0,g,f];var
D=j(a7),O=k===D?a7[1]:e===D?b(i[2],a7):a7;if(h(l[an],m,d,O))return[0,1,g,f];var
M=F(mk[3],0,o[1],o[2],d);if(b(Fn[10],M))return[0,[0,d],g,f];throw P}return c(g,f,d)}function
mA(g,m,a){function
c(a,h){var
c=j(a8),m=k===c?a8[1]:e===c?b(i[2],a8):a8,d=j(a8),n=[0,m,h],o=k===d?a8[1]:e===d?b(i[2],a8):a8,p=b(ml[12],[0,g,[0,l[16],[0,o,n]]]),f=j(a),q=k===f?a[1]:e===f?b(i[2],a):a;return b(l[23],[0,q,p])}function
d(a){if(typeof
a==="number")return 0===a?c(DR,0):c(DV,0);else
switch(a[0]){case
0:return c(Ed,[0,a[1],0]);case
1:var
n=a[1],f=j(fR),o=0,p=k===f?fR[1]:e===f?b(i[2],fR):fR;return c(D$,[0,b(m,n),[0,p,o]]);case
2:var
q=a[1],r=[0,d(a[2]),0];return c(DZ,[0,d(q),r]);case
3:var
s=a[1],t=[0,d(a[2]),0];return c(D3,[0,d(s),t]);case
4:return c(D7,[0,d(a[1]),0]);default:var
u=a[1],v=[0,d(a[3]),0],g=j(a8),w=k===g?a8[1]:e===g?b(i[2],a8):a8,h=j(fQ),x=[0,w],y=k===h?fQ[1]:e===h?b(i[2],fQ):fQ,z=[0,b(l[23],[0,y,x]),v];return c(Eh,[0,d(u),z])}}return d(a)}function
iY(b,a){function
d(h,g){var
b=h,a=g;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
0:return gG(b,a[1])[1];case
4:var
a=a[1];continue;case
5:var
f=a[3],e=a[1],c=1;break;case
1:var
c=0;break;default:var
f=a[2],e=a[1],c=1}if(c){var
b=d(b,e),a=f;continue}return b}}return d(gF(b),a)}var
gK=[e,function(w){function
n(c){var
a=c[1],f=c[2],d=j(a),g=k===d?a[1]:e===d?b(i[2],a):a;return[0,f,g]}var
o=a(g[17],n,mp);function
p(a){var
c=b(ai[4],a);return au(b(B[7],c))}var
c=j(ec),q=k===c?ec[1]:e===c?b(i[2],ec):ec,d=j(eb),r=k===d?eb[1]:e===d?b(i[2],eb):eb,f=j(ea),s=k===f?ea[1]:e===f?b(i[2],ea):ea,h=j(d$),t=k===h?d$[1]:e===h?b(i[2],d$):d$,l=j(d_),u=k===l?d_[1]:e===l?b(i[2],d_):d_,m=j(x),v=k===m?x[1]:e===m?b(i[2],x):x;return[0,v,au,u,t,s,r,q,p,o]}],gL=[e,function(w){function
n(c){var
a=c[1],f=c[2],d=j(a),g=k===d?a[1]:e===d?b(i[2],a):a;return[0,f,g]}var
o=a(g[17],n,mr);function
p(a){var
c=b(ai[4],a);return au(b(B[7],c))}var
c=j(eh),q=k===c?eh[1]:e===c?b(i[2],eh):eh,d=j(eg),r=k===d?eg[1]:e===d?b(i[2],eg):eg,f=j(ef),s=k===f?ef[1]:e===f?b(i[2],ef):ef,h=j(ee),t=k===h?ee[1]:e===h?b(i[2],ee):ee,l=j(ed),u=k===l?ed[1]:e===l?b(i[2],ed):ed,m=j(aL),v=k===m?aL[1]:e===m?b(i[2],aL):aL;return[0,v,cO,u,t,s,r,q,p,o]}];function
bj(a){if(typeof
a==="number"){if(0===a){var
d=j(cf);return k===d?cf[1]:e===d?b(i[2],cf):cf}var
f=j(cg);return k===f?cg[1]:e===f?b(i[2],cg):cg}else
switch(a[0]){case
0:var
u=[0,cO(a[1])],g=j(ck),v=k===g?ck[1]:e===g?b(i[2],ck):ck;return b(l[23],[0,v,u]);case
1:var
w=[0,au(a[1])],h=j(cj),x=k===h?cj[1]:e===h?b(i[2],cj):cj;return b(l[23],[0,x,w]);case
2:var
y=a[1],z=bj(a[2]),A=[0,bj(y),z],m=j(bv),B=k===m?bv[1]:e===m?b(i[2],bv):bv;return b(l[23],[0,B,A]);case
3:var
C=a[1],D=bj(a[2]),E=[0,bj(C),D],n=j(bw),F=k===n?bw[1]:e===n?b(i[2],bw):bw;return b(l[23],[0,F,E]);case
4:var
G=a[1],H=bj(a[2]),I=[0,bj(G),H],o=j(bx),J=k===o?bx[1]:e===o?b(i[2],bx):bx;return b(l[23],[0,J,I]);case
5:var
c=a[2],p=a[1];if(0===c[0]){var
K=au(c[1]),L=[0,bj(p),K],q=j(ge),M=k===q?ge[1]:e===q?b(i[2],ge):ge;return b(l[23],[0,M,L])}var
N=ei(c[1]),O=[0,bj(p),N],r=j(aV),P=k===r?aV[1]:e===r?b(i[2],aV):aV;return b(l[23],[0,P,O]);case
6:var
Q=[0,bj(a[1])],s=j(ci),R=k===s?ci[1]:e===s?b(i[2],ci):ci;return b(l[23],[0,R,Q]);default:var
S=[0,bj(a[1])],t=j(bW),T=k===t?bW[1]:e===t?b(i[2],bW):bW;return b(l[23],[0,T,S])}}var
gM=[e,function(w){function
n(c){var
a=c[1],f=c[2],d=j(a),g=k===d?a[1]:e===d?b(i[2],a):a;return[0,f,g]}var
o=a(g[17],n,mq);function
p(a){var
c=b(ai[4],a);return ei(b(B[4],c))}var
c=j(aV),q=k===c?aV[1]:e===c?b(i[2],aV):aV,d=j(bx),r=k===d?bx[1]:e===d?b(i[2],bx):bx,f=j(bW),s=k===f?bW[1]:e===f?b(i[2],bW):bW,h=j(bw),t=k===h?bw[1]:e===h?b(i[2],bw):bw,l=j(bv),u=k===l?bv[1]:e===l?b(i[2],bv):bv,m=j(at),v=k===m?at[1]:e===m?b(i[2],at):at;return[0,v,bj,u,t,s,r,q,p,o]}];function
mB(i,h,g){var
c=[0,i,h,g];for(;;){var
e=c[1];if(0===e)return c[3];var
d=c[2];if(d){var
f=d[1],j=c[3],k=d[2],m=f[2],n=[0,a(mC[4],f[1],0),m,j],c=[0,e-1|0,k,b(l[20],n)];continue}throw[0,a0,Fo]}}function
mD(x,d,c){function
n(d){var
c=d;for(;;)switch(c[0]){case
0:return u[1];case
1:var
e=b(ai[3],c[1]);return b(u[5],e);case
5:case
6:var
c=c[1];continue;default:var
f=c[1],g=n(c[2]),h=n(f);return a(u[7],h,g)}}function
p(j){var
b=j;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
1:var
d=b[1],g=d[1],h=n(d[3]),i=n(g);return a(u[7],i,h);case
4:var
b=b[1];continue;case
5:var
f=b[3],e=b[1],c=1;break;case
0:var
c=0;break;default:var
f=b[2],e=b[1],c=1}if(c){var
k=p(f),l=p(e);return a(u[7],l,k)}return u[1]}}var
w=p(c),y=b(u[21],w);function
z(b,a){return[0,a,b+1|0]}var
r=a(g[18],z,y),s=iY(x,c);function
A(c){var
e=d[1],f=a(m[4],Fp,c[2]);return[0,b(b_[1][6],f),e]}var
o=a(g[17],A,r),B=s[1];function
C(c,f){var
d=l[16],e=a(m[4],Fq,c+1|0);return[0,b(b_[1][6],e),d]}var
t=a(g[18],C,B);function
D(b,a){return[0,a[1],b[1]]}var
E=h(g[23],D,r,o);function
v(f,c){function
e(c){switch(c[0]){case
0:return b(d[2],c[1]);case
1:var
h=b(ai[3],c[1]),i=f+a(g[38],h,r)|0;return b(l[10],i);case
2:var
j=c[1],k=e(c[2]),m=[0,e(j),k];return b(l[23],[0,d[3],m]);case
3:var
n=c[1],o=e(c[2]),p=[0,e(n),o];return b(l[23],[0,d[4],p]);case
4:var
q=c[1],s=e(c[2]),t=[0,e(q),s];return b(l[23],[0,d[6],t]);case
5:var
u=[0,e(c[1])];return b(l[23],[0,d[5],u]);default:var
v=c[1],w=b(d[8],c[2]),x=[0,e(v),w];return b(l[23],[0,d[7],x])}}return e(c)}function
F(m,f,c){try{var
p=[0,a(g[38],m,d[9]),[0,f,c]],r=b(l[23],p);return r}catch(a){a=q(a);if(a===O){var
n=[0,d[1],f,c],h=j(a9),o=k===h?a9[1]:e===h?b(i[2],a9):a9;return b(l[23],[0,o,n])}throw a}}function
f(d,c,a){if(typeof
a==="number"){if(0===a){var
m=j(ca);return k===m?ca[1]:e===m?b(i[2],ca):ca}var
n=j(a7);return k===n?a7[1]:e===n?b(i[2],a7):a7}else
switch(a[0]){case
0:var
w=d+mv(s,a[1])|0;return b(l[10],w);case
1:var
g=a[1],r=g[2],t=g[1],u=v(c,g[3]);return F(r,v(c,t),u);case
2:var
x=a[1],y=f(d,c,a[2]),z=[0,f(d,c,x),y],o=j(a5),A=k===o?a5[1]:e===o?b(i[2],a5):a5;return b(l[23],[0,A,z]);case
3:var
B=a[1],C=f(d,c,a[2]),D=[0,f(d,c,B),C],p=j(a6),E=k===p?a6[1]:e===p?b(i[2],a6):a6;return b(l[23],[0,E,D]);case
4:var
G=a[1],q=j(a7),H=k===q?a7[1]:e===q?b(i[2],a7):a7,I=f(d,c,G);return h(l[35],I,0,H);default:var
J=a[1],K=f(d+1|0,c+1|0,a[3]),L=f(d,c,J);return h(l[35],L,0,K)}}var
G=b(g[1],o),H=b(g[1],t),I=bF(function(c){var
d=mv(s,c),e=a(m[4],Fr,d),f=b(b_[1][6],e);return b(l[11],f)},c),J=b(g[9],E),K=b(g[9],t),L=f(b(g[1],o),0,c);function
M(a){return[0,[0,a[1]],a[2]]}var
N=mB(G,a(g[17],M,o),L);function
P(a){return[0,[0,a[1]],a[2]]}return[0,mB(H,a(g[17],P,t),N),K,J,I]}function
mE(g,f){var
d=f,c=g;for(;;){if(c){var
e=c[1],h=c[2],i=e[3],j=e[2],k=b(b_[1][6],e[1]),m=a(mC[4],k,0),d=F(l[47],m,j,i,d),c=h;continue}return d}}var
gN=[e,function(a){return Y(Fu,Ft,Fs)}],gO=[e,function(a){return Y(Fx,Fw,Fv)}],gP=[e,function(a){return Y(FA,Fz,Fy)}],gQ=[e,function(a){return Y(FD,FC,FB)}];function
gR(c,a){if(typeof
a==="number"){var
d=j(gP),h=[0,c],m=k===d?gP[1]:e===d?b(i[2],gP):gP;return b(l[23],[0,m,h])}else{if(0===a[0]){var
n=[0,c,a[1]],f=j(gO),o=k===f?gO[1]:e===f?b(i[2],gO):gO;return b(l[23],[0,o,n])}var
p=a[2],q=a[1],r=gR(c,a[3]),s=[0,c,gR(c,q),p,r],g=j(gN),t=k===g?gN[1]:e===g?b(i[2],gN):gN;return b(l[23],[0,t,s])}}function
mF(a){if(a){var
c=a[1][1],d=0,e=function(d,a){var
e=a[1];return eA(c,b(B[1],a[2]),e,d)};return h(g[20],e,d,a)}return 0}function
gS(a){if(typeof
a==="number"){var
c=j(ga);return k===c?ga[1]:e===c?b(i[2],ga):ga}else
switch(a[0]){case
0:var
m=a[1],n=gS(a[2]),o=[0,dh(x,au,m),n],d=j(gb),p=k===d?gb[1]:e===d?b(i[2],gb):gb;return b(l[23],[0,p,o]);case
1:var
q=a[1],r=gS(a[2]),s=[0,dh(x,au,q),r],f=j(gc),t=k===f?gc[1]:e===f?b(i[2],gc):gc;return b(l[23],[0,t,s]);default:var
u=a[3],v=a[2],w=a[1],g=j(ch),y=k===g?ch[1]:e===g?b(i[2],ch):ch,z=iT(y,gS,u),A=dh(x,au,v),B=[0,dh(x,au,w),A,z],h=j(gd),C=k===h?gd[1]:e===h?b(i[2],gd):gd;return b(l[23],[0,C,B])}}function
mG(a){return gS(a)}function
bB(b,a){return Z(m[1],b,FE,cn,a[1],iR,a[2])}function
cP(c,b){if(typeof
b==="number")return a(m[1],c,FF);else
switch(b[0]){case
0:var
d=b[2],e=b[1],f=function(a,b){return bY(cn,a,b)};return Z(m[1],c,FG,f,e,cP,d);case
1:var
g=b[2],h=b[1],i=function(a,b){return bY(cn,a,b)};return Z(m[1],c,FH,i,h,cP,g);default:var
j=b[3],k=b[2],l=b[1],n=function(a,c){function
b(c,a){if(a){var
d=a[2],e=a[1];return d?Z(m[1],c,EB,cP,e,b,d):F(m[1],c,EC,cP,e)}return 0}return Z(m[1],a,ED,FJ,b,c,FI)},o=function(a,b){return bY(cn,a,b)},p=function(a,b){return bY(cn,a,b)};return m7(m[1],c,FK,p,l,o,k,n,j)}}function
mH(h,g,f,e,a){if(a){var
i=a[1],m=i[2],n=i[1],c=mH(h,g,f,e,a[2]),j=c[3],k=c[2],l=c[1];try{var
d=mz(h,g,k,j,m),o=[0,[0,[0,n,d[1]],l],d[2],d[3]];return o}catch(a){a=q(a);if(b(df[18],a))return[0,l,k,j];throw a}}return[0,0,f,e]}function
mI(d,c,h,g,f){var
a=mz(d,c,h,b(bK[4],0),f),i=a[1],e=mH(d,c,a[2],a[3],g);return[0,e[1],i,e[2]]}var
gT=[e,function(l){var
a=j(ch),f=k===a?ch[1]:e===a?b(i[2],ch):ch,c=j(x),g=k===c?x[1]:e===c?b(i[2],x):x,d=j(x),h=k===d?x[1]:e===d?b(i[2],x):x;return[0,h,g,au,f,mG]}],gU=[e,function(m){function
f(a){return dh(aL,cO,a)}var
a=j(a_),g=k===a?a_[1]:e===a?b(i[2],a_):a_,c=j(aL),h=k===c?aL[1]:e===c?b(i[2],aL):aL,d=j(aL),l=k===d?aL[1]:e===d?b(i[2],aL):aL;return[0,l,h,cO,g,f]}];function
FL(d,c){function
f(a){return(2*a|0)+1|0}return j2(function(d,c,h){var
o=b(ai[3],c),g=b(ai[3],d)+o|0;if(h){if(0===h[1]){var
p=[1,a(w[2],d,c)],q=-(2*f(g)|0)|0,r=b(bK[4],q),s=dJ([1,p]),l=j(x),t=k===l?x[1]:e===l?b(i[2],x):x;return[0,r,ek(t,au,s)]}var
u=[0,a(w[2],d,c)],v=-f(f(g))|0,y=b(bK[4],v),z=dJ([0,u]),m=j(x),A=k===m?x[1]:e===m?b(i[2],x):x;return[0,y,ek(A,au,z)]}var
B=[1,a(w[2],d,c)],C=[0,a(w[2],d,c)],D=b(bK[4],-(2*(2*g|0)|0)|0),E=hq(c,B,C),n=j(x),F=k===n?x[1]:e===n?b(i[2],x):x;return[0,D,ek(F,au,E)]},d,c)}function
FM(a,m,h,g,f){var
n=[0,a[2]],c=j(cm),o=k===c?cm[1]:e===c?b(i[2],cm):cm,d=b(l[23],[0,o,n]),p=a[3],q=a[2],r=mA(d,function(a){return ek(q,p,a)},f),s=mF(g),t=gR(a[1],s);function
u(g){var
o=b(a$[35][6],g),q=[0,a[1]],c=j(gQ),n=0,p=[0,[0,FN,m,h],0],s=k===c?gQ[1]:e===c?b(i[2],gQ):gQ,u=[0,[0,FO,t,b(l[23],[0,s,q])],p],f=j(cl),v=[0,d],w=k===f?cl[1]:e===f?b(i[2],cl):cl,x=mE([0,[0,FP,r,b(l[23],[0,w,v])],u],o),y=[0,b(aM[54],x),n];return b(Q[65][22],y)}return b(di[68][8],u)}function
co(c,a){if(typeof
c==="number"){if(0===c){if(typeof
a==="number")if(0===a)return 0}else
if(typeof
a==="number")if(0!==a)return 1}else
switch(c[0]){case
0:return[0,c[1]];case
1:if(typeof
a!=="number"&&1===a[0])return a;break;case
2:if(typeof
a!=="number"&&2===a[0]){var
d=a[1],e=c[1],g=co(c[2],a[2]);return[2,co(e,d),g]}break;case
3:if(typeof
a!=="number"&&3===a[0]){var
h=a[1],i=c[1],j=co(c[2],a[2]);return[3,co(i,h),j]}break;case
4:if(typeof
a!=="number"&&4===a[0])return[4,co(c[1],a[1])];break;default:if(typeof
a!=="number"&&5===a[0]){var
k=a[2],l=a[1],m=c[1],n=co(c[3],a[3]);return[5,co(m,l),k,n]}}return b(f[3],FV)}var
iZ=[aX,FW,aW(0)];function
mJ(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return h(g[21],d,b,c)}function
FX(w,v,t,o,W,I,H,V){var
p=mJ(I,H)[1],A=b(bK[4],0),C=dz(function(c,b){return a(bK[3],c,b[1])},p,A),D=1+b(bK[5],C)|0,x=b(B[1],D),y=b(v,a(w,x,p)),r=y[1],J=y[2];function
s(f){if(f){var
h=f[1],d=s(f[2]);if(typeof
d==="number")return 0;else{if(0===d[0]){var
m=d[1],j=function(a){return a[1]},k=a(g[17],j,h),l=[0,b(o[2],0),k],c=b(o[3],l),e=typeof
c==="number"?0:0===c[0]?[0,[0,c[1],o]]:[1,c[1]];return typeof
e==="number"?0:0===e[0]?[0,[0,e[1],m]]:[1,[0,e[1],h]]}var
i=d[1];return[1,[0,i[1],i[2]]]}}return FQ}var
c=s(r);if(typeof
c==="number")return 0;else{if(0===c[0]){var
z=c[1],K=a(g[47],r,z),L=function(a){return a[1]},M=a(g[17],L,J),N=cz[1],P=function(c,b){return a(cz[4],b,c)},Q=h(g[20],P,N,M),R=function(e,c){var
d=c[2],f=c[1],i=cz[1],j=b(d[2][4],d[1]);function
k(c,b){var
d=a(g[7],f,c)[2][1];return a(cz[4],d,b)}var
l=h(u[15],k,j,i);return a(cz[7],e,l)},S=h(g[20],R,Q,K),d=function(c){if(typeof
c==="number")return 0===c?0:1;else
switch(c[0]){case
0:return[0,c[1]];case
1:var
r=c[2],s=r[2],t=r[1],y=c[1];return a(cz[3],t,S)?[1,y,[0,t,s]]:[0,s];case
2:var
z=c[2],g=d(c[1]),m=d(z);if(typeof
g!=="number"&&0===g[0])if(typeof
m!=="number"&&0===m[0]){var
A=[0,g[1],m[1]],u=j(a5),B=k===u?a5[1]:e===u?b(i[2],a5):a5;return[0,b(l[23],[0,B,A])]}return[2,g,m];case
3:var
C=c[2],n=d(c[1]),o=d(C);if(typeof
n!=="number"&&0===n[0])if(typeof
o!=="number"&&0===o[0]){var
D=[0,n[1],o[1]],v=j(a6),E=k===v?a6[1]:e===v?b(i[2],a6):a6;return[0,b(l[23],[0,E,D])]}return[3,n,o];case
4:var
p=d(c[1]);if(typeof
p!=="number"&&0===p[0]){var
F=[0,p[1]],w=j(b$),G=k===w?b$[1]:e===w?b(i[2],b$):b$;return[0,b(l[23],[0,G,F])]}return[4,p];default:var
x=c[2],H=c[3],q=d(c[1]),f=d(H);if(typeof
q!=="number"&&0===q[0]){var
I=q[1];if(x)return f;if(typeof
f!=="number"&&0===f[0])return[0,h(l[35],I,0,f[1])]}return[5,q,x,f]}},n=d(p),T=b(v,a(w,x,n))[1],E=a(g[47],r,z),F=function(l){try{var
y=function(c){var
e=c[2],f=c[1],i=b(e[2][4],e[1]);function
d(g,f){var
c=g,b=f;for(;;){if(b){var
e=b[2],h=b[1];if(a(u[3],c,i))return[0,h,d(c+1|0,e)];var
c=c+1|0,b=e;continue}return 0}}return kb(_,d(0,f),l)},z=a(g[33],y,E),c=z}catch(a){a=q(a);if(a!==O)throw a;b(m[2],FT);b(f[52],f[28]);var
c=b(f[3],FU)}var
n=c[2],d=n[2],x=c[1],o=n[1];function
p(b,a){return[0,a[1],b]}var
i=a(g[18],p,l);function
r(d){try{var
e=a(g[7],x,d)[1],c=e}catch(a){a=q(a);if(a[1]!==eX)throw a;var
c=b(f[3],FR)}return a(g[38],c,i)}try{var
w=a(d[5],o,r),k=w}catch(c){c=q(c);if(!b(df[18],c))throw c;var
s=function(a){return a[1]},t=a(g[17],s,i),v=[0,b(d[2],0),t],e=b(d[3],v);if(typeof
e==="number")var
h=0;else
if(0===e[0])var
j=e[1],h=1;else
var
h=0;if(!h)var
j=b(f[3],FS);var
k=j}return k},G=a(g[17],F,T),U=hc(n);return[0,[0,U,n,iT(t[4],t[5],G)]]}return[1,c[1]]}}function
mK(j,i,h,g,e,d,c,a){try{var
k=FX(j,i,h,g,e,d,c,a);return k}catch(a){a=q(a);if(a===O){b(eG[4],f[28]);b(f[52],f[28]);return 0}throw a}}function
mL(d,c,a){var
e=b(di[68][4],a);return h(aM[13],d,c,e)}function
cp(H,G,E,m,h,D,C){function
c(c){var
I=b(a$[35][4],c),J=b(a$[35][6],c),K=b(a$[35][14],c);try{var
d=[0,b(a$[35][5],c),I],o=mI(d,H,gF(d),K,J),v=o[3][1],T=o[2],U=o[1],w=j(m),p=k===w?m[1]:e===w?b(i[2],m):m,x=j(h),V=k===x?h[1]:e===x?b(i[2],h):h,r=mK(G,E,p,D,v,U,T,d);if(typeof
r==="number"){b(f[52],f[28]);var
W=b(bA[3],F4),s=a(Q[65][4],0,W)}else
if(0===r[0])var
t=r[1],y=t[2],X=t[3],Y=t[1],n=mD(d,V,y),u=n[3],Z=n[4],_=n[2],$=n[1],z=function(a){return b(aM[2],a[1])},aa=a(g[17],z,u),ab=b(Q[65][22],aa),ac=a(g[17],z,_),ad=b(Q[65][22],ac),ae=function(b){return[0,a(mM[1],0,[1,[0,b]])]},af=b(b_[1][6],F5),A=mL(b_[1][10][1],af,c),ag=function(a){var
c=a[2];return[0,b(l[11],a[1]),c]},ah=a(g[17],ag,u),aj=[0,p[4]],B=j(cb),ai=0,ak=k===B?cb[1]:e===B?b(i[2],cb):cb,al=[0,ad,[0,ab,[0,FM(p,X,b(l[23],[0,ak,aj]),ah,Z),ai]]],am=b(Q[65][22],al),an=iY(d,y)[1],ao=b(g[9],an),ap=function(b){return a(g[7],v,b[2]-1|0)},aq=a(g[17],ap,u),ar=a(f[26],ao,aq),as=a(Q[65][3],am,C),at=b(aM[79],0),au=a(Q[65][3],at,as),av=[0,b(l[11],A),ar],aw=b(l[40],av),ax=[0,b(aM[46],aw),0],ay=a(g[17],l[11],Y),az=[0,b(aM[ne],ay),ax],aA=[0,au,[0,b(Q[65][22],az),0]],aB=ae(A),aC=F(aM[n0],1,F6,aB,$),s=a(Q[65][21],aC,aA);else
var
aD=b(bA[3],F7),s=a(Q[65][4],0,aD);return s}catch(c){c=q(c);if(c===P){var
L=b(bA[3],FY);return a(Q[65][4],0,L)}if(c===fF){var
M=b(bA[3],FZ);return a(Q[65][4],0,M)}if(c===iZ){b(f[52],f[28]);var
N=a(f[17],F1,F0),O=a(f[17],F2,N),R=a(f[17],F3,O),S=b(bA[3],R);return a(Q[65][4],0,S)}throw c}}return b(di[68][8],c)}function
F8(q,p,o){var
a=j(ce),c=k===a?ce[1]:e===a?b(i[2],ce):ce,d=j(at),f=k===d?at[1]:e===d?b(i[2],at):at,g=j(a_),r=k===g?a_[1]:e===g?b(i[2],a_):a_,h=j(cb),s=[0,r],t=k===h?cb[1]:e===h?b(i[2],cb):cb,u=b(l[23],[0,t,s]),m=j(cm),v=[0,c],w=k===m?cm[1]:e===m?b(i[2],cm):cm,n=b(l[23],[0,w,v]),x=mA(n,function(a){return ek(c,by,a)},o),y=gR(f,mF(p));function
z(c){var
g=b(a$[35][6],c),h=[0,Y(Ga,F$,F_),[0,f]],m=[0,[0,Gb,y,b(l[23],h)],[0,[0,F9,q,u],0]],a=j(cl),d=0,o=[0,n],p=k===a?cl[1]:e===a?b(i[2],cl):cl,r=mE([0,[0,Gc,x,b(l[23],[0,p,o])],m],g),s=[0,b(aM[54],r),d];return b(Q[65][22],s)}return b(di[68][8],z)}function
gV(aA){return function(aB){var
h=[e,function(m){function
f(a){return dh(aL,cO,a)}var
a=j(a_),g=k===a?a_[1]:e===a?b(i[2],a_):a_,c=j(ce),h=k===c?ce[1]:e===c?b(i[2],ce):ce,d=j(at),l=k===d?at[1]:e===d?b(i[2],at):at;return[0,l,h,cO,g,f]}];function
c(c){var
D=b(a$[35][4],c),E=b(a$[35][6],c),G=b(a$[35][14],c);try{var
d=[0,b(a$[35][5],c),D],n=mI(d,Fi,gF(d),G,E),t=n[2],u=n[1],v=n[3][1],w=j(h),N=k===w?h[1]:e===w?b(i[2],h):h,O=function(a){var
b=a[2],c=a[1];return[0,c,ba(function(a){return ez(aH,a)},b)]},R=a(g[17],O,u),S=ba(function(a){return ez(aH,a)},t),o=mK(function(b,a){return a},cZ,N,aA,v,R,S,d);if(typeof
o==="number")var
s=0;else
if(0===o[0])var
p=o[1],U=p[3],V=p[2],W=p[1],X=function(b){return a(g[31],b[1],W)},y=mJ(a(g[35],X,u),t),Y=y[2],z=co(V,y[1]),A=j(gM),Z=k===A?gM[1]:e===A?b(i[2],gM):gM,m=mD(d,Z,z),r=m[3],_=m[4],$=m[2],aa=m[1],B=function(a){return b(aM[2],a[1])},ab=a(g[17],B,r),ac=b(Q[65][22],ab),ad=a(g[17],B,$),ae=b(Q[65][22],ad),af=function(b){return[0,a(mM[1],0,[1,[0,b]])]},ag=b(b_[1][6],Gk),C=mL(b_[1][10][1],ag,c),ah=function(a){var
c=a[2];return[0,b(l[11],a[1]),c]},ai=[0,ae,[0,ac,[0,F8(U,a(g[17],ah,r),_),0]]],aj=b(Q[65][22],ai),ak=iY(d,z)[1],al=b(g[9],ak),am=function(b){return a(g[7],v,b[2]-1|0)},an=a(g[17],am,r),ao=a(f[26],al,an),ap=a(Q[65][3],aj,aB),aq=b(aM[79],0),ar=a(Q[65][3],aq,ap),as=[0,b(l[11],C),ao],at=b(l[40],as),au=[0,b(aM[46],at),0],av=a(g[17],l[11],Y),aw=[0,b(aM[ne],av),au],ax=[0,ar,[0,b(Q[65][22],aw),0]],ay=af(C),az=F(aM[n0],1,Gl,ay,aa),x=a(Q[65][21],az,ax),s=1;else
var
s=0;if(!s){b(f[52],f[28]);var
T=b(bA[3],Gj),x=a(Q[65][4],0,T)}return x}catch(c){c=q(c);if(c===P){var
H=b(bA[3],Gd);return a(Q[65][4],0,H)}if(c===fF){var
I=b(bA[3],Ge);return a(Q[65][4],0,I)}if(c===iZ){b(f[52],f[28]);var
J=a(f[17],Gg,Gf),K=a(f[17],Gh,J),L=a(f[17],Gi,K),M=b(bA[3],L);return a(Q[65][4],0,M)}throw c}}return b(di[68][8],c)}}var
Gm=fD([0,_,bn[27]]),mN=b(Gp[8],Go)?0:[e,function(a){throw iZ}];function
Gv(m){var
p=m[2],q=m[1],d=j(mN);if(k!==d)if(e===d)b(i[2],mN);var
n=[0,Gt,[0,Gs,[0,a(f[17],Gr,Gq[22]),0]]],o=b(Gu[3],0),l=h(g[20],bM[4],o,n),c=kl(l,[0,l],[0,q,p]);if(0===c[0])return c[1];throw b(f[3],c[1])}var
Gw=a(Gm[4],Gn,Gv);function
mO(c,a){return b(Gw,[0,c,a])}function
gW(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,gW(a[2])];default:var
c=a[2],d=a[1],e=gW(a[3]);return[2,gW(d),c,e]}}function
mP(e,a){var
c=mO(e,a);if(c){var
d=l7(c[1]);return hs(a,d)?[0,d]:(b(f[31],Gx),0)}return 0}function
el(e,d,c){function
f(i,h){var
c=i,d=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
g=b(ai[5],c[1]);return e<=g?a(u[4],g-e|0,d):d;case
2:var
c=c[2];continue;case
3:case
4:var
j=c[1],k=f(c[2],d),c=j,d=k;continue}return d}}return f(c,d)}function
em(a){return el(0,u[1],a)}function
bZ(a,d){function
c(a){if(typeof
a!=="number")switch(a[0]){case
0:var
e=b(d,b(ai[5],a[1]));return[0,b(B[4],e)];case
2:var
f=a[1];return[2,f,c(a[2])];case
3:var
g=a[1],h=c(a[2]);return[3,c(g),h];case
4:var
i=a[1],j=c(a[2]);return[4,c(i),j]}return a}return c(a)}function
i0(a){function
d(i,f,e){var
b=i,a=f,c=e;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
j=a[2],k=el(b,c,a[1]),b=b+1|0,a=j,c=k;continue;case
1:var
l=a[2],m=el(b,c,a[1]),b=b+1|0,a=l,c=m;continue;default:var
n=a[3],o=a[1],p=el(b,el(b,c,a[2]),o),q=function(c,a){return d(b+1|0,a,c)};return h(g[20],q,p,n)}}return d(0,a,u[1])}function
i1(a,e){function
c(c,a){return a<c?a:b(e,a-c|0)+c|0}function
d(b,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
e=a[1],f=d(b+1|0,a[2]);return[0,bZ(e,function(a){return c(b,a)}),f];case
1:var
g=a[1],h=d(b+1|0,a[2]);return[1,bZ(g,function(a){return c(b,a)}),h];default:var
i=a[3],j=a[2],k=a[1],l=bl(function(a){return d(b+1|0,a)},i),m=bZ(j,function(a){return c(b,a)});return[2,bZ(k,function(a){return c(b,a)}),m,l]}}return d(0,a)}function
en(d,c){function
e(a){var
b=a[2];return[0,jP(a[1]),b]}return b(d,a(g[17],e,c))}var
mQ=fD([0,_,bn[27]]),Gz=fD([0,_,bn[27]]);function
GA(a){var
b=a[1],c=a[2],d=b[3],e=b[2];return en(function(a){return mc(e,d,a)},c)}var
GC=a(mQ[4],GB,GA);function
GD(a){var
b=a[1],c=a[2],d=b[3],e=b[2];return en(function(a){return md(e,d,a)},c)}var
GF=a(mQ[4],GE,GD);function
GG(a){var
b=a[2],c=a[1];return en(function(a){return l4(c,a)},b)}var
GI=a(Gz[4],GH,GG);function
GJ(b,a){return bz(bB,b,a[1])}function
GK(a,b){return bY(bB,a,b)}var
GM=[0,GL,iO,function(a){var
b=a[2],c=a[1];return en(function(a){return iH(c,a)},b)},em,bZ,GK,GJ];function
GN(b,a){return bz(bB,b,a[1])}function
GO(a,b){return bY(bB,a,b)}var
GQ=[0,GP,iO,function(a){var
b=a[2],c=a[1];return en(function(a){return iH(c,a)},b)},em,bZ,GO,GN];function
GR(b,a){return bz(bB,b,a[1])}var
mR=[0,GS,iO,GI,em,bZ,function(a,b){return bY(bB,a,b)},GR];function
mS(b,a){function
c(b,a){return bz(bB,b,a[1])}function
d(a,b){return bY(bB,a,b)}function
e(a){return mP(a[1],a[2])}return[0,GT,function(c){return[0,b,a]},e,em,bZ,d,c]}function
mT(b,a){function
c(b,a){return bz(bB,b,a[1])}function
d(a,b){return bY(bB,a,b)}function
e(a){return mP(a[1],a[2])}return[0,GU,function(c){return[0,b,a]},e,em,bZ,d,c]}function
mU(d,c){function
e(b,a){return bz(cn,b,a[1])}function
h(h){var
i=h[2],k=h[1];function
j(a){var
b=a[2];return[0,gW(a[1]),b]}var
d=mO(k,a(g[17],j,i));if(d)var
e=l9(d[1]),c=jR(i,e)?[0,e]:(b(f[31],Gy),b(f[52],f[28]),0);else
var
c=0;if(typeof
c!=="number"&&0===c[0])return[0,[0,c[1],0]];return 0}return[0,GV,function(a){return[0,d,c]},h,i0,i1,cP,e]}var
GX=[0,GW,mg,GC,i0,i1,cP,function(b,a){return bz(cn,b,a[1])}],GZ=[0,GY,mg,GF,i0,i1,cP,function(b,a){return bz(cn,b,a[1])}];function
G0(b,a){return a}function
mV(a){return cp(gI,G0,cZ,gU,gL,GM,a)}function
i2(a){var
b=mS(G1,[0,a]);function
c(b,a){return a}return function(a){return cp(gI,c,cZ,gU,gL,b,a)}}var
mW=gV(GQ);function
i3(a){return gV(mT(G2,[0,a]))}function
i4(a){var
b=mU(G3,[0,a]);function
c(b,a){return a}return function(a){return cp(gH,c,dF,gT,gK,b,a)}}var
G5=mU(G4,0);function
G6(b,a){return a}function
mX(a){return cp(gH,G6,dF,gT,gK,G5,a)}var
G8=mS(G7,0);function
G9(b,a){return a}function
mY(a){return cp(gI,G9,cZ,gU,gL,G8,a)}var
mZ=gV(mT(G_,0));function
m0(a){return cp(gH,FL,dF,gT,gK,GX,a)}function
G$(b,a){return a}function
m1(a){return cp(gH,G$,dF,gT,gK,GZ,a)}var
m2=gV(mR);function
Ha(b,a){return a}function
m3(a){return cp(gI,Ha,cZ,gU,gL,mR,a)}function
m4(d){function
c(c){var
e=b(a$[35][4],c);if(iS(b(a$[35][5],c),e,d))return Q[65][2];var
f=b(bA[3],Hb);return a(Q[65][4],0,f)}return b(di[68][8],c)}aE(1043,[0,m4,i4,i2,i3,m0,m1,m2,m3,mX,mY,mZ,mV,mW,mG],"Micromega_plugin__Coq_micromega");b(Hc[9],aB);var
Hd=0,Hf=[0,[0,He,function(a){return aM[56]}],Hd];W(aN[8],aB,Hg,0,0,Hf);var
Hh=0;function
Hi(a,b){return m4(a)}var
Hk=[0,[0,[0,Hj,[1,[5,b(ad[16],gX[11])],0]],Hi],Hh];W(aN[8],aB,Hl,0,0,Hk);var
Hm=0;function
Hn(d,c){var
e=a(aC[24],c,d);return b(i4(-1),e)}var
Hp=[0,[0,[0,Ho,[1,[5,b(ad[16],aD[9])],0]],Hn],Hm];function
Hq(e,d,c){var
f=a(aC[24],c,d);return b(i4(e),f)}var
Hr=[1,[5,b(ad[16],aD[9])],0],Ht=[0,[0,[0,Hs,[1,[5,b(ad[16],gX[6])],Hr]],Hq],Hp];W(aN[8],aB,Hu,0,0,Ht);var
Hv=0;function
Hw(c,b){return m0(a(aC[24],b,c))}var
Hy=[0,[0,[0,Hx,[1,[5,b(ad[16],aD[9])],0]],Hw],Hv];W(aN[8],aB,Hz,0,0,Hy);var
HA=0;function
HB(c,b){return m1(a(aC[24],b,c))}var
HD=[0,[0,[0,HC,[1,[5,b(ad[16],aD[9])],0]],HB],HA];W(aN[8],aB,HE,0,0,HD);var
HF=0;function
HG(d,c){return b(m2,a(aC[24],c,d))}var
HI=[0,[0,[0,HH,[1,[5,b(ad[16],aD[9])],0]],HG],HF];W(aN[8],aB,HJ,0,0,HI);var
HK=0;function
HL(c,b){return m3(a(aC[24],b,c))}var
HN=[0,[0,[0,HM,[1,[5,b(ad[16],aD[9])],0]],HL],HK];W(aN[8],aB,HO,0,0,HN);var
HP=0;function
HQ(c,b){return mX(a(aC[24],b,c))}var
HS=[0,[0,[0,HR,[1,[5,b(ad[16],aD[9])],0]],HQ],HP];W(aN[8],aB,HT,0,0,HS);var
HU=0;function
HV(c,b){return mY(a(aC[24],b,c))}var
HX=[0,[0,[0,HW,[1,[5,b(ad[16],aD[9])],0]],HV],HU];W(aN[8],aB,HY,0,0,HX);var
HZ=0;function
H0(d,c){return b(mZ,a(aC[24],c,d))}var
H2=[0,[0,[0,H1,[1,[5,b(ad[16],aD[9])],0]],H0],HZ];W(aN[8],aB,H3,0,0,H2);var
H4=0;function
H5(c,b){return mV(a(aC[24],b,c))}var
H7=[0,[0,[0,H6,[1,[5,b(ad[16],aD[9])],0]],H5],H4];W(aN[8],aB,H8,0,0,H7);var
H9=0;function
H_(d,c){return b(mW,a(aC[24],c,d))}var
Ia=[0,[0,[0,H$,[1,[5,b(ad[16],aD[9])],0]],H_],H9];W(aN[8],aB,Ib,0,0,Ia);var
Ic=0;function
Id(d,c){var
e=a(aC[24],c,d);return b(i3(-1),e)}var
If=[0,[0,[0,Ie,[1,[5,b(ad[16],aD[9])],0]],Id],Ic];function
Ig(e,d,c){var
f=a(aC[24],c,d);return b(i3(e),f)}var
Ih=[1,[5,b(ad[16],aD[9])],0],Ij=[0,[0,[0,Ii,[1,[5,b(ad[16],gX[6])],Ih]],Ig],If];W(aN[8],aB,Ik,0,0,Ij);var
Il=0;function
Im(d,c){var
e=a(aC[24],c,d);return b(i2(-1),e)}var
Io=[0,[0,[0,In,[1,[5,b(ad[16],aD[9])],0]],Im],Il];function
Ip(e,d,c){var
f=a(aC[24],c,d);return b(i2(e),f)}var
Iq=[1,[5,b(ad[16],aD[9])],0],Is=[0,[0,[0,Ir,[1,[5,b(ad[16],gX[6])],Iq]],Ip],Io];W(aN[8],aB,It,0,0,Is);aE(1050,[0],"Micromega_plugin__G_micromega");return}
