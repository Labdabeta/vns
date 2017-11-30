"----------------------------------------------------------------------------
"  Description: Von Neumann Standing Assembly syntax file
"     Language: VNS (2017)
"   Maintainer: Louis Burke
"------------------------------------------------------------------------------

if exists("b:current_syntax") || version < 700
    finish
endif

let b:current_syntax = "vns"

syntax keyword vnsOperation add sub mul div and orr xor nan clz cnt lsr lsl
syntax keyword vnsOperation abs rnd cmp jiz jnz jgz jlz jge jle biz bnz bgz
syntax keyword vnsOperation blz bge ble blx ldr str pop psh who wht qcs qct
syntax keyword vnsOperation qbp qck gnd whr dst cvr ded sht dir wlk crl swm
syntax keyword vnsOperation cap rtt hid say rad yel ear die nrt nre est soe
syntax keyword vnsOperation sot sow wst nrw wcs wct wbp wcl tcs tct tbp tcl
syntax keyword vnsOperation pnt ccs cct cbp ccl ucs uct ubp ucl dcs dct dbp
syntax keyword vnsOperation dcl mcs mct mbp mcl rcs rct rbp rcl tim dly adv
syntax keyword vnsOperation ask plz beg gvl kmk sac bom air sum hak emp all
syntax keyword vnsOperation rmt rsn res ref rms rmf rss rsf rrs rrf wmt wsn
syntax keyword vnsOperation wes wef wms wmf wss wsf wrs wrf wtg rtg wsg rsg
syntax keyword vnsOperation wfg rfg itf fad fsu fmu fdv cel rtf sin cos tan
syntax keyword vnsOperation pow asn acs atn log fcp mle set css cfs wss wfs
syntax keyword vnsOperation gup sup cam unc hip nmr ist tgt lie mor wir cut
syntax keyword vnsOperation snd dig mlf run hit raw
highlight link vnsOperation Keyword

syntax keyword vnsRegister r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31
highlight link vnsRegister String

syntax match vnsComment "\v;.*$"
highlight link vnsComment Comment

syntax match vnsInclude "\v#.*$"
highlight link vnsInclude PreProc

syntax match vnsOperator "\v\!\S+"
syntax match vnsOperator ","
syntax match vnsOperator "-"
syntax match vnsOperator "+"
highlight link vnsOperator Operator

syntax match vnsIdentifier "\h\w*"
highlight link vnsIdentifier Identifier

syntax match vnsLabel "[a-zA-Z_][a-zA-Z0-9_]*:"
syntax match vnsLabel "\."
highlight link vnsLabel Type

syntax match vnsLiteral "\v\d\x*"
highlight link vnsLiteral Constant
