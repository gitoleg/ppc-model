(**
   sthx       1           | store halfword indexed
   bclr+      2           |
   lhau       2           | load halfword algebraic with update
   nor        2           | not_ or_
   bnelr      3           |
   mullw.     3           |
   stwbrx     4           |
   stbux      4           | store word with update indexed
   lwzux      6           | load word zero with update indexed
   rotlw      6           | rotate left word
   extsh      7           |
   neg.       7           | neg
   trap       8           |
   creqv      9           |
   bltlr      9           |
   andc.      13          | and with complement
   sraw       19          | shift right word
   oris       21          | or_ imm shifted
   fadds      22          |
   lbzux      24          |
   nand       30          | not_ and
   extsb.     31          |
   orc        38          | or_ with complement
   lwbrx      38          |
   lha        40          | load halfword algebraic
   extsb      40          |
   fdivs      44          |
   add.       44          | add
   addme      67          | add minus one
   fmsub      69          |
   fdiv       71          |
   crnot      73          |
   divw       86          |
   stwux      101         | store word with update indexed
   bso        102         |
   srawi.     106         |
   format     107         |
   addze.     107         | add to zero extended
   blelr      107         |
   subfze     114         |
   rlwimi     123         | rotate left imm then mask insert
   fsubs      127         |
   lhzu       154         | load halfword with update
   andc       154         | and with complement
   fmadd      165         |
   rlwinm.    172         |
   mtfsb1     173         |
   mtfsf      173         |
   mffs       173         |
   mtfsb0     173         |
   crset      200         |
   fabs       230         |
   fmuls      232         |
   frsp       254         |
   subf.      260         |
   and.       289         | and
   mulhw      292         |
   addze      298         | add to zero extended
   fneg       332         |
   lhz        334         |
   andis.     337         | and imm shifted
   lhzx       338         | load halfword zero indexed
   not        378           | not_
   beqlr      390         |
   mcrf       434         |
   sth        452         | store halfword
   or.        479          | or_
   addic.     480         | add imm complemented
   bcl        527         | branch conditional
   subfc      610         |
   fmul       615         |
   neg        616         | neg
   cror       617         |
   fctiwz     635         |
   fadd       742         |
   stbu       768         | store byte with update
   xoris      805         | xor imm shifted
   bctrl      813         |
   mulhwu     839         |
   bdz        857         |
   subfic     1180        |
   stwx       1180        | store word indexed
   fsub       1235        |
   addic      1238        | add imm complemented
   .long      1286        |
   xori       1342        | xor imm
   mulli      1383        |
   addis      1435        | add imm shifted
   cntlzw     1470        | count leading zero
   subfe      1706        |
   srawi      1749        | shift right algebraic imm
   srw        1831        | shift right word
   rotlwi     1832        | rotate left word imm
   mfcr       1862        |
   divwu      2008        |
   and        2033        | and
   lfs        2039        |
   lbzx       2128        |
   addc       2225        | add complemented
   mtcrf      2235        |
   adde       2276        | add extended
   slw        2304        | shift left word
   lbzu       2429        | load byte with update
   bdnz       2458        |
   xor.       2465        | xor
   lwzu       2474        | load word with update
   fcmpu      3293        |
   stfd       3774        |
   fmr        4061        |
   mullw      4116        |
   andi.      4147        | add immediate
   stbx       4285        | store byte indexed
   clrlwi     4387        | extended mnemonic for Rotate left imm + mask
   mr.        4847        | or_dot (extended)
   blt        4910        |
   ori        5288        | ori
   lwzx       5330        | load word indexed
   lfd        5433        |
   xor        6147        | xor
   bge        6910        |
   crclr      7127        |
   or         7808         | or_
   cmpw       8326        | compare
   bctr       8684        | branch conditional
   subf       8883        |
   stb        9331        | store byte
   bgt        9421        |
   mflr       9835        |
   ble        10935       |
   mtlr       11504       |
   blr        12568       |
   mtctr      13029       |
   cmplwi     13495       | compare logical
   stwu       14900       | store word with update
   lbz        17430       | load byte and zero
   add        20844       | add
   cmplw      22235       | compare logical
   rlwinm     27103       | rotate left imm then and mask
   bne        40300       | branch (extended)
   lis        43125       | addis (extended)
   beq        55306       | branch (extended)
   b          59781       | branch
   cmpwi      65197       | compare
   bl         66624       | branch
   nop        78681       | ori (extended)
   addi       91142       | add immediate
   li         104738      | add immediate (extended)
   mr         105024      | or_ (extended)
   stw        105118      | store word
   lwz        159459      | load word and zero

*)
