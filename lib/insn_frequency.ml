(**
   sthx       1           | store halfword indexed
   bclr+      2           | branch conditional to LR with branch prediction
   lhau       2           | load halfword algebraic with update
   nor        2           | not_ or_
   bnelr      3           | branch to LR if not equal
   mullw.     3           |
   stwbrx     4           |
   stbux      4           | store word with update indexed
   lwzux      6           | load word zero with update indexed
   rotlw      6           | rotate left word
   extsh      7           | extend sign halfword
   neg.       7           | neg
   trap       8           |
   creqv      9           | CR eqv
   bltlr      9           | branch to LR is less than
   andc.      13          | and with complement
   sraw       19          | shift right word
   oris       21          | or_ imm shifted
   fadds      22          |
   lbzux      24          | load byte zero update indexed
   nand       30          | not_ and
   extsb.     31          | extend sign byte
   orc        38          | or_ with complement
   lwbrx      38          |
   lha        40          | load halfword algebraic
   extsb      40          | extend sign byte
   fdivs      44          |
   add.       44          | add
   addme      67          | add minus one
   fmsub      69          |
   fdiv       71          |
   crnot      73          | CR not (extended for crnor)
   divw       86          | div word
   stwux      101         | store word with update indexed
   bso        102         | branch if summary overflow
   srawi.     106         |
   format     107         |
   addze.     107         | add to zero extended
   blelr      107         | branch to LR if less than or equal
   subfze     114         | substract from
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
   crset      200         | CR set (extended for creqv)
   fabs       230         |
   fmuls      232         |
   frsp       254         |
   subf.      260         |
   and.       289         | and
   mulhw      292         | mul high word
   addze      298         | add to zero extended
   fneg       332         |
   lhz        334         | load halfword zero
   andis.     337         | and imm shifted
   lhzx       338         | load halfword zero indexed
   not        378           | not_
   beqlr      390         | branch to LR if equal
   mcrf       434         | move CR field
   sth        452         | store halfword
   or.        479          | or_
   addic.     480         | add imm complemented
   bcl        527         | branch conditional
   subfc      610         | substract from
   fmul       615         |
   neg        616         | neg
   cror       617         | CR or
   fctiwz     635         |
   fadd       742         |
   stbu       768         | store byte with update
   xoris      805         | xor imm shifted
   bctrl      813         | branch to CTR unconditionally
   mulhwu     839         | mul high word unsigned
   bdz        857         | branch conditional extended
   subfic     1180        | substract from
   stwx       1180        | store word indexed
   fsub       1235        |
   addic      1238        | add imm complemented
   .long      1286        |
   xori       1342        | xor imm
   mulli      1383        | mul imm
   addis      1435        | add imm shifted
   cntlzw     1470        | count leading zero
   subfe      1706        | substract from
   srawi      1749        | shift right algebraic imm
   srw        1831        | shift right word
   rotlwi     1832        | rotate left word imm
   mfcr       1862        | move from CR
   divwu      2008        | div word unsigned
   and        2033        | and
   lfs        2039        |
   lbzx       2128        | load byte zero indexed
   addc       2225        | add complemented
   mtcrf      2235        | move to CR field
   adde       2276        | add extended
   slw        2304        | shift left word
   lbzu       2429        | load byte with update
   bdnz       2458        | branch conditional extended
   xor.       2465        | xor
   lwzu       2474        | load word with update
   fcmpu      3293        |
   stfd       3774        |
   fmr        4061        |
   mullw      4116        | mul word
   andi.      4147        | add immediate
   stbx       4285        | store byte indexed
   clrlwi     4387        | extended mnemonic for Rotate left imm + mask
   mr.        4847        | or_dot (extended)
   blt        4910        | branch if less than
   ori        5288        | ori
   lwzx       5330        | load word indexed
   lfd        5433        |
   xor        6147        | xor
   bge        6910        | branch if greater than or equal
   crclr      7127        | CR clear (extended for creqv)
   or         7808         | or_
   cmpw       8326        | compare
   bctr       8684        | branch to CTR unconditionally
   subf       8883        | substract from
   stb        9331        | store byte
   bgt        9421        | branch if greater than
   mflr       9835        | mfspr - move from link register
   ble        10935       | branch is less than or equal
   mtlr       11504       | mtspr - move to link register
   blr        12568       | branch to LR unconditionally
   mtctr      13029       | mtspr - move to cnt register
   cmplwi     13495       | compare logical
   stwu       14900       | store word with update
   lbz        17430       | load byte and zero
   add        20844       | add
   cmplw      22235       | compare logical
   rlwinm     27103       | rotate left imm then and mask
   bne        40300       | branch (extended)
   lis        43125       | addis (extended)
   beq        55306       | branch if equal
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
