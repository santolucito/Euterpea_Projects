-----------------------------------------------------------------------------
-- |
-- Module : Timer_without_beep
--
-- Applicative Interface for timer_without_beep.
--
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}

-----------------------------------------------------------------------------

module TimerControl
  ( timer_without_beep
  ) where

import Control.Monad.Fix
-----------------------------------------------------------------------------

timer_without_beep
  :: (MonadFix m, Applicative signal)
     -- cell implementation
  => (forall poly. poly -> signal poly -> m (signal poly)) 
    -- eq
  -> (c -> c -> Bool)
     -- zero
  -> c
     -- countdown
  -> (c -> a -> c)
     -- countup
  -> (c -> a -> c)
     -- display
  -> (c -> b)
     -- incMinutes
  -> (c -> c)
     -- incSeconds
  -> (c -> c)
     -- initial value: dsp
  -> b
     -- initial value: time
  -> c
     -- dt (input)
  -> signal a
     -- btnMin (input)
  -> signal Bool
     -- btnSec (input)
  -> signal Bool
     -- btnStartStop (input)
  -> signal Bool
     -- outputs
  -> m ( -- dsp
       signal b
     , -- time
       signal c
     )

timer_without_beep
  cell
  p_eq
  f_zero
  f_countdown
  f_countup
  f_display
  f_incMinutes
  f_incSeconds
  i_dsp
  i_time
  s_dt
  s_btnMin
  s_btnSec
  s_btnStartStop

  = do
      rec 
        c_dsp <- cell i_dsp o_dsp
        c_time <- cell i_time o_time
        
        let 
          w3 = pure f_zero
          w4 = f_display <$> c_time
          w5 = f_incMinutes <$> c_time
          w6 = f_incSeconds <$> c_time
          w7 = f_countdown <$> c_time <*> s_dt
          w8 = f_countup <$> c_time <*> s_dt
          b12 = p_eq <$> c_time <*> w3

        (cout0, cout1, cout2, cout3, cout4, cout5, cout6, cout7) <-
          controlCircuit
            cell
            s_btnMin
            s_btnSec
            s_btnStartStop
            b12

        let
          o_dsp =
            dspSwitch
              c_dsp
              cout0
              w4
              cout1
          o_time =
            timeSwitch
              c_time
              cout2
              w3
              cout3
              w5
              cout4
              w6
              cout5
              w7
              cout6
              w8
              cout7

      return (o_dsp, o_time)

-----------------------------------------------------------------------------

dspSwitch
  :: Applicative signal
  => signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a

dspSwitch s0 b0 s1 _ =
  let ite b s a = (\b s a -> if b then s else a) <$> b <*> s <*> a
  in ite b0 s0 s1

-----------------------------------------------------------------------------

timeSwitch
  :: Applicative signal
  => signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a

timeSwitch s0 b0 s1 b1 s2 b2 s3 b3 s4 b4 s5 _ =
  let ite b s a = (\b s a -> if b then s else a) <$> b <*> s <*> a
  in ite b0 s0 $ ite b1 s1 $ ite b2 s2 $ ite b3 s3 $ ite b4 s4 s5

-----------------------------------------------------------------------------

controlCircuit
  :: (MonadFix m, Applicative signal)
     -- cell implementation
  => (Bool -> signal Bool -> m (signal Bool))
     -- inputs
  -> signal Bool
  -> signal Bool
  -> signal Bool
  -> signal Bool
     -- outputs
  -> m ( signal Bool
     , signal Bool
     , signal Bool
     , signal Bool
     , signal Bool
     , signal Bool
     , signal Bool
     , signal Bool
     )

controlCircuit cell cin0 cin1 cin2 cin3 = do
  rec
    w5 <- _lat_ w322
    w6 <- _lat_ x6
    w7 <- _lat_ x7
    w8 <- _lat_ x8
    w9 <- _lat_ x9
    w10 <- _lat_ x10
    let  
      x6 = _not_ w363
      x7 = _not_ w396
      x8 = _not_ w422
      x9 = _not_ w439
      x10 = _not_ w454
      a11 = _not_ w7
      b11 = _not_ w6
      w11 = _and_ a11 b11
      w12 = _and_ w8 w7
      a13 = _not_ w12
      b13 = _not_ w11
      w13 = _and_ a13 b13
      a14 = _not_ w13
      b14 = _not_ w9
      w14 = _and_ a14 b14
      b15 = _not_ w8
      w15 = _and_ w14 b15
      a16 = _not_ w15
      w16 = _and_ a16 w10
      b17 = _not_ cin0
      w17 = _and_ cin1 b17
      w18 = _and_ w15 w10
      b19 = _not_ cin3
      w19 = _and_ w18 b19
      w20 = _and_ w19 w5
      w21 = _and_ w7 w6
      w22 = _and_ w21 w5
      a23 = _not_ w9
      w23 = _and_ a23 w8
      w24 = _and_ w23 w22
      a25 = _not_ w7
      w25 = _and_ a25 w6
      a26 = _not_ w12
      w26 = _and_ a26 w9
      w27 = _and_ w25 w5
      a28 = _not_ w27
      w28 = _and_ a28 w26
      a29 = _not_ w8
      b29 = _not_ w7
      w29 = _and_ a29 b29
      b30 = _not_ w5
      w30 = _and_ w29 b30
      a31 = _not_ w30
      w31 = _and_ a31 w28
      w32 = _and_ w31 w25
      a33 = _not_ w32
      b33 = _not_ w24
      w33 = _and_ a33 b33
      a34 = _not_ w29
      w34 = _and_ a34 w26
      a35 = _not_ w7
      w35 = _and_ a35 w5
      w36 = _and_ w6 w5
      a37 = _not_ w36
      w37 = _and_ a37 w8
      a38 = _not_ w37
      w38 = _and_ a38 w14
      b39 = _not_ w35
      w39 = _and_ w38 b39
      a40 = _not_ w39
      b40 = _not_ w34
      w40 = _and_ a40 b40
      a41 = _not_ w40
      w41 = _and_ a41 cin3
      w42 = _and_ w41 w33
      a43 = _not_ w42
      w43 = _and_ a43 cin2
      b44 = _not_ w8
      w44 = _and_ w21 b44
      b45 = _not_ w5
      w45 = _and_ w44 b45
      b46 = _not_ w5
      w46 = _and_ w11 b46
      w47 = _and_ w46 w8
      a48 = _not_ w47
      b48 = _not_ w45
      w48 = _and_ a48 b48
      a49 = _not_ w48
      w49 = _and_ a49 w9
      a50 = _not_ w7
      b50 = _not_ w5
      w50 = _and_ a50 b50
      b51 = _not_ w6
      w51 = _and_ w7 b51
      a52 = _not_ w51
      b52 = _not_ w25
      w52 = _and_ a52 b52
      a53 = _not_ w51
      b53 = _not_ w5
      w53 = _and_ a53 b53
      a54 = _not_ w21
      w54 = _and_ a54 w5
      a55 = _not_ w54
      b55 = _not_ w53
      w55 = _and_ a55 b55
      a56 = _not_ w55
      b56 = _not_ w6
      w56 = _and_ a56 b56
      a57 = _not_ w56
      b57 = _not_ w52
      w57 = _and_ a57 b57
      b58 = _not_ w8
      w58 = _and_ w57 b58
      b59 = _not_ w50
      w59 = _and_ w58 b59
      a60 = _not_ w21
      b60 = _not_ w5
      w60 = _and_ a60 b60
      a61 = _not_ w11
      w61 = _and_ a61 w5
      a62 = _not_ w61
      w62 = _and_ a62 w8
      b63 = _not_ w60
      w63 = _and_ w62 b63
      a64 = _not_ w35
      b64 = _not_ w8
      w64 = _and_ a64 b64
      b65 = _not_ w60
      w65 = _and_ w64 b65
      a66 = _not_ w65
      b66 = _not_ w63
      w66 = _and_ a66 b66
      b67 = _not_ w59
      w67 = _and_ w66 b67
      b68 = _not_ w5
      w68 = _and_ w12 b68
      a69 = _not_ w22
      w69 = _and_ a69 w9
      b70 = _not_ w68
      w70 = _and_ w69 b70
      b71 = _not_ w67
      w71 = _and_ w70 b71
      b72 = _not_ w9
      w72 = _and_ w57 b72
      w73 = _and_ w72 w13
      a74 = _not_ w73
      b74 = _not_ w49
      w74 = _and_ a74 b74
      b75 = _not_ w71
      w75 = _and_ w74 b75
      a76 = _not_ w75
      b76 = _not_ cin3
      w76 = _and_ a76 b76
      a77 = _not_ w76
      w77 = _and_ a77 w43
      b78 = _not_ w8
      w78 = _and_ w9 b78
      w79 = _and_ w78 cin3
      w80 = _and_ w79 w46
      a81 = _not_ w80
      b81 = _not_ w42
      w81 = _and_ a81 b81
      w82 = _and_ w69 w12
      w83 = _and_ w78 w55
      w84 = _and_ w34 w27
      a85 = _not_ w84
      b85 = _not_ w28
      w85 = _and_ a85 b85
      w86 = _and_ w25 w8
      a87 = _not_ w86
      b87 = _not_ w29
      w87 = _and_ a87 b87
      a88 = _not_ w87
      w88 = _and_ a88 w9
      b89 = _not_ w85
      w89 = _and_ w88 b89
      a90 = _not_ w89
      b90 = _not_ cin3
      w90 = _and_ a90 b90
      a91 = _not_ w86
      b91 = _not_ w9
      w91 = _and_ a91 b91
      b92 = _not_ w45
      w92 = _and_ w91 b92
      b93 = _not_ w59
      w93 = _and_ w92 b93
      a94 = _not_ w83
      b94 = _not_ w82
      w94 = _and_ a94 b94
      w95 = _and_ w94 w90
      b96 = _not_ w93
      w96 = _and_ w95 b96
      b97 = _not_ cin2
      w97 = _and_ w81 b97
      b98 = _not_ w96
      w98 = _and_ w97 b98
      a99 = _not_ w98
      b99 = _not_ w10
      w99 = _and_ a99 b99
      b100 = _not_ w77
      w100 = _and_ w99 b100
      a101 = _not_ w100
      b101 = _not_ w20
      w101 = _and_ a101 b101
      a102 = _not_ w101
      w102 = _and_ a102 w17
      w103 = _and_ w46 w19
      b104 = _not_ w5
      w104 = _and_ w15 b104
      a105 = _not_ w104
      b105 = _not_ w31
      w105 = _and_ a105 b105
      a106 = _not_ w105
      w106 = _and_ a106 cin3
      a107 = _not_ w106
      b107 = _not_ cin2
      w107 = _and_ a107 b107
      w108 = _and_ w53 w8
      b109 = _not_ w52
      w109 = _and_ w64 b109
      a110 = _not_ w109
      b110 = _not_ w108
      w110 = _and_ a110 b110
      b111 = _not_ w9
      w111 = _and_ w110 b111
      b112 = _not_ w53
      w112 = _and_ w64 b112
      a113 = _not_ w112
      w113 = _and_ a113 w9
      b114 = _not_ w63
      w114 = _and_ w113 b114
      a115 = _not_ w111
      b115 = _not_ cin3
      w115 = _and_ a115 b115
      b116 = _not_ w114
      w116 = _and_ w115 b116
      a117 = _not_ w116
      w117 = _and_ a117 w107
      a118 = _not_ w58
      b118 = _not_ w37
      w118 = _and_ a118 b118
      a119 = _not_ w118
      b119 = _not_ w9
      w119 = _and_ a119 b119
      w120 = _and_ w87 w26
      a121 = _not_ w120
      b121 = _not_ w119
      w121 = _and_ a121 b121
      a122 = _not_ w108
      w122 = _and_ a122 w14
      w123 = _and_ w69 w21
      a124 = _not_ w123
      b124 = _not_ cin3
      w124 = _and_ a124 b124
      b125 = _not_ w122
      w125 = _and_ w124 b125
      b126 = _not_ w121
      w126 = _and_ w125 b126
      a127 = _not_ w105
      w127 = _and_ a127 w41
      a128 = _not_ w127
      w128 = _and_ a128 cin2
      b129 = _not_ w126
      w129 = _and_ w128 b129
      a130 = _not_ w117
      b130 = _not_ w10
      w130 = _and_ a130 b130
      b131 = _not_ w129
      w131 = _and_ w130 b131
      a132 = _not_ w103
      w132 = _and_ a132 cin0
      b133 = _not_ w131
      w133 = _and_ w132 b133
      a134 = _not_ w19
      b134 = _not_ cin0
      w134 = _and_ a134 b134
      w135 = _and_ w79 w50
      a136 = _not_ w135
      b136 = _not_ w106
      w136 = _and_ a136 b136
      b137 = _not_ w41
      w137 = _and_ w136 b137
      a138 = _not_ w137
      b138 = _not_ cin2
      w138 = _and_ a138 b138
      w139 = _and_ w56 w8
      a140 = _not_ w139
      b140 = _not_ w59
      w140 = _and_ a140 b140
      w141 = _and_ w140 w9
      a142 = _not_ w141
      b142 = _not_ cin3
      w142 = _and_ a142 b142
      a143 = _not_ w58
      b143 = _not_ w9
      w143 = _and_ a143 b143
      b144 = _not_ w63
      w144 = _and_ w143 b144
      a145 = _not_ w144
      w145 = _and_ a145 w142
      a146 = _not_ w145
      b146 = _not_ w41
      w146 = _and_ a146 b146
      a147 = _not_ w146
      w147 = _and_ a147 cin2
      a148 = _not_ cin3
      b148 = _not_ cin2
      w148 = _and_ a148 b148
      w149 = _and_ w148 w85
      b150 = _not_ w9
      w150 = _and_ w66 b150
      a151 = _not_ w150
      w151 = _and_ a151 w149
      a152 = _not_ w151
      b152 = _not_ w138
      w152 = _and_ a152 b152
      b153 = _not_ w147
      w153 = _and_ w152 b153
      a154 = _not_ w153
      b154 = _not_ w10
      w154 = _and_ a154 b154
      a155 = _not_ w154
      w155 = _and_ a155 w134
      a156 = _not_ w133
      b156 = _not_ cin1
      w156 = _and_ a156 b156
      b157 = _not_ w155
      w157 = _and_ w156 b157
      a158 = _not_ w157
      b158 = _not_ w102
      w158 = _and_ a158 b158
      w159 = _and_ w18 cin3
      a160 = _not_ w159
      w160 = _and_ a160 cin0
      b161 = _not_ w38
      w161 = _and_ w90 b161
      a162 = _not_ w161
      w162 = _and_ a162 w137
      b163 = _not_ w10
      w163 = _and_ w162 b163
      w164 = _and_ w160 cin1
      b165 = _not_ w163
      w165 = _and_ w164 b165
      b166 = _not_ w16
      w166 = _and_ w165 b166
      a167 = _not_ w84
      b167 = _not_ w24
      w167 = _and_ a167 b167
      a168 = _not_ w167
      w168 = _and_ a168 cin3
      a169 = _not_ w168
      b169 = _not_ w135
      w169 = _and_ a169 b169
      b170 = _not_ w53
      w170 = _and_ w62 b170
      a171 = _not_ w44
      b171 = _not_ w9
      w171 = _and_ a171 b171
      b172 = _not_ w170
      w172 = _and_ w171 b172
      w173 = _and_ w124 w85
      b174 = _not_ w172
      w174 = _and_ w173 b174
      a175 = _not_ w174
      w175 = _and_ a175 w169
      a176 = _not_ w175
      b176 = _not_ w10
      w176 = _and_ a176 b176
      a177 = _not_ w176
      b177 = _not_ w20
      w177 = _and_ a177 b177
      a178 = _not_ cin1
      w178 = _and_ a178 cin0
      b179 = _not_ w177
      w179 = _and_ w178 b179
      w180 = _and_ w7 w5
      b181 = _not_ w8
      w181 = _and_ w180 b181
      a182 = _not_ w60
      w182 = _and_ a182 w37
      a183 = _not_ w182
      b183 = _not_ w181
      w183 = _and_ a183 b183
      a184 = _not_ w183
      b184 = _not_ w9
      w184 = _and_ a184 b184
      a185 = _not_ w184
      b185 = _not_ w82
      w185 = _and_ a185 b185
      a186 = _not_ w185
      b186 = _not_ cin3
      w186 = _and_ a186 b186
      a187 = _not_ w33
      w187 = _and_ a187 cin3
      a188 = _not_ w136
      b188 = _not_ w8
      w188 = _and_ a188 b188
      w189 = _and_ w188 w88
      b190 = _not_ w46
      w190 = _and_ w189 b190
      a191 = _not_ w190
      b191 = _not_ w187
      w191 = _and_ a191 b191
      b192 = _not_ w186
      w192 = _and_ w191 b192
      a193 = _not_ w192
      b193 = _not_ w10
      w193 = _and_ a193 b193
      a194 = _not_ w193
      b194 = _not_ w103
      w194 = _and_ a194 b194
      a195 = _not_ w194
      w195 = _and_ a195 w17
      a196 = _not_ w10
      b196 = _not_ cin0
      w196 = _and_ a196 b196
      w197 = _and_ w51 w23
      a198 = _not_ w197
      b198 = _not_ w120
      w198 = _and_ a198 b198
      a199 = _not_ w198
      w199 = _and_ a199 w148
      a200 = _not_ cin3
      w200 = _and_ a200 cin2
      a201 = _not_ w68
      w201 = _and_ a201 w9
      b202 = _not_ w21
      w202 = _and_ w201 b202
      a203 = _not_ w202
      b203 = _not_ w83
      w203 = _and_ a203 b203
      a204 = _not_ w203
      b204 = _not_ w65
      w204 = _and_ a204 b204
      a205 = _not_ w197
      b205 = _not_ w9
      w205 = _and_ a205 b205
      a206 = _not_ w205
      b206 = _not_ w204
      w206 = _and_ a206 b206
      w207 = _and_ w206 w200
      a208 = _not_ w207
      b208 = _not_ w199
      w208 = _and_ a208 b208
      a209 = _not_ w208
      w209 = _and_ a209 w196
      w210 = _and_ w197 w5
      a211 = _not_ w210
      b211 = _not_ w49
      w211 = _and_ a211 b211
      a212 = _not_ w211
      w212 = _and_ a212 w148
      a213 = _not_ w210
      b213 = _not_ w123
      w213 = _and_ a213 b213
      a214 = _not_ w213
      b214 = _not_ cin3
      w214 = _and_ a214 b214
      w215 = _and_ w214 cin2
      a216 = _not_ w215
      b216 = _not_ w212
      w216 = _and_ a216 b216
      a217 = _not_ w10
      w217 = _and_ a217 cin0
      b218 = _not_ w216
      w218 = _and_ w217 b218
      a219 = _not_ w218
      b219 = _not_ w209
      w219 = _and_ a219 b219
      a220 = _not_ w219
      b220 = _not_ cin1
      w220 = _and_ a220 b220
      w221 = _and_ w199 w55
      a222 = _not_ w198
      w222 = _and_ a222 w55
      a223 = _not_ w222
      b223 = _not_ w9
      w223 = _and_ a223 b223
      b224 = _not_ w69
      w224 = _and_ w200 b224
      b225 = _not_ w223
      w225 = _and_ w224 b225
      a226 = _not_ w225
      b226 = _not_ w221
      w226 = _and_ a226 b226
      b227 = _not_ w10
      w227 = _and_ w17 b227
      b228 = _not_ w226
      w228 = _and_ w227 b228
      a229 = _not_ w228
      b229 = _not_ w220
      w229 = _and_ a229 b229
      b230 = _not_ w182
      w230 = _and_ w205 b230
      b231 = _not_ w58
      w231 = _and_ w230 b231
      a232 = _not_ w231
      w232 = _and_ a232 w90
      a233 = _not_ w232
      b233 = _not_ cin1
      w233 = _and_ a233 b233
      b234 = _not_ w9
      w234 = _and_ w183 b234
      w235 = _and_ w52 cin1
      w236 = _and_ w235 w234
      a237 = _not_ w236
      w237 = _and_ a237 w90
      a238 = _not_ w237
      b238 = _not_ w190
      w238 = _and_ a238 b238
      a239 = _not_ w238
      b239 = _not_ w233
      w239 = _and_ a239 b239
      a240 = _not_ w239
      w240 = _and_ a240 cin2
      b241 = _not_ w65
      w241 = _and_ w230 b241
      w242 = _and_ w241 w93
      a243 = _not_ w242
      w243 = _and_ a243 w90
      a244 = _not_ w135
      w244 = _and_ a244 cin1
      w245 = _and_ w244 w107
      b246 = _not_ w243
      w246 = _and_ w245 b246
      a247 = _not_ w246
      b247 = _not_ w240
      w247 = _and_ a247 b247
      a248 = _not_ w247
      b248 = _not_ cin0
      w248 = _and_ a248 b248
      w249 = _and_ w171 w118
      a250 = _not_ w249
      w250 = _and_ a250 w90
      a251 = _not_ w250
      w251 = _and_ a251 cin2
      w252 = _and_ w172 w110
      b253 = _not_ w37
      w253 = _and_ w252 b253
      w254 = _and_ w253 w107
      a255 = _not_ w254
      b255 = _not_ w251
      w255 = _and_ a255 b255
      a256 = _not_ w255
      w256 = _and_ a256 cin0
      b257 = _not_ cin0
      w257 = _and_ w241 b257
      a258 = _not_ w257
      w258 = _and_ a258 w90
      a259 = _not_ w258
      w259 = _and_ a259 w107
      a260 = _not_ w259
      b260 = _not_ w256
      w260 = _and_ a260 b260
      a261 = _not_ w135
      b261 = _not_ cin1
      w261 = _and_ a261 b261
      b262 = _not_ w260
      w262 = _and_ w261 b262
      a263 = _not_ w262
      b263 = _not_ w248
      w263 = _and_ a263 b263
      a264 = _not_ w263
      b264 = _not_ w41
      w264 = _and_ a264 b264
      w265 = _and_ cin1 cin0
      w266 = _and_ w265 w162
      a267 = _not_ w266
      b267 = _not_ w264
      w267 = _and_ a267 b267
      a268 = _not_ w267
      b268 = _not_ w10
      w268 = _and_ a268 b268
      a269 = _not_ w268
      b269 = _not_ w159
      w269 = _and_ a269 b269
      a270 = _not_ w57
      w270 = _and_ a270 w48
      a271 = _not_ w270
      b271 = _not_ w9
      w271 = _and_ a271 b271
      w272 = _and_ w78 w27
      a273 = _not_ w272
      b273 = _not_ w83
      w273 = _and_ a273 b273
      b274 = _not_ w271
      w274 = _and_ w273 b274
      a275 = _not_ w274
      b275 = _not_ cin3
      w275 = _and_ a275 b275
      a276 = _not_ w275
      w276 = _and_ a276 w81
      a277 = _not_ w276
      b277 = _not_ cin2
      w277 = _and_ a277 b277
      a278 = _not_ w47
      b278 = _not_ w9
      w278 = _and_ a278 b278
      b279 = _not_ w65
      w279 = _and_ w278 b279
      a280 = _not_ w201
      b280 = _not_ cin3
      w280 = _and_ a280 b280
      b281 = _not_ w279
      w281 = _and_ w280 b281
      a282 = _not_ w281
      b282 = _not_ w189
      w282 = _and_ a282 b282
      a283 = _not_ w282
      w283 = _and_ a283 cin2
      a284 = _not_ w277
      b284 = _not_ w10
      w284 = _and_ a284 b284
      b285 = _not_ w283
      w285 = _and_ w284 b285
      a286 = _not_ w18
      w286 = _and_ a286 w17
      b287 = _not_ w285
      w287 = _and_ w286 b287
      a288 = _not_ w181
      w288 = _and_ a288 w140
      a289 = _not_ w288
      w289 = _and_ a289 w9
      a290 = _not_ w123
      b290 = _not_ w119
      w290 = _and_ a290 b290
      b291 = _not_ w289
      w291 = _and_ w290 b291
      a292 = _not_ w291
      b292 = _not_ cin3
      w292 = _and_ a292 b292
      a293 = _not_ w292
      b293 = _not_ w41
      w293 = _and_ a293 b293
      a294 = _not_ w293
      w294 = _and_ a294 cin2
      a295 = _not_ w88
      b295 = _not_ cin3
      w295 = _and_ a295 b295
      b296 = _not_ w205
      w296 = _and_ w295 b296
      a297 = _not_ w296
      b297 = _not_ w174
      w297 = _and_ a297 b297
      w298 = _and_ w297 w169
      a299 = _not_ w298
      b299 = _not_ cin2
      w299 = _and_ a299 b299
      a300 = _not_ w299
      b300 = _not_ w10
      w300 = _and_ a300 b300
      b301 = _not_ w294
      w301 = _and_ w300 b301
      a302 = _not_ w301
      w302 = _and_ a302 w160
      a303 = _not_ w148
      w303 = _and_ a303 w18
      b304 = _not_ cin2
      w304 = _and_ w137 b304
      a305 = _not_ w86
      b305 = _not_ w47
      w305 = _and_ a305 b305
      w306 = _and_ w305 w143
      a307 = _not_ w26
      b307 = _not_ cin3
      w307 = _and_ a307 b307
      b308 = _not_ w306
      w308 = _and_ w307 b308
      a309 = _not_ w308
      w309 = _and_ a309 w304
      a310 = _not_ w9
      b310 = _not_ cin3
      w310 = _and_ a310 b310
      b311 = _not_ w65
      w311 = _and_ w305 b311
      a312 = _not_ w311
      w312 = _and_ a312 w310
      a313 = _not_ w312
      w313 = _and_ a313 cin2
      b314 = _not_ w189
      w314 = _and_ w313 b314
      a315 = _not_ w314
      b315 = _not_ w309
      w315 = _and_ a315 b315
      a316 = _not_ w315
      b316 = _not_ w10
      w316 = _and_ a316 b316
      a317 = _not_ w303
      b317 = _not_ cin0
      w317 = _and_ a317 b317
      b318 = _not_ w316
      w318 = _and_ w317 b318
      a319 = _not_ w318
      b319 = _not_ w302
      w319 = _and_ a319 b319
      a320 = _not_ w319
      b320 = _not_ cin1
      w320 = _and_ a320 b320
      a321 = _not_ w287
      b321 = _not_ w165
      w321 = _and_ a321 b321
      b322 = _not_ w320
      w322 = _and_ w321 b322
      a323 = _not_ w178
      b323 = _not_ w17
      w323 = _and_ a323 b323
      b324 = _not_ cin2
      w324 = _and_ w19 b324
      b325 = _not_ w323
      w325 = _and_ w324 b325
      a326 = _not_ w206
      b326 = _not_ w78
      w326 = _and_ a326 b326
      a327 = _not_ w326
      w327 = _and_ a327 w36
      a328 = _not_ w327
      b328 = _not_ w72
      w328 = _and_ a328 b328
      a329 = _not_ w328
      b329 = _not_ cin3
      w329 = _and_ a329 b329
      a330 = _not_ w329
      b330 = _not_ w42
      w330 = _and_ a330 b330
      a331 = _not_ w330
      w331 = _and_ a331 cin2
      a332 = _not_ w222
      w332 = _and_ a332 w161
      a333 = _not_ w332
      w333 = _and_ a333 w191
      a334 = _not_ w333
      b334 = _not_ cin2
      w334 = _and_ a334 b334
      a335 = _not_ w334
      b335 = _not_ w331
      w335 = _and_ a335 b335
      a336 = _not_ w335
      w336 = _and_ a336 w17
      b337 = _not_ cin2
      w337 = _and_ w169 b337
      a338 = _not_ w89
      b338 = _not_ w49
      w338 = _and_ a338 b338
      b339 = _not_ w27
      w339 = _and_ w172 b339
      a340 = _not_ w339
      b340 = _not_ cin3
      w340 = _and_ a340 b340
      w341 = _and_ w340 w338
      a342 = _not_ w341
      w342 = _and_ a342 w337
      a343 = _not_ w108
      b343 = _not_ w58
      w343 = _and_ a343 b343
      a344 = _not_ w52
      w344 = _and_ a344 w5
      a345 = _not_ w344
      w345 = _and_ a345 w343
      a346 = _not_ w345
      w346 = _and_ a346 w310
      a347 = _not_ w214
      w347 = _and_ a347 cin2
      b348 = _not_ w106
      w348 = _and_ w347 b348
      b349 = _not_ w346
      w349 = _and_ w348 b349
      a350 = _not_ w342
      w350 = _and_ a350 w178
      b351 = _not_ w349
      w351 = _and_ w350 b351
      a352 = _not_ w351
      b352 = _not_ w336
      w352 = _and_ a352 b352
      a353 = _not_ w352
      b353 = _not_ w10
      w353 = _and_ a353 b353
      w354 = _and_ w312 cin2
      a355 = _not_ w66
      b355 = _not_ w9
      w355 = _and_ a355 b355
      a356 = _not_ w355
      b356 = _not_ w272
      w356 = _and_ a356 b356
      a357 = _not_ w356
      w357 = _and_ a357 w148
      a358 = _not_ w357
      b358 = _not_ w354
      w358 = _and_ a358 b358
      w359 = _and_ w358 w137
      b360 = _not_ cin1
      w360 = _and_ w196 b360
      b361 = _not_ w359
      w361 = _and_ w360 b361
      a362 = _not_ w361
      b362 = _not_ w325
      w362 = _and_ a362 b362
      b363 = _not_ w353
      w363 = _and_ w362 b363
      a364 = _not_ w243
      b364 = _not_ w187
      w364 = _and_ a364 b364
      a365 = _not_ w364
      b365 = _not_ cin2
      w365 = _and_ a365 b365
      b366 = _not_ w181
      w366 = _and_ w223 b366
      b367 = _not_ w70
      w367 = _and_ w200 b367
      b368 = _not_ w366
      w368 = _and_ w367 b368
      a369 = _not_ w368
      b369 = _not_ w190
      w369 = _and_ a369 b369
      b370 = _not_ w365
      w370 = _and_ w369 b370
      a371 = _not_ w370
      b371 = _not_ w10
      w371 = _and_ a371 b371
      a372 = _not_ w371
      b372 = _not_ w324
      w372 = _and_ a372 b372
      a373 = _not_ w372
      w373 = _and_ a373 w17
      a374 = _not_ w324
      w374 = _and_ a374 cin0
      a375 = _not_ w252
      b375 = _not_ cin3
      w375 = _and_ a375 b375
      w376 = _and_ w375 w338
      a377 = _not_ w376
      w377 = _and_ a377 w337
      a378 = _not_ w210
      w378 = _and_ a378 w171
      w379 = _and_ w378 w343
      b380 = _not_ cin3
      w380 = _and_ w203 b380
      b381 = _not_ w379
      w381 = _and_ w380 b381
      a382 = _not_ w135
      w382 = _and_ a382 cin2
      b383 = _not_ w381
      w383 = _and_ w382 b383
      a384 = _not_ w377
      b384 = _not_ w10
      w384 = _and_ a384 b384
      b385 = _not_ w383
      w385 = _and_ w384 b385
      a386 = _not_ w385
      w386 = _and_ a386 w374
      a387 = _not_ w296
      b387 = _not_ w41
      w387 = _and_ a387 b387
      a388 = _not_ w387
      w388 = _and_ a388 cin2
      b389 = _not_ w63
      w389 = _and_ w122 b389
      a390 = _not_ w389
      w390 = _and_ a390 w149
      a391 = _not_ w390
      b391 = _not_ w388
      w391 = _and_ a391 b391
      a392 = _not_ w391
      b392 = _not_ w10
      w392 = _and_ a392 b392
      a393 = _not_ w392
      w393 = _and_ a393 w134
      a394 = _not_ w386
      b394 = _not_ cin1
      w394 = _and_ a394 b394
      b395 = _not_ w393
      w395 = _and_ w394 b395
      a396 = _not_ w395
      b396 = _not_ w373
      w396 = _and_ a396 b396
      b397 = _not_ w234
      w397 = _and_ w295 b397
      b398 = _not_ w83
      w398 = _and_ w397 b398
      a399 = _not_ w398
      b399 = _not_ cin2
      w399 = _and_ a399 b399
      w400 = _and_ w399 w191
      a401 = _not_ w281
      w401 = _and_ a401 w43
      b402 = _not_ w188
      w402 = _and_ w401 b402
      a403 = _not_ w402
      b403 = _not_ w10
      w403 = _and_ a403 b403
      b404 = _not_ w400
      w404 = _and_ w403 b404
      a405 = _not_ w404
      b405 = _not_ w324
      w405 = _and_ a405 b405
      a406 = _not_ w405
      w406 = _and_ a406 w17
      w407 = _and_ w203 w125
      a408 = _not_ w407
      w408 = _and_ a408 w136
      a409 = _not_ w408
      w409 = _and_ a409 cin2
      a410 = _not_ w409
      b410 = _not_ w299
      w410 = _and_ a410 b410
      a411 = _not_ w410
      b411 = _not_ w10
      w411 = _and_ a411 b411
      a412 = _not_ w411
      w412 = _and_ a412 w374
      a413 = _not_ w296
      b413 = _not_ cin2
      w413 = _and_ a413 b413
      a414 = _not_ w389
      w414 = _and_ a414 w142
      w415 = _and_ w137 cin2
      b416 = _not_ w414
      w416 = _and_ w415 b416
      a417 = _not_ w413
      b417 = _not_ w10
      w417 = _and_ a417 b417
      b418 = _not_ w416
      w418 = _and_ w417 b418
      a419 = _not_ w418
      w419 = _and_ a419 w134
      a420 = _not_ w412
      b420 = _not_ cin1
      w420 = _and_ a420 b420
      b421 = _not_ w419
      w421 = _and_ w420 b421
      a422 = _not_ w421
      b422 = _not_ w406
      w422 = _and_ a422 b422
      a423 = _not_ w296
      b423 = _not_ w138
      w423 = _and_ a423 b423
      b424 = _not_ w397
      w424 = _and_ w423 b424
      a425 = _not_ w424
      b425 = _not_ cin2
      w425 = _and_ a425 b425
      a426 = _not_ w425
      b426 = _not_ w331
      w426 = _and_ a426 b426
      a427 = _not_ w426
      b427 = _not_ w10
      w427 = _and_ a427 b427
      a428 = _not_ w427
      b428 = _not_ w324
      w428 = _and_ a428 b428
      a429 = _not_ w428
      w429 = _and_ a429 w17
      w430 = _and_ w304 w297
      b431 = _not_ w127
      w431 = _and_ w347 b431
      a432 = _not_ w431
      b432 = _not_ w10
      w432 = _and_ a432 b432
      b433 = _not_ w430
      w433 = _and_ w432 b433
      a434 = _not_ w433
      w434 = _and_ a434 w374
      a435 = _not_ w423
      b435 = _not_ w10
      w435 = _and_ a435 b435
      a436 = _not_ w435
      w436 = _and_ a436 w134
      a437 = _not_ w434
      b437 = _not_ cin1
      w437 = _and_ a437 b437
      b438 = _not_ w436
      w438 = _and_ w437 b438
      a439 = _not_ w438
      b439 = _not_ w429
      w439 = _and_ a439 b439
      a440 = _not_ w323
      w440 = _and_ a440 w200
      w441 = _and_ w440 w18
      b442 = _not_ w9
      w442 = _and_ w170 b442
      a443 = _not_ w442
      b443 = _not_ w289
      w443 = _and_ a443 b443
      a444 = _not_ w443
      b444 = _not_ cin3
      w444 = _and_ a444 b444
      a445 = _not_ w444
      b445 = _not_ w168
      w445 = _and_ a445 b445
      a446 = _not_ w445
      w446 = _and_ a446 w178
      a447 = _not_ w70
      b447 = _not_ w23
      w447 = _and_ a447 b447
      a448 = _not_ w447
      w448 = _and_ a448 w397
      a449 = _not_ w448
      b449 = _not_ w187
      w449 = _and_ a449 b449
      a450 = _not_ w449
      w450 = _and_ a450 w17
      a451 = _not_ w450
      b451 = _not_ w446
      w451 = _and_ a451 b451
      a452 = _not_ w10
      w452 = _and_ a452 cin2
      b453 = _not_ w451
      w453 = _and_ w452 b453
      a454 = _not_ w453
      b454 = _not_ w441
      w454 = _and_ a454 b454
      o1 = _not_ w16
      o2 = _not_ w158
      o6 = _not_ w229
      o7 = _not_ w269
  return ((pure False), o1, o2, w166, w179, w195, o6, o7)

  where
    _lat_ = cell False
    _and_ x y = (&&) <$> x <*> y
    _not_ = fmap not

-----------------------------------------------------------------------------
