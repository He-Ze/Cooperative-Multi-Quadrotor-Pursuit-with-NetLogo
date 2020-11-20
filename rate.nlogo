

breed [ leaders leader ]
breed [ followers follower ]
breed [ keys key ]
globals [the-leader flag bex bey ber the-key live stopp lo the-key2]

to setup-axes
  ask patches with [pxcor = 0 or pycor = 0]
    [ set pcolor gray - 3 ]

end

to setup-area
  let p 50
  let i p
  ask [patches in-radius 20] of patch (-40) (10) [ set pcolor gray + 1 ]
  while [i > 0][
    ask [patches in-radius 20] of patch (-60 * i / p) (40 * i / p) [ set pcolor gray + 1 ]
    ask [patches in-radius 20] of patch (-30 * i / p) (0 * i / p) [ set pcolor gray + 1 ]
    ask [patches in-radius 20] of patch (-40 * i / p) (50 * i / p) [ set pcolor gray + 1 ]
    ask [patches in-radius 20] of patch (-30 * i / p) (50 * i / p) [ set pcolor gray + 1 ]
    ask [patches in-radius 20] of patch (-20 * i / p) (55 * i / p) [ set pcolor gray + 1 ]
    ask [patches in-radius 20] of patch (0 * i / p) (20 * i / p) [ set pcolor gray + 1 ]

    set i i - 1
  ]

end


to setup-leader
  create-leaders 1[
    set the-leader self
    set color red
    set size 3
    set shape "circle"
    set xcor 0
    set ycor -70
    set heading 0
    if show-trail?
      [
        set pen-size 1
    pen-down
        ]
      if not show-leader? [ hide-turtle ] ]
end

to move-leader
  ask the-leader
  [
    ifelse auto[
      let t heading
      face turtle 1
      let a heading
      face turtle 2
      let b heading
      face turtle 3
      let c heading
      set heading t
      ifelse flag = 1[
        let banjin 0
        let jiaodu 0
        let jiaodumin 0
        let banjinmin 0
        let hh 0
        while[hh = 0 ][
          set jiaodu 0
          set banjin banjin + 0.5
          while [jiaodu <= 360 and hh = 0][
            set jiaodu jiaodu + 1
            if (xcor + banjin * sin jiaodu) <= max-pxcor and (xcor + banjin * sin jiaodu) >= min-pxcor  and (ycor + banjin * cos jiaodu) <= max-pycor  and  (ycor + banjin * cos jiaodu) >= min-pycor  [
              if [pcolor] of patch (xcor + banjin * sin jiaodu) (ycor + banjin * cos jiaodu) != gray + 1 and [pcolor] of patch (xcor + banjin * sin jiaodu) (ycor + banjin * cos jiaodu) != red + 2[
                set hh 1
                set jiaodumin jiaodu
                set banjinmin banjin
              ]
            ]
          ]
        ]
        ifelse lo > 150[
          ifelse (distance turtle 1 < 1.5 * p-step-size and (abs([heading] of turtle 1 - heading) < 90 or abs([heading] of turtle 1 - heading) > 270)) or (distance turtle 2 < 1.5 * p-step-size and (abs([heading] of turtle 2 - heading) < 90 or abs([heading] of turtle 2 - heading) > 270)) or (distance turtle 3 < 1.5 * p-step-size and (abs([heading] of turtle 3 - heading) < 90 or abs([heading] of turtle 3 - heading) > 270))[
            rt turnangle
            fd r-step-size
          ][
            ifelse abs(heading - jiaodumin) < turnangle [
              set heading jiaodumin
              ifelse random 2 = 1[

                ][
                  ifelse random 2 = 1[
                    lt (random-float 1) * turnangle
                  ][
                    rt (random-float 1) * turnangle
                  ]
                ]
              fd r-step-size
            ][
              ifelse (heading - jiaodumin > 0 and heading - jiaodumin < 180) or (heading - jiaodumin < 0 and heading - jiaodumin < -180) [
                lt turnangle
              ][
                rt turnangle
              ]
              fd r-step-size
            ]
          ]
        ][
          ifelse (distance turtle 1 < 1.5 * p-step-size and (abs([heading] of turtle 1 - heading) < 90 or abs([heading] of turtle 1 - heading) > 270)) or (distance turtle 2 < 1.5 * p-step-size and (abs([heading] of turtle 2 - heading) < 90 or abs([heading] of turtle 2 - heading) > 270)) or (distance turtle 3 < 1.5 * p-step-size and (abs([heading] of turtle 3 - heading) < 90 or abs([heading] of turtle 3 - heading) > 270))[
            lt turnangle
            fd r-step-size
          ][
            ifelse 0 = 1 [
              ifelse abs(heading - jiaodumin) < turnangle [
                set heading jiaodumin
                fd r-step-size
              ][
                ifelse (heading - jiaodumin > 0 and heading - jiaodumin < 180) or (heading - jiaodumin < 0 and heading - jiaodumin < -180) [
                  lt turnangle
                ][
                  rt turnangle
                ]
                fd r-step-size
              ]
            ][
              set jiaodumin jiaodumin - 180
              if jiaodumin < 0 [set jiaodumin jiaodumin + 360]
              if jiaodumin > 360 [set jiaodumin jiaodumin - 360]
              ifelse abs(heading - jiaodumin) < turnangle [
                set heading jiaodumin
                ifelse random 2 = 1[

                ][
                  ifelse random 2 = 1[
                    lt (random-float 1) * turnangle
                  ][
                    rt (random-float 1) * turnangle
                  ]
                ]
                fd r-step-size
              ][
                ifelse (heading - jiaodumin > 0 and heading - jiaodumin < 180) or (heading - jiaodumin < 0 and heading - jiaodumin < -180) [
                  lt turnangle
                ][
                  rt turnangle
                ]
                fd r-step-size
              ]
            ]
          ]

        ]
      ][
        let banjin 0
        let jiaodu 0
        let jiaodumin 0
        let banjinmin 0
        let hh 0
        while[hh = 0 ][
          set jiaodu 0
          set banjin banjin + 0.5
          while [jiaodu <= 360 and hh = 0][
            set jiaodu jiaodu + 1
            if (xcor + banjin * sin jiaodu) <= max-pxcor and (xcor + banjin * sin jiaodu) >= min-pxcor  and (ycor + banjin * cos jiaodu) <= max-pycor  and  (ycor + banjin * cos jiaodu) >= min-pycor  [
              if [pcolor] of patch (xcor + banjin * sin jiaodu) (ycor + banjin * cos jiaodu) = gray + 1[
                set hh 1
                set jiaodumin jiaodu
                set banjinmin banjin
              ]
            ]
          ]
        ]
        ifelse lo < 50 [
          ifelse (distance turtle 1 <  sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))) or (distance turtle 2 < sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))) or (distance turtle 3 < sen *(p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2)))[
            let jiao 0
            let zong 0
            if distance turtle 1 <  sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))[set jiao jiao + a set zong zong + 1]
            if distance turtle 2 <  sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))[set jiao jiao + b set zong zong + 1]
            if distance turtle 3 <  sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))[set jiao jiao + c set zong zong + 1]
            set jiao jiao / zong
            set jiao jiao + 90
            if jiao < 0 [set jiao jiao + 360]
            if jiao > 360 [set jiao jiao - 360]
            ifelse abs(heading - jiao) < turnangle [
              set heading jiao
              fd r-step-size
            ][
              ifelse (heading - jiao > 0 and heading - jiao < 180) or (heading - jiao < 0 and heading - jiao < -180) [
                lt turnangle
              ][
                rt turnangle
              ]
              fd r-step-size
            ]
          ][
            ifelse abs(heading - jiaodumin) < turnangle [
              set heading jiaodumin
              fd r-step-size
            ][
              ifelse (heading - jiaodumin > 0 and heading - jiaodumin < 180) or (heading - jiaodumin < 0 and heading - jiaodumin < -180) [
                lt turnangle
              ][
                rt turnangle
              ]
              fd r-step-size
            ]
          ]
        ][
          ifelse (distance turtle 1 <  2 * sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))) or (distance turtle 2 < 2 * sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))) or (distance turtle 3 < 2 * sen *(p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2)))[
            let jiao 0
            let zong 0
            if distance turtle 1 <  2 * sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))[set jiao jiao + a set zong zong + 1]
            if distance turtle 2 <  2 * sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))[set jiao jiao + b set zong zong + 1]
            if distance turtle 3 <  2 * sen * (p-step-size + r-step-size + (([size] of the-leader) / 2) + (([size] of follower 1) / 2))[set jiao jiao + c set zong zong + 1]
            set jiao jiao / zong
            set jiao jiao - 90
            if jiao < 0 [set jiao jiao + 360]
            if jiao > 360 [set jiao jiao - 360]
            ifelse abs(heading - jiao) < turnangle [
              set heading jiao
              fd r-step-size
            ][
              ifelse (heading - jiao > 0 and heading - jiao < 180) or (heading - jiao < 0 and heading - jiao < -180) [
                lt turnangle
              ][
                rt turnangle
              ]
              fd r-step-size
            ]
          ][
            ifelse banjinmin > 15 * r-step-size[
              ifelse abs(heading - jiaodumin) < turnangle [
                set heading jiaodumin
                fd r-step-size
              ][
                ifelse (heading - jiaodumin > 0 and heading - jiaodumin < 180) or (heading - jiaodumin < 0 and heading - jiaodumin < -180) [
                  lt turnangle
                ][
                  rt turnangle
                ]
                fd r-step-size
              ]
            ][
              ifelse banjinmin < 10 * r-step-size[
                set jiaodumin jiaodumin - 180
                if jiaodumin < 0 [set jiaodumin jiaodumin + 360]
                if jiaodumin > 360 [set jiaodumin jiaodumin - 360]
                ifelse abs(heading - jiaodumin) < turnangle [
                  set heading jiaodumin
                  fd r-step-size
                ][
                  ifelse (heading - jiaodumin > 0 and heading - jiaodumin < 180) or (heading - jiaodumin < 0 and heading - jiaodumin < -180) [
                    lt turnangle
                  ][
                    rt turnangle
                  ]
                  fd r-step-size
                ]
              ][
                set jiaodumin jiaodumin - 90
                if jiaodumin < 0 [set jiaodumin jiaodumin + 360]
                if jiaodumin > 360 [set jiaodumin jiaodumin - 360]
                ifelse abs(heading - jiaodumin) < turnangle [
                  set heading jiaodumin
                  fd r-step-size
                ][
                  ifelse (heading - jiaodumin > 0 and heading - jiaodumin < 180) or (heading - jiaodumin < 0 and heading - jiaodumin < -180) [
                    lt turnangle
                  ][
                    rt turnangle
                  ]
                  fd r-step-size
                ]
              ]
            ]
          ]
        ]
      ]
    ][
      fd r-step-size
    ]
  ]
end


to turnright
  ask the-leader
  [
     rt turnangle
  ]
end

to turnleft
  ask the-leader
  [
     lt turnangle
  ]
end

to setup-follower
  create-followers 3
  [
    ask turtle 1 [ setxy -40 -120 set color green]
    ask turtle 2 [ setxy 0 -120 set color blue]
    ask turtle 3 [ setxy 40 -120 set color pink]
    set shape "circle"
    set size 2
    face the-leader
    if show-trail?
      [
        set pen-size 1
    pen-down
        ]
  ]
end

to move-follower
  ifelse flag = 1 [
    let banjin 0
    let jiaodu 0
    let hh 0
    while[hh = 0][
      set jiaodu 0
      set banjin banjin + 0.1
      while[jiaodu <= 360 and hh = 0][
        set jiaodu jiaodu + 0.1
        if [pcolor] of patch (bex + (banjin * sin jiaodu)) (bey + (banjin * cos jiaodu)) != gray + 1 and [pcolor] of patch (bex + (banjin * sin jiaodu)) (bey + (banjin * cos jiaodu)) != red + 2  [set hh 1]
      ]
    ]
    if jiaodu > 360 [set jiaodu jiaodu - 360]
    if jiaodu < 0 [set jiaodu jiaodu + 360]
    ifelse bex + ( waitt * sin jiaodu) >= max-pxcor[ask the-key2[set xcor max-pxcor]][ifelse bex + ( waitt * sin jiaodu) <= min-pxcor[ask the-key2[set xcor min-pxcor]][ask the-key2[set xcor bex + ( waitt * sin jiaodu)]]]
    ifelse bey + ( waitt * cos jiaodu) >= max-pycor[ask the-key2[set ycor max-pycor]][ifelse bey + ( waitt * cos jiaodu) <= min-pycor[ask the-key2[set ycor min-pycor]][ask the-key2[set ycor bey + ( waitt * cos jiaodu)]]]
    ask turtle 1[
      let f 0
      let p 50
      let i p
      while [i > 0][
        if [pcolor] of patch (xcor + (([xcor] of the-key2 - xcor )* i / p)) (ycor + (([ycor] of the-key2 - ycor )* i / p)) = gray + 1 or [pcolor] of patch (xcor + (([xcor] of the-key2 - xcor )* i / p)) (ycor + (([ycor] of the-key2 - ycor )* i / p)) = red + 2 [
          set f 1
          set i 1
        ]
        set i i - 1
      ]
      ifelse f = 0[
        let t heading
        face the-key2
        let a heading
        set heading t
        ifelse abs(heading - a) < turnangle-p [
          face the-key2
          fd p-step-size
        ][
          ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
            lt turnangle-p
          ][
            rt turnangle-p
          ]
          ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
            let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
            if  s = 0[fd p-step-size / 2]
          ][

          ]
        ]
      ][
        let t heading
        face the-key2
        let a heading
        set heading t
        let b a
        let c 1
        let d 1
        while[c = 1 and d = 1][
          set a a + turnangle-p
          set b b - turnangle-p
          if a > 360 [set a a - 360]
          if b < 0 [set b b + 360]
          set c 0
          set d 0
          let u 0
          while[c = 0 and xcor + (u * sin a) > min-pxcor and xcor + (u * sin a) < max-pxcor and ycor + (u * cos a) < max-pycor and ycor + (u * cos a) > min-pycor][
            if [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = gray + 1 or [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = red + 2[set c 1]
            set u u + 0.1
          ]
          set u 0
          while[d = 0 and xcor + (u * sin b) < max-pxcor and xcor + (u * sin b) > min-pxcor and ycor + (u * cos b) <  max-pycor and ycor + (u * cos b) >  min-pycor][
            if [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = gray + 1 or [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = red + 2[set d 1]
            set u u + 0.1
          ]

        ]
        set a a + turnangle-p
        set b b - turnangle-p
        if a > 360 [set a a - 360]
        if b < 0 [set b b + 360]
        ifelse c = 0[
          ifelse abs(heading - a) < turnangle-p [
            set heading a
            fd p-step-size
          ][
            ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size]
            ][

            ]
          ]
        ][
          ifelse abs(heading - b) < turnangle-p [
            set heading b
            fd p-step-size
          ][
            ifelse (heading - b > 0 and heading - b < 180) or (heading - b < 0 and heading - b < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size / 2]
            ][

            ]
          ]
        ]
      ]
      if [pcolor] of patch xcor ycor = gray + 1 or [pcolor] of patch xcor ycor = red + 2[set stopp 2]
    ]
    set jiaodu jiaodu + 140
    if jiaodu > 360 [set jiaodu jiaodu - 360]
    if jiaodu < 0 [set jiaodu jiaodu + 360]
    ifelse bex + (kk * waitt * sin jiaodu) >= max-pxcor[ask the-key2[set xcor max-pxcor]][ifelse bex + (kk * waitt * sin jiaodu) <= min-pxcor[ask the-key2[set xcor min-pxcor]][ask the-key2[set xcor bex + (kk * waitt * sin jiaodu)]]]
    ifelse bey + (kk * waitt * cos jiaodu) >= max-pycor[ask the-key2[set ycor max-pycor]][ifelse bey + (kk * waitt * cos jiaodu) <= min-pycor[ask the-key2[set ycor min-pycor]][ask the-key2[set ycor bey + (kk * waitt * cos jiaodu)]]]
    ask turtle 2[
      let f 0
      let p 50
      let i p
      while [i > 0][
        if [pcolor] of patch (xcor + (([xcor] of the-key2 - xcor )* i / p)) (ycor + (([ycor] of the-key2 - ycor )* i / p)) = gray + 1 or [pcolor] of patch (xcor + (([xcor] of the-key2 - xcor )* i / p)) (ycor + (([ycor] of the-key2 - ycor )* i / p)) = red + 2 [
          set f 1
          set i 1
        ]
        set i i - 1
      ]
      ifelse f = 0[
        let t heading
        face the-key2
        let a heading
        set heading t
        ifelse abs(heading - a) < turnangle-p [
          face the-key2
          fd p-step-size
        ][
          ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
            lt turnangle-p
          ][
            rt turnangle-p
          ]
          ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
            let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
            if  s = 0[fd p-step-size / 2]
          ][

          ]
        ]
      ][
        let t heading
        face the-key2
        let a heading
        set heading t
        let b a
        let c 1
        let d 1
        while[c = 1 and d = 1][
          set a a + turnangle-p
          set b b - turnangle-p
          if a > 360 [set a a - 360]
          if b < 0 [set b b + 360]
          set c 0
          set d 0
          let u 0
          while[c = 0 and xcor + (u * sin a) > min-pxcor and xcor + (u * sin a) < max-pxcor and ycor + (u * cos a) < max-pycor and ycor + (u * cos a) > min-pycor][
            if [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = gray + 1 or [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = red + 2[set c 1]
            set u u + 0.1
          ]
          set u 0
          while[d = 0 and xcor + (u * sin b) < max-pxcor and xcor + (u * sin b) > min-pxcor and ycor + (u * cos b) <  max-pycor and ycor + (u * cos b) >  min-pycor][
            if [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = gray + 1 or [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = red + 2[set d 1]
            set u u + 0.1
          ]

        ]
        set a a + turnangle-p
        set b b - turnangle-p
        if a > 360 [set a a - 360]
        if b < 0 [set b b + 360]
        ifelse c = 0[
          ifelse abs(heading - a) < turnangle-p [
            set heading a
            fd p-step-size
          ][
            ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size]
            ][

            ]
          ]
        ][
          ifelse abs(heading - b) < turnangle-p [
            set heading b
            fd p-step-size
          ][
            ifelse (heading - b > 0 and heading - b < 180) or (heading - b < 0 and heading - b < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size / 2]
            ][

            ]
          ]
        ]
      ]
      if [pcolor] of patch xcor ycor = gray + 1 or [pcolor] of patch xcor ycor = red + 2[set stopp 2]
    ]
    set jiaodu jiaodu + 90
    if jiaodu > 360 [set jiaodu jiaodu - 360]
    if jiaodu < 0 [set jiaodu jiaodu + 360]
    ifelse bex + (kk * waitt * sin jiaodu) >= max-pxcor[ask the-key2[set xcor max-pxcor]][ifelse bex + (kk * waitt * sin jiaodu) <= min-pxcor[ask the-key2[set xcor min-pxcor]][ask the-key2[set xcor bex + (kk * waitt * sin jiaodu)]]]
    ifelse bey + (kk * waitt * cos jiaodu) >= max-pycor[ask the-key2[set ycor max-pycor]][ifelse bey + (kk * waitt * cos jiaodu) <= min-pycor[ask the-key2[set ycor min-pycor]][ask the-key2[set ycor bey + (kk * waitt * cos jiaodu)]]]
    ask turtle 3[
      let f 0
      let p 50
      let i p
      while [i > 0][
        if [pcolor] of patch (xcor + (([xcor] of the-key2 - xcor )* i / p)) (ycor + (([ycor] of the-key2 - ycor )* i / p)) = gray + 1 or [pcolor] of patch (xcor + (([xcor] of the-key2 - xcor )* i / p)) (ycor + (([ycor] of the-key2 - ycor )* i / p)) = red + 2 [
          set f 1
          set i 1
        ]
        set i i - 1
      ]
      ifelse f = 0[
        let t heading
        face the-key2
        let a heading
        set heading t
        ifelse abs(heading - a) < turnangle-p [
          face the-key2
          fd p-step-size
        ][
          ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
            lt turnangle-p
          ][
            rt turnangle-p
          ]
          ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
            let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
            if  s = 0[fd p-step-size / 2]
          ][

          ]
        ]
      ][
        let t heading
        face the-key2
        let a heading
        set heading t
        let b a
        let c 1
        let d 1
        while[c = 1 and d = 1][
          set a a + turnangle-p
          set b b - turnangle-p
          if a > 360 [set a a - 360]
          if b < 0 [set b b + 360]
          set c 0
          set d 0
          let u 0
          while[c = 0 and xcor + (u * sin a) > min-pxcor and xcor + (u * sin a) < max-pxcor and ycor + (u * cos a) < max-pycor and ycor + (u * cos a) > min-pycor][
            if [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = gray + 1 or [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = red + 2[set c 1]
            set u u + 0.1
          ]
          set u 0
          while[d = 0 and xcor + (u * sin b) < max-pxcor and xcor + (u * sin b) > min-pxcor and ycor + (u * cos b) <  max-pycor and ycor + (u * cos b) >  min-pycor][
            if [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = gray + 1 or [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = red + 2[set d 1]
            set u u + 0.1
          ]

        ]
        set a a + turnangle-p
        set b b - turnangle-p
        if a > 360 [set a a - 360]
        if b < 0 [set b b + 360]
        ifelse c = 0[
          ifelse abs(heading - a) < turnangle-p [
            set heading a
            fd p-step-size
          ][
            ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size]
            ][

            ]
          ]
        ][
          ifelse abs(heading - b) < turnangle-p [
            set heading b
            fd p-step-size
          ][
            ifelse (heading - b > 0 and heading - b < 180) or (heading - b < 0 and heading - b < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-key2 - (([size] of the-key2) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size / 2]
            ][

            ]
          ]
        ]
      ]
      if [pcolor] of patch xcor ycor = gray + 1 or [pcolor] of patch xcor ycor = red + 2[set stopp 2]
    ]
  ][
    ask followers[
      let f 0
      let p 50
      let i p
      while [i > 0][
        if [pcolor] of patch (xcor + (([xcor] of the-leader - xcor )* i / p)) (ycor + (([ycor] of the-leader - ycor )* i / p)) = gray + 1 or [pcolor] of patch (xcor + (([xcor] of the-leader - xcor )* i / p)) (ycor + (([ycor] of the-leader - ycor )* i / p)) = red + 2 [
          set f 1
          set i 1
        ]
        set i i - 1
      ]
      ifelse f = 0[
        let t heading
        face the-leader
        let a heading
        set heading t
        ifelse abs(heading - a) < turnangle-p [
          face the-leader
          ifelse (distance the-leader - (([size] of the-leader) / 2) - (([size] of follower 1) / 2)) > p-step-size[
            fd p-step-size
          ][
            fd p-step-size
            ;;move-to the-leader
            set stopp 1
          ]
        ][
          ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
            lt turnangle-p
          ][
            rt turnangle-p
          ]
          ifelse (distance the-leader - (([size] of the-leader) / 2) - (([size] of follower 1) / 2)) > 0[
            let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
            if  s = 0[fd p-step-size / 2]
          ][
            ;;move-to the-leader
            set stopp 1
          ]
        ]
      ][
        let t heading
        face the-leader
        let a heading
        set heading t
        let b a
        let c 1
        let d 1
        while[c = 1 and d = 1][
          set a a + turnangle-p
          set b b - turnangle-p
          if a > 360 [set a a - 360]
          if b < 0 [set b b + 360]
          set c 0
          set d 0
          let u 0
          while[c = 0 and xcor + (u * sin a) > min-pxcor and xcor + (u * sin a) < max-pxcor and ycor + (u * cos a) < max-pycor and ycor + (u * cos a) > min-pycor][
            if [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = gray + 1 or [pcolor] of patch (xcor + (u * sin a)) (ycor + (u * cos a)) = red + 2[set c 1]
            set u u + 0.1
          ]
          set u 0
          while[d = 0 and xcor + (u * sin b) < max-pxcor and xcor + (u * sin b) > min-pxcor and ycor + (u * cos b) <  max-pycor and ycor + (u * cos b) >  min-pycor][
            if [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = gray + 1 or [pcolor] of patch (xcor + (u * sin b)) (ycor + (u * cos b)) = red + 2[set d 1]
            set u u + 0.1
          ]

        ]
        set a a + turnangle-p
        set b b - turnangle-p
        if a > 360 [set a a - 360]
        if b < 0 [set b b + 360]
        ifelse c = 0[
          ifelse abs(heading - a) < turnangle-p [
            set heading a
            ifelse (distance the-leader - (([size] of the-leader) / 2) - (([size] of follower 1) / 2)) > p-step-size[
              fd p-step-size
            ][
              fd p-step-size
              ;;move-to the-leader
              set stopp 1
            ]
          ][
            ifelse (heading - a > 0 and heading - a < 180) or (heading - a < 0 and heading - a < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-leader - (([size] of the-leader) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius (p-step-size + [size] of follower 1)] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size]
            ][
              ;;move-to the-leader
              set stopp 1
            ]
          ]
        ][
          ifelse abs(heading - b) < turnangle-p [
            set heading b
            ifelse (distance the-leader - (([size] of the-leader) / 2) - (([size] of follower 1) / 2)) > p-step-size[
              fd p-step-size
            ][
              fd p-step-size
              ;;move-to the-leader
              set stopp 1
            ]
          ][
            ifelse (heading - b > 0 and heading - b < 180) or (heading - b < 0 and heading - b < -180) [
              lt turnangle-p
            ][
              rt turnangle-p
            ]
            ifelse (distance the-leader - (([size] of the-leader) / 2) - (([size] of follower 1) / 2)) > 0[
              let s count ([patches in-radius ((p-step-size + [size] of follower 1))] of patch xcor ycor) with [pcolor = gray + 1 or pcolor = red + 2]
              if  s = 0[fd p-step-size / 2]
            ][
              ;;move-to the-leader
              set stopp 1
            ]
          ]
        ]
      ]
      if [pcolor] of patch xcor ycor = gray + 1 or [pcolor] of patch xcor ycor = red + 2[set stopp 2]
    ]
  ]
end

to setup-key
  create-keys 1[
    set the-key self
    set xcor 0
    set ycor 0
    hide-turtle
 ]
  create-keys 1[
    set the-key2 self
    set xcor 0
    set ycor 0
    hide-turtle
 ]

end

to dangerplace
  ask the-leader[ set live count neighbors with [pcolor = gray + 1 or pcolor = red + 2] ]
  ifelse live != 0[
    ifelse flag = 1  [
      set ber ber + r-step-size
      ask patches[if distance the-key < ber and pcolor = gray + 1[set pcolor red + 2]]
    ][
      set flag 1
      set bex [xcor] of the-leader
      set bey [ycor] of the-leader
      set ber 1
      ask the-key[setxy bex bey]
      ask patches[if distance the-key < ber and pcolor = gray + 1[set pcolor red + 2]]
    ]
  ][
    ifelse flag = 1  [
      set flag 0
      set bex 0
      set bey 0
      set ber 0
      ask the-key[setxy bex bey]
      ask patches[if pcolor = red + 2[set pcolor gray + 1]]
    ][
      set bex 0
      set bey 0
      set ber 0
      ask the-key[setxy bex bey]
    ]
  ]




end

to test
  let i 0
  let pj 0
  let best 0
  while[i < testround][
    while[stopp = 0 and flag = 0 and i < testround][
      go-once
      if stoptest [set i testround + 1]
      ifelse flag = 1[
        set lo lo + 2
        if lo >= 200 [
          set stopp 3
        ]
      ][
        if lo != 0 [set lo lo - 1]
      ]
    ]
    while[stopp = 0 and flag = 1 and i < testround][
      go-once
      if stoptest [set i testround + 1]
      ifelse flag = 1[
        set lo lo + 2
        if lo >= 200 [
          set stopp 3
        ]
      ][
        if lo != 0 [set lo lo - 1]
      ]
    ]
    type "round "
    type i + 1
    print " begins"
    while[stopp = 0 and i < testround and flag = 0][
      go-once
      if stoptest [set i testround + 1]
      ifelse flag = 1[
        set lo lo + 2
        if lo >= 200 [
          set stopp 3
        ]
      ][
        if lo != 0 [set lo lo - 1]
      ]
    ]
    ifelse stopp = 0[
      if flag = 1 [set pj ((i * pj) + 0) / (i + 1)]
    ][
      if stopp = 1 or stopp = 3[set pj ((i * pj) + 1) / (i + 1)]
      if stopp = 2 [set pj ((i * pj) + 0) / (i + 1)]
      setup
    ]
    type "average catched rate: "
    type pj * 100
    print "%"
    set i i + 1
  ]
end

to go
  ifelse flag = 1[
    set lo lo + 2
    if lo >= 200 [
      print "overtime!"
      print "escaper failed"
      print "score is"
      print ticks
      stop
    ]
  ][
    if lo != 0 [set lo lo - 1]
  ]
  if stopp != 0 [
    if stopp = 1 [print "catched!" print "escaper failed" print "score is" print ticks]
    if stopp = 2 [print "overstep!" print "pursuers failed" print "score is" print 2000000000]
    stop ]
  print lo
  go-once
end

to go-once
  move-leader
  move-follower
  dangerplace
  tick
end


to setup
  clear-all
  set stopp 0
  set lo 0
  setup-axes
  setup-area
  setup-leader
  setup-follower
  setup-key
  ifelse [pcolor] of patch ([xcor] of the-leader) ([ycor] of the-leader) = gray + 1[
    set flag 1
    set bex [xcor] of the-leader
    set bey [ycor] of the-leader
    set ber 1
    ask the-key[setxy bex bey]
    ask the-leader[ set live count neighbors with [pcolor = gray + 1 or pcolor = red + 2] ]
  ][
    set flag 0
    set bex 0
    set bey 0
    set ber 0
    ask the-key[setxy bex bey]
    ask the-leader[ set live count neighbors with [pcolor = gray + 1 or pcolor = red + 2] ]
  ]

  dangerplace

  reset-ticks
end
@#$#@#$#@
GRAPHICS-WINDOW
383
10
993
621
-1
-1
2.0
1
10
1
1
1
0
0
0
1
-150
150
-150
150
1
1
1
ticks
15.0

BUTTON
25
35
91
68
NIL
setup
NIL
1
T
OBSERVER
NIL
R
NIL
NIL
1

BUTTON
27
174
107
207
NIL
go-once
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
27
84
90
117
NIL
go
T
1
T
OBSERVER
NIL
P
NIL
NIL
1

SWITCH
194
30
342
63
show-trail?
show-trail?
1
1
-1000

SWITCH
204
76
345
109
show-leader?
show-leader?
0
1
-1000

SLIDER
182
138
354
171
turnangle
turnangle
1
180
30.0
1
1
NIL
HORIZONTAL

SLIDER
186
191
358
224
r-step-size
r-step-size
1
10
4.0
1
1
NIL
HORIZONTAL

BUTTON
193
244
280
277
NIL
turnleft
NIL
1
T
OBSERVER
NIL
A
NIL
NIL
1

BUTTON
193
295
287
328
NIL
turnright
NIL
1
T
OBSERVER
NIL
D
NIL
NIL
1

SLIDER
188
345
360
378
p-step-size
p-step-size
1
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
188
408
360
441
turnangle-p
turnangle-p
1
180
10.0
1
1
NIL
HORIZONTAL

SLIDER
9
393
181
426
waitt
waitt
0
200
40.0
1
1
NIL
HORIZONTAL

SLIDER
8
354
180
387
kk
kk
1
5
3.0
1
1
NIL
HORIZONTAL

SWITCH
28
127
131
160
auto
auto
0
1
-1000

BUTTON
28
219
91
252
NIL
test
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
25
305
138
338
stoptest
stoptest
1
1
-1000

SLIDER
14
262
186
295
testround
testround
0
2000
100.0
10
1
NIL
HORIZONTAL

SLIDER
10
447
182
480
sen
sen
0
10
1.0
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

a pursuit game with a place where the pursuers can not get in (no-ﬂy zones)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
