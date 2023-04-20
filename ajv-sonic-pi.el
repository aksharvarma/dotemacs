(provide 'ajv-sonic-pi)

(use-package sonic-pi)

(defun ajv/sonic-pi/filename-extension-spi-p ()
  (interactive)
  (and (buffer-file-name)
       (string-match ".*?\\.spi" (buffer-file-name))))

(defun ajv/sonic-pi/initialize-mode ()
  (interactive)
  (ruby-mode)
  (sonic-pi-mode))

(defvar ajv/sonic-pi/list-of-synths
  (list "use_synth :beep" "use_synth :blade" "use_synth :bnoise" "use_synth :chipbass" "use_synth :chiplead" "use_synth :chipnoise" "use_synth :cnoise" "use_synth :dark_ambience" "use_synth :dpulse" "use_synth :dsaw" "use_synth :dtri" "use_synth :dull_bell" "use_synth :fm" "use_synth :gnoise" "use_synth :growl" "use_synth :hollow" "use_synth :hoover" "use_synth :kalimba" "use_synth :mod_beep" "use_synth :mod_dsaw" "use_synth :mod_fm" "use_synth :mod_pulse" "use_synth :mod_saw" "use_synth :mod_sine" "use_synth :mod_tri" "use_synth :noise" "use_synth :piano" "use_synth :pluck" "use_synth :pnoise" "use_synth :pretty_bell" "use_synth :prophet" "use_synth :pulse" "use_synth :rodeo" "use_synth :saw" "use_synth :sine" "use_synth :sound_in" "use_synth :sound_in_stereo" "use_synth :square" "use_synth :subpulse" "use_synth :supersaw" "use_synth :tb303" "use_synth :tech_saws" "use_synth :tri" "use_synth :zawa"))

(defvar ajv/sonic-pi/list-of-samples
  (list "sample :ambi_soft_buzz" "sample :ambi_swoosh" "sample :ambi_drone" "sample :ambi_glass_hum" "sample :ambi_glass_rub" "sample :ambi_haunted_hum" "sample :ambi_piano" "sample :ambi_lunar_land" "sample :ambi_dark_woosh" "sample :ambi_choir" "sample :ambi_sauna" "sample :bd_ada" "sample :bd_pure" "sample :bd_808" "sample :bd_zum" "sample :bd_gas" "sample :bd_sone" "sample :bd_haus" "sample :bd_zome" "sample :bd_boom" "sample :bd_klub" "sample :bd_fat" "sample :bd_tek" "sample :bd_mehackit" "sample :bass_hit_c" "sample :bass_hard_c" "sample :bass_thick_c" "sample :bass_drop_c" "sample :bass_woodsy_c" "sample :bass_voxy_c" "sample :bass_voxy_hit_c" "sample :bass_dnb_f" "sample :bass_trance_c" "sample :drum_heavy_kick" "sample :drum_tom_mid_soft" "sample :drum_tom_mid_hard" "sample :drum_tom_lo_soft" "sample :drum_tom_lo_hard" "sample :drum_tom_hi_soft" "sample :drum_tom_hi_hard" "sample :drum_splash_soft" "sample :drum_splash_hard" "sample :drum_snare_soft" "sample :drum_snare_hard" "sample :drum_cymbal_soft" "sample :drum_cymbal_hard" "sample :drum_cymbal_open" "sample :drum_cymbal_closed" "sample :drum_cymbal_pedal" "sample :drum_bass_soft" "sample :drum_bass_hard" "sample :drum_cowbell" "sample :drum_roll" "sample :elec_triangle" "sample :elec_snare" "sample :elec_lo_snare" "sample :elec_hi_snare" "sample :elec_mid_snare" "sample :elec_cymbal" "sample :elec_soft_kick" "sample :elec_filt_snare" "sample :elec_fuzz_tom" "sample :elec_chime" "sample :elec_bong" "sample :elec_twang" "sample :elec_wood" "sample :elec_pop" "sample :elec_beep" "sample :elec_blip" "sample :elec_blip2" "sample :elec_ping" "sample :elec_bell" "sample :elec_flip" "sample :elec_tick" "sample :elec_hollow_kick" "sample :elec_twip" "sample :elec_plip" "sample :elec_blup" "sample :glitch_bass_g" "sample :glitch_perc1" "sample :glitch_perc2" "sample :glitch_perc3" "sample :glitch_perc4" "sample :glitch_perc5" "sample :glitch_robot1" "sample :glitch_robot2" "sample :mehackit_phone1" "sample :mehackit_phone2" "sample :mehackit_phone3" "sample :mehackit_phone4" "sample :mehackit_robot1" "sample :mehackit_robot2" "sample :mehackit_robot3" "sample :mehackit_robot4" "sample :mehackit_robot5" "sample :mehackit_robot6" "sample :mehackit_robot7" "sample :misc_burp" "sample :misc_crow" "sample :misc_cineboom" "sample :perc_bell" "sample :perc_bell2" "sample :perc_snap" "sample :perc_snap2" "sample :perc_swash" "sample :perc_till" "sample :perc_door" "sample :perc_impact1" "sample :perc_impact2" "sample :perc_swoosh" "sample :sn_dub" "sample :sn_dolf" "sample :sn_zome" "sample :sn_generic" "sample :guit_harmonics" "sample :guit_e_fifths" "sample :guit_e_slide" "sample :guit_em9" "sample :loop_industrial" "sample :loop_compus" "sample :loop_amen" "sample :loop_amen_full" "sample :loop_garzul" "sample :loop_mika" "sample :loop_breakbeat" "sample :loop_safari" "sample :loop_tabla" "sample :loop_3d_printer" "sample :loop_drone_g_97" "sample :loop_electric" "sample :loop_mehackit1" "sample :loop_mehackit2" "sample :loop_perc1" "sample :loop_perc2" "sample :loop_weirdo" "sample :tabla_tas1" "sample :tabla_tas2" "sample :tabla_tas3" "sample :tabla_ke1" "sample :tabla_ke2" "sample :tabla_ke3" "sample :tabla_na" "sample :tabla_na_o" "sample :tabla_tun1" "sample :tabla_tun2" "sample :tabla_tun3" "sample :tabla_te1" "sample :tabla_te2" "sample :tabla_te_ne" "sample :tabla_te_m" "sample :tabla_ghe1" "sample :tabla_ghe2" "sample :tabla_ghe3" "sample :tabla_ghe4" "sample :tabla_ghe5" "sample :tabla_ghe6" "sample :tabla_ghe7" "sample :tabla_ghe8" "sample :tabla_dhec" "sample :tabla_na_s" "sample :tabla_re" "sample :vinyl_backspin" "sample :vinyl_rewind" "sample :vinyl_scratch" "sample :vinyl_hiss"))

(defvar ajv/sonic-pi/list-of-fxs
  (list "with_fx :autotuner" "with_fx :band_eq" "with_fx :bitcrusher" "with_fx :bpf" "with_fx :compressor" "with_fx :distortion" "with_fx :echo" "with_fx :eq" "with_fx :flanger" "with_fx :gverb" "with_fx :hpf" "with_fx :ixi_techno" "with_fx :krush" "with_fx :level" "with_fx :lpf" "with_fx :mono" "with_fx :nbpf" "with_fx :nhpf" "with_fx :nlpf" "with_fx :normaliser" "with_fx :nrbpf" "with_fx :nrhpf" "with_fx :nrlpf" "with_fx :octaver" "with_fx :pan" "with_fx :panslicer" "with_fx :ping_pong" "with_fx :pitch_shift" "with_fx :rbpf" "with_fx :record" "with_fx :reverb" "with_fx :rhpf" "with_fx :ring_mod" "with_fx :rlpf" "with_fx :slicer" "with_fx :sound_out" "with_fx :sound_out_stereo" "with_fx :tanh" "with_fx :tremolo" "with_fx :vowel" "with_fx :whammy" "with_fx :wobble"))

(defun ajv/sonic-pi/company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ajv/sonic-pi/company-backend))
    (prefix (and (eq major-mode 'sonic-pi-mode)
                 (member (company-grab-symbol)
			 (list "with_fx" "sample" "use_synth"))))
    (candidates
     ;; (progn
     ;;   (message (format "Symbol caught: %s" arg))
     ;;   (if (equal arg "with_fx")
     ;; 	   ajv/sonic-pi/list-of-fxs
     ;; 	 (if (equal arg "sample")
     ;; 	     ajv/sonic-pi/list-of-samples)
     ;; 	 (if (equal arg "use_synth")
     ;; 	     ajv/sonic-pi/list-of-synths)))
     (progn
       (message (format "Symbol caught: %s" arg))
       (cond
	((equal arg "with_fx") ajv/sonic-pi/list-of-fxs)
	((equal arg "use_synth") ajv/sonic-pi/list-of-synths)
	((equal arg "sample") ajv/sonic-pi/list-of-samples)
	(t (concat ajv/sonic-pi/list-of-fxs ajv/sonic-pi/list-of-samples ajv/sonic-pi/list-of-synths)))))))
