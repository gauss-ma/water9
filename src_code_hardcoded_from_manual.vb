WATER9: SOURCE CODE
23-3
Sub Antoines(t1, v1, t2, v2, t3, v3, a, bb, c)
If v3 > 0 Then
c = 1 / ((Log(v3 / v2) / Log(v2 / v1) * (t2 - t1) / (t3 - t2) - 1) / (t3 -
t1)) - t3
Else: c = 273.16 ' default for c ln(vp)= A exp(B/T(K))
End If
If v1 > 0 And v2 > 0 And Abs(t2 - t1) > 0 Then
bb = Log(v2 / v1) / 2.3026 / (t2 - t1) * (t1 + c) * (t2 + c)
a = Log(v1) / 2.3026 + bb / (t1 + c)
ElseIf v1 > 0 Then
bb = 0: a = Log(v1) / 2.3026: c = 0
Else: a = 0: bb = 0: c = 0
End If
End Sub
Sub AERATED(nt%)
'calculates aerated impoundment fate
'_______ input variables___________
Call sets8(nt%, n%, ci, v, q, T)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
Length = asgn8(3) * 100 ' cm
ta = Length * asgn8(4) / 100 'Total area m2
le(nt%).AREA = ta
depth = asgn8(5) * 100 ' cm
If L(nt%).typ = 30 Then 'diffused air sludge
fragi = asgn8(6)
frqu = asgn8(7)
Else aag = asgn8(6) ' m2
na = asgn8(7)
power = asgn8(8) ' hp
dia = asgn8(9) ' cm
RPM = asgn8(10)
eff = asgn8(11)
alpha = asgn8(12)
End If
plugflow = asgn8(13) ' =1 if plug flow
bioavg = asgn8(14) ' mg/g-bio-hr
xb = asgn8(16) ' g/l biomass
controlf = 0
If L(nt%).typ = 38 Then
controlf = asgn8(18)
End If
airMps = asgn8(15)
updateventin nt%, plugflow, ci, q, ta * asgn8(5) * n%, airMps, vair
air = airMps * 1000 * 3600 'Diffused air l/hr
' submerged air independent of cover air
' submerged air at equilibrium
' cover air may or may not be at equilibrium
volpump = asgn8(18) * na 'm3/s from aerators
covern = asgn8(17)
If covern > 0 And covern <> 1 Then 'specified cover vent rate
WATER9: SOURCE CODE
23-4
'm3/m2 surface per second
coverair = covern * 1000 * 3600 * ta 'L/hr
cover% = 1
ElseIf covern = 1 Then '=1 for cover with no coverair
cover% = 1: coverair = 0
ElseIf covern < 0 Then 'negative cover for control efficiency
controlf = -covern: If controlf > 1 Then controlf = 1
cover% = 1
Else
cover% = 0
End If
If n% = 0 Or q = 0 Then Exit Sub ' no source
ql = q * 1000 * 3600 / n% ' L/hr per unit
If showprint = 1 Then
meanrtime = asgn8(5) * asgn8(3) * asgn8(4) * 1000 / ql
ppprnt "The residence time in the unit is " + FORMP(meanrtime) + " hr."
If cover% = 1 Then
ppprnt "The aerated unit has a cover."
If controlf > 0 Then ppprnt "The control efficiency of the vent is "
+ FORMP(controlf) + "."
ppprnt "The vent flow rate is " + FORMP(asgn8(15)) + " m3/s."
Call printprop2(k1, vmax, dl, dv, vp, Hl, T)
End If
End If
'___ calculates overall mass transfer coef.
If L(nt%).typ = 30 Then 'diffused air biotreatment
areaagit = ta * fragi
areaQuies = ta * frqu
ta = areaagit + areaQuies
na = Int(areaagit / 23): If na < 1 Then na = 1
aag = ta / na
ko = KAC8(Length, depth, 1200, 60, dv, 15, 0.85, 0.86, aag, na, dl, Hl *
55555, ta)
Else
If eff < 0.01 Then eff = 0.01
ko = KAC8(Length, depth, RPM, dia, dv, power, eff, alpha, aag, na, dl, Hl *
55555, ta)
End If
airdens = 1 / 0.0224 * 273 / (273 + T) ' g-m/m3
vol = depth * ta * 10 ' volume of impoundment, l
rtime = vol / ql ' residence time, hr
If volpump > 0 Then pumptime = vol / volpump / 1000 / 60 'min
If cover% = 1 Then
If coverair = 0 Then
ko = 1E-20: For i = 172 To 177: ASGN2(i) = ko: Next
kair = ko / depth * 100 * 3600 ' hr-1
Else
'transfer from surface
kair = ko / depth * 100 * 3600 ' hr-1
'equilibrium limit in cover air
kcover = coverair * Hl * airdens / vol ' hr-1
' if greater than equilibrium limit, use equilibrium limit
If kcover < kair Then kair = kcover
End If
Else
controlf = 0
WATER9: SOURCE CODE
23-5
kair = ko / depth * 100 * 3600 ' hr-1
End If
'_____________________________________
'fremsusp,fremsolids,caddsolids,caddsusp,outsolids,outsusp
totalin nt%, sumSolids, sumBiomass, sumOil, sumDiss 'g/s
inletsolids = (sumSolids + sumBiomass + sumOil) * 3600 * 1000 'mg/hr
ksl = 10 ^ (0.67 * aa.low - 2.61) + 0.099 ' g/Kg biomass per g/m3
ffmax = sumDiss * 3600 * 1000 * 0.5 ' maximum biomass generated mg/hr
ff = vol * bioavg * xb * 0.5 ' mg biomass generated/hr
If ff > ffmax Then ff = ffmax * 0.95
'le(nt%).Mrds = 0.5 * le(nt%).Mrds + 0.5 * ff / 1800 ' mg/s dissolved solids
removed
If ffmax > 0 Then le(nt%).Mrds = (ff / ffmax) Else le(nt%).Mrds = 0 '
le(nt%).bios = ff / 1000 / 3600 ' g/s new biomass
ff = ff + inletsolids
If L(nt%).typ = 24 Then ff = 0 'no sorption removal in mix tank
'note solids and oil partitioning independent of specified biomass
'____________________________________
' ff = ff + xb * q * 3600 * 1000 'includes adsorption in biomass
Kads = ff * 0.001 * ksl / vol ' /hr
kdiff = air * Hl * airdens / vol ' hr-1
Kother = kair + Kads + kdiff
If showprint = 1 And cover% = 1 Then
ppprnt "kdiff (/hr) = air * hl * airdens / vol"
ppprnt " The air rate is " + FORMP(air) + " L per hr."
ppprnt " The Henry's law constant for air stripping is " + FORMP(Hl) +
" atm-m3/gm mol."
ppprnt " The air density is " + FORMP(airdens) + " g-m/m3."
ppprnt " The volume of the system is " + FORMP(vol) + " L."
ppprnt " The rate constant for submerged bubbles is " + FORMP(kdiff) +
" per hr."
End If
'____calculate the air emissions and residuals..
dissol = 0
fract28 ci, xb, vmax, Kother, kair, solids, dissol, rtime, Co, fbio, fair,
fraleft, plugflow, ksl, fads, vair
If Kother > 0 Then
fdiff = kdiff / Kother * fair
fads = Kads / Kother * fair
fair = kair / Kother * fair
Else: fdiff = 0: fads = 0
End If
'___print the results______
ASGN2(180) = fair
fair = fair + fdiff
ASGN2(181) = fdiff
If (kair + kdiff) > 0 Then ASGN2(179) = 60 / (kair + kdiff) Else ASGN2(179) = 0
'stripping time constant (min)
Call setx8(0, 0, 0, fair, fbio, 0, fads)
Call sumrates8(nt%, fair, fbio, fads, ta, controlf)
If pumptime > 0 Then ASGN2(182) = ASGN2(179) * 0.693 / pumptime / 3 Else
ASGN2(182) = 0
End Sub
WATER9: SOURCE CODE
23-6
Sub APISEPARATOR(nt%)
Call sets8(nt%, n%, ci, v, q, T)
If n% = 0 Or q = 0 Then Exit Sub
areaenter = asgn8(3) 'm2
areaoil = asgn8(4) 'm2
fractoil = asgn8(5) 'fraction of separation surface covered with oil
oilfract = asgn8(6) / 100 'fraction oil in total waste
densoil = asgn8(7) 'g/cm3
mwtoil = asgn8(8)
drop = asgn8(9) ' cm
widthFall = asgn8(10) ' m
removal = asgn8(11) ' fraction of oil removed by separator
ta = areaenter + areaoil
le(nt%).AREA = ta
dia = Sqr(areaenter * 4 / 3.14) 'width m
ASGN2(107) = dia
totalin nt%, sumSolids, sumBiomass, sumOil, sumDiss 'g/s
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
If oilfract = 0 Then
Call fractionoil(oilfract, owpc, owr, frinoil, nt%)
Else
Call fractionoil(oilfract, owpc, owr, frinoil, 0) 'use specification
End If
If showprint = 1 Then ppprnt "The oil corrected aqueous HL is " + Format$(Hl
* (1 - frinoil) * 55555, "#.###e+00 (y/x)")
'____entrance zone______
kg = KGC8(v, dia * 100, dv, 2) ' Gmol/cc-sec
kl = KLC8(v, 180, dl, T, 6, 0, 1) ' Gmol/cc-sec
ko = 1 / (1 / kl + 1 / Hl / (1 - frinoil) / 55555 / kg)
Mtr = ko * 0.18 * areaenter * n% / q
frEnt = 1 - Exp(-Mtr)
'____oil covered separation zone______
Keq = vp * 0.0012 * mwtoil / densoil / 28.8 / 760
k = Keq * kg * 240 ' m/s
xmtr = k * areaoil * fractoil * owpc * n% / q
'____water covered separation zone______
mtr2 = ko * 0.18 * areaoil * (1 - fractoil) * n% / q
froil = 1 - Exp(-xmtr - mtr2)
frabs = (1 - frEnt) * (1 - froil) * frinoil * removal
'____waterfall exit zone______
wkg = KGC8(v, widthFall * 100, dv, 6) ' gmol/cm2-sec
Call WEIR(widthFall, q / n%, drop / 100, frwf, wkl, dl, 0)
wkl = wkl / 0.18 'g m/cm-s
wko = 1 / (1 / wkl + 1 / Hl / 55555 / wkg)
AREA = widthFall * drop / 100 'm2
Mtr = wko * 0.18 * AREA / q * n%
frwf = 1 - Exp(-Mtr)
'__print results_______
Call setx1(kg / 0.00409, kl * 0.18, ko * 0.18, frEnt, doil, 0, froil, frwf,
55555 * Hl * (1 - frinoil), frinoil)
fair = frEnt + (1 - frEnt) * froil + (1 - frEnt) * (1 - froil) * (1 -
frinoil) * frwf
ASGN2(182) = fair
ASGN2(183) = frabs
Call sumrates8(nt%, fair, 0, frabs, ta, 0)
WATER9: SOURCE CODE
23-7
le(nt%).foil = frabs
' Call sumrates8(nt%, fair, fbio, Fads, ta)
End Sub
Sub CLARIFIER(nt%)
Call sets8(nt%, n%, ci, v, q, T)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
eff = 0 '
xb = 0 'no active biomass g/l
w = asgn8(3) 'width m
fwe = asgn8(7) 'fraction overflow of cir.
d = asgn8(4) 'Depth m
wl = asgn8(6) 'waterfall dist cm
eff = asgn8(5) 'effectiveness,generally set to zero
cwell = asgn8(8) 'center well present
controlf = asgn8(15)
ventperarea = asgn8(16) 'm3/s per m2 area
cover% = asgn8(17)
totalin nt%, sumSolids, sumBiomass, sumOil, sumDiss 'g/s
If cover% = 1 Then
v = 200 * ventperarea 'cm/s 0.5 meter depth
If showprint = 1 Then
ppprnt "The circular clarifier has a cover."
ppprnt "A headspace distance of 50 cm is assumed under the cover."
ppprnt "The gas flow rate is " + FORMP(v) + " cm/s based on 200 * the
vent rate per area " + FORMP(ventperarea) + " m3/s per m2"
End If
End If
If L(nt%).typ = 25 Then
nc% = 5 'primary municipal model
Else
nc% = 11 'secondary municipal: industrial clarifiers considered more like
secondary municipal
End If
If n% = 0 Or Hl = 0 Or q <= 0 Then e = 0: fair = 0: fra = 1: Exit Sub 'no unit
present
solids = (L(nt%).solids + L(nt%).sludge) / q 'suspended solids g/s / m3/s =
mg/lASGN2(107) = w 'width m
SAREA = 3.14159 * w * w / 4 'm2
le(nt%).AREA = SAREA
ventrate = SAREA * ventperarea
'________emissions from clarifier surface_____
ksl = 10 ^ (0.67 * aa.low - 2.61) + 0.099 'g/m3 per g/Kg biomass
fadso = solids * ksl / (solids * ksl + 1000)
'fadso = solids * 1000 / (solids * 1000 + ksl)
restime = SAREA * d / q * n% 's
rthrs = restime / 3600
vcl = w / 2 / restime * 100 'water vel, cm/s
If showprint = 1 Then
ppprnt "Clarifier surface_____________"
ppprnt "The residence time in the clarifier is " + Format$(rthrs,
"###.###") + " hrs."
ppprnt "The Henry's law constant of " + FORMP(Hl) + " atm-m3/mol is
multiplied by"
ppprnt " a adsorption factor of " + FORMP(1 - fadso) + "."
WATER9: SOURCE CODE
23-8
ppprnt "The adsorption corrected aqueous HL is " + Format$(Hl * (1 -
fadso) * 55555, "#.###e+00 (y/x)")
ppprnt "The gas phase mass transfer is estimated using correlation 3,
MacKay (1983)."
End If
skg = KGC8(v, w * 100, dv, 3) 'gm/c2-s
Vclc = vcl * 10 'corrects for flow distribution
If cwell > 0.01 Then flowfactor = 0.13 Else flowfactor = 0.1
skl = KLC8(Vclc, d * 100, dl, q / n% * 10000 / w * 2, 7, 0, flowfactor)
'gm/c2-s
If skl > 0 And Hl > 0 And skg > 0 Then sko = 1 / (1 / skl + 1 / (Hl * (1 -
fadso)) / 55555 / skg) Else skl = 0 'gm/c2-s
Mtr = sko * 0.18 / d * restime
sfair = 1 - Exp(-Mtr)
If cover% = 1 And ventrate = 0 Then sfair = 0
If eff > 0 Then
fads = eff * fadso ' corrects for settling losses,equil.
solids = solids * (1 - eff) ' after settling
Else
fads = fadso
End If
If showprint = 1 Then
ppprnt "Gas phase mass transfer " + FORMP(skg) + " g mol/ cm2-s."
ppprnt " The flow of water is " + FORMP(q / n% * 1000) + " cm3/s."
ppprnt " The effective flow depth in the clarifier is " + FORMP(d * 10)
+ " cm."ppprnt "Clarifier model liquid phase mass transfer " + FORMP(skl) + " g
mol/ cm2-s."
ppprnt "Overall mass transfer " + FORMP(sko) + " g mol/cm2-s."
End If
If showprint = 1 Then
ppprnt "Clarifier weir emissions_____________"
End If
cir = 3.14159 * w * fwe 'circumference, m
wkg = KGC8(v, wl, dv, 3) ' gmol/c2-sec '%%%%wl?? width of air flow cm
Call WEIR(cir, q / n%, wl / 100, fair, wkl, dl, nc%) 'reference
wkl = wkl / 0.18 'g m/cm-s
wko = 1 / (1 / wkl + 1 / (Hl * (1 - fadso)) / 55555 / wkg)
AREA = cir * wl / 100 'm2
Mtr = wko * 0.18 * AREA / q * n%
wfair = 1 - Exp(-Mtr)
'_____cover effects_______
If cover% = 1 Then
If ventrate = 0 Then
wfair = 0: sfair = 0 'mass transfer supressed
Else
'-------
airdens = 1 / 0.0224 * 273 / (273 + T) ' g-m/m3
koc = (Hl * (1 - fadso) * 55555)
air = ventrate 'emitted air m3/s
gm = airdens * air 'moles/s gas
arear = SAREA 'area m2
kor = sko * 0.18 'm/s



WATER9: SOURCE CODE
23-9
Lm = q * 1000000 / 18 / n% 'moles/s liquid
ghx = gm * koc
skor = arear / q * n% * kor * (1 + Lm / ghx) 'm3/s per m3/s
sfair = 1 - (ghx / (Lm + ghx) * Exp(-skor) + Lm / (Lm + ghx))
wkor = AREA / q * n% * wko * 0.18 * (1 + Lm / ghx) 'm3/s per m3/s
If ghx < (Lm * 0.000000000000001) Then ghx = Lm * 0.000000000000001
fres = (Exp(-skor))
wfair2 = 1 - (ghx / (Lm + ghx) * Exp(-wkor - skor) + Lm / (Lm + ghx))
If Lm > 0 And ghx > 0 Then frequil = wfair2 / (1 - Lm / (Lm + ghx)) Else
frequil = 0
wfair = (wfair2 - sfair) / (1 - sfair)
If showprint = 1 Then
ppprnt "Corrections due to cover effects________________"
ppprnt "The density of air in the vent is " + FORMP(airdens) + " g-
m/m3"
ppprnt "The emitted air is " + FORMP(air) + " m3/s"
ppprnt "Surface loss f = 1-(ghx/(Lm+ghx)*Exp(-skor)+Lm/(Lm+ghx)),
where" ppprnt " f = " + FORMP(sfair)
ppprnt " ghx is the product of gas rate mol/s and Henry's law, " +
FORMP(ghx) ppprnt " Lm is the liquid rate mol/s, " + FORMP(Lm)
ppprnt " skor is the ratio of the stripping rate to the flow rate,
" + FORMP(skor) + "."
ppprnt "Total loss f = 1 -(ghx/(Lm+ghx)*Exp(-wkor-
skor)+Lm/(Lm+ghx)), where"
ppprnt " f = " + FORMP(wfair2) + ", with the weir contribution f= "
+ FORMP(wfair)
ppprnt " wkor is the ratio of the weir stripping rate to the flow
rate, " + FORMP(wkor) + "."
ppprnt "Inlet data____________"
End If
End If
End If
wfair = wfair * (1 - fadso) * (1 - sfair) 'corrects for adsorption during
vol.Call setx1(restime / 3600, skg / 0.00409, skl * 0.18, sko * 0.18, sfair, wkg
/ 0.00409, wkl * 0.18, wko * 0.18, wfair, frequil)
fair = sfair + wfair
Call sumrates8(nt%, fair, fbio, fads, SAREA, controlf)
ASGN2(188) = fair
ASGN2(187) = fads
End Sub
Sub compress(T, air, depth, k, kl, kg, aag, ta)
'T deg C air m3/s depth ft
'kl m/s kg m/s
' aag is the area for each agit m2, ta total area m2
If air > 0 And depth > 0 Then
molesair = air * 273 / (273 + T) / 0.0224 'g mol/s
k = 1.395 'heat capacity ratio
'work, g-cal/g mol
work = 1.987 * k / (k - 1) * (273 + T) * (((33 + depth) / 33) ^ ((k - 1) / k) -
1)
WATER9: SOURCE CODE
23-10
hp = work * molesair * 4.18 / 746
' agitator model for air emissions
pow = hp: na = 1
Else: pow = 0: na = 0: aag = 0
End If
eff = 1: alpha = 0.65
kl = KAC8(0, depth * 30.48, 1200, 60, aa.dv, pow, eff, alpha, aag, na, aa.dl, k,
ta)
End Sub
Sub CONDVENT(nt%)
oilfract = asgn8(2) 'fract oil in cond.
temp = asgn8(3) 'temp of the exit vent
tempventin = asgn8(4)
rhvent = asgn8(5) ' fract. actual humidity
directwater = asgn8(6) 'l/s
indirectwater = asgn8(8) 'l/s
ynonin = asgn8(9) 'y noncond in vent
gasflow = asgn8(10) 'gmol/s
yin = asgn8(11) / 1000000 'ppmv > y in
cooltemp = asgn8(12) 'cooling water
totalMPS = gasflow + directwater * 1000 / 18
'___flow rate water in condensate___
ywaterin = rhvent
humidity8 temp, 1, ywaterout, rhowater 'saturated condensate
waterinMPS = rhvent * gasflow
If asgn8(22) = 1 Then ynonin = 1 - ywaterin
compMPS = yin * gasflow
nonmps = ynonin * gasflow
If ywaterout >= 1 Then ywaterout = 0.9999
waterinvent = waterinMPS * 18 / 1000 'l/s water in vapor
'assumes total mols=water + noncond
nonoutmps = gasflow * ynonin
If ywaterout < ywaterin Then
totaloutventmps = gasflow * ynonin / (1 - ywaterout)
wateroutventMPS = totaloutventmps - nonoutmps
wateroutvent = wateroutventMPS * 18 / 1000 'l/s water not condensed
condensate = waterinvent - wateroutvent + directwater 'L/s
Else
condensate = directwater
totaloutventmps = gasflow
wateroutventMPS = gasflow * ywaterin
wateroutvent = waterinvent
End If
If gasflow > 0 Then
fractioncondensed = (gasflow - totaloutventmps) / gasflow
Else
fractioncondensed = 0
End If
wateroutcondMPS = condensate * 1000 / 18
If condensate > 0 Then
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
vp = vp / 760 'atm
Call fractionoil(oilfract, owpc, owr, frinoil, 0)
WATER9: SOURCE CODE
23-11
k = Hl * 55555
If k = 0 Then k = 1E-20
'assume partitioning between gas, oil, and water
xwMax = aa.sol / 1000000
ygasout = compMPS / (totaloutventmps + wateroutcondMPS / k * (1 + frinoil))
xwater = ygasout / k
If xwater > xwMax Then 'above max solubility, liquid condensate adds to oil
' xwater = xwMax 'not implemented here. need a separator
ygasout = k * xwMax
End If
' all compound condensed goes to liquid
compcondMPS = compMPS - ygasout * totaloutventmps
xwater = compcondMPS / wateroutcondMPS
totalgasoutmps = totaloutventmps
If compMPS > 0 Then
frem = ygasout * totaloutventmps / compMPS
Else
frem = 0
End If
Else 'no liquid
If totalMPS > 0 Then ygasout = compMPS / totalMPS Else ygasout = 0
totalgasoutmps = totalMPS
L(nt%).wflow = 0
L(nt%).oil = 0
compcondMPS = 0
wateroutvent = waterinvent 'mol water/s
frem = 0
End If
L(nt%).TwasteF = condensate 'added water L/s
If oilfract >= 1 Then oilfract = 0.99
molsgas = totalgasoutmps * ygasout
compcondMPS = compMPS - molsgas
If totalMPS > 0 Then xincond = compcondMPS / totalMPS
If compMPS > 0 Then frem = (compMPS - molsgas) / compMPS Else frem = 0
If totalMPS <> 0 Then frconds = (totalMPS - totalgasoutmps) / totalMPS
conv = 22400 * (temp + 273) / 273 'L/mol
ventCCps = totaloutventmps * conv
le(nt%).vup = compMPS * aa.mwt
le(nt%).ventoutccs = ventCCps 'cc/s
le(nt%).nonoutmps = nonoutmps 'mol/s noncond
le(nt%).yout = ygasout
le(nt%).vout = totalgasoutmps * ygasout * aa.mwt 'g/s
le(nt%).gmout = totalgasoutmps
le(nt%).gmwvent = wateroutvent 'mol water/s
le(nt%).ventt = temp 'temp vent C
le(nt%).unittrans = le(nt%).vup + le(nt%).cin - le(nt%).vout
L(nt%).TwasteF = condensate
L(nt%).oil = condensate * 1000 * oilfract / (1 - oilfract) 'g/s oil
le(nt%).cline = wateroutcondMPS * xwater * aa.mwt
le(nt%).cout = le(nt%).cline 'g/s out unit
le(nt%).unittrans = le(nt%).cout
L(nt%).wflow = condensate 'L water/s
le(nt%).uwflow = condensate 'L water/s
If condensate > 0 Then
xincond = compcondMPS / (condensate / 18)
Else: xincond = 0
WATER9: SOURCE CODE
23-12
End If
If showprint = 1 Then
totalmpsin = waterinMPS + compMPS + nonmps
ppprnt " The fraction of the water that is condensed is " +
Format(fractioncondensed, "#.######")
ppprnt " The inlet gas compound concentration is " + FORMP(yin) + "
mol fraction"
ppprnt " The inlet vent rate is " + FORMP(gasflow) + " mol per s"
ppprnt " The exit vent rate is " + FORMP(totaloutventmps) + " mol per
s"
ppprnt " The exit condensate is " + FORMP(wateroutcondMPS) + " mol per
s"
ppprnt " The exit condensate is " + FORMP(condensate) + " L per s"
If xwater > 0 Then ppprnt " The exit condensate is " +
FORMP(wateroutcondMPS * xwater * aa.mwt) + " g compound per s"
If condensate > 0 Then ppprnt " The condensate concentration is " +
FORMP(le(nt%).cout / condensate) + " weight fract."
End If
'tmpsout, gpsout, molwaterout, molnoncout
unit_vent_out nt%, temp, totalgasoutmps, le(nt%).vout, wateroutventMPS, nonmps
If totalgasoutmps > 0 Then frnon = nonmps / totaloutventmps
' 172v 173v 4v 5 6v 7v
178v 179 180
Call setx1(le(nt%).yout, xwater, frem, oilfract, frinoil, condensate,
totaloutventmps * 22.4, frnon, fractioncondensed, 0)
findexitvent isect 'also updates flows in vent, air
Break = Break
Exit Sub
End Sub
Sub coolcalcsa8(nt%, twater(), tair(), numbElements, enthalpy())
Call sets8(nt%, n%, ci, v, q, tinL)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, tinL, nt%)
cpair = 0.25 'cal/g-degC
KH20 = 0.014 'btu/ft2-hr-degF
Mair = 29 'g/mol
ratio = 0.87 '(Nsh/Npr)^.667
dvwv = 0.1 'water vapor cm2/s
dH = 18000 'cal/mol vaporization
tp = asgn8(12) 'total bed porosity
crossdraft = asgn8(14) '1, crossdraft, else packed bed
frr = asgn8(3) 'fraction recycle
humid = asgn8(10) / 100 'Humidity inlet air (fraction sat,)
Tina = asgn8(9) 'Temp inlet air C
tlength = asgn8(5) 'length of tower m
theight = asgn8(6) ' thickness of tower packing m
tWidth = asgn8(4) ' width of tower packing m
If crossdraft = 1 Then
FLOWx = q / n% / 2 / (1 - frr) 'one side m3/s
CAREAM = tWidth * theight 'crossdraft based on side areas
Else
FLOWx = q / n% / (1 - frr) 'm3/s
CAREAM = tWidth * tlength
WATER9: SOURCE CODE
23-13
End If
Crossarea = tlength * tWidth
Qg = asgn8(11) * CAREAM 'air flow m3/s
If Qg = 0 Then Qg = 0.1
rtwater = asgn8(8) 'liquid holdup s/m
lp = rtwater * FLOWx / Crossarea
If (tp - lp) > 0 Then flood = lp / (tp - lp) Else flood = 0
aOa = asgn8(7) / 100 'm2/m3 to cm2/cm3
Carea = Crossarea * 10000 'tower cross sect, cm2
pd = 6 / aOa * (1 - tp) 'equivalent dia packing cm
ASGN2(90) = pd
pa = tp - lp: If pa < 0 Then pa = 0.01
molDensitya = 1 / 0.0224 * 273 / (Tina + 273) 'mol/m3
moldensityac = molDensitya * 1000000! 'mol/cm3
moldensityL = 1000000! / 18 'mol/m3
La = FLOWx * 1000000# / Carea 'cm3/cm2-s
Ga = Qg * molDensitya / Carea 'gmol/cm2-s
mol_water_psec = FLOWx * 55555 'mol water/s
G = Qg * molDensitya 'mol gas/s
'___water vapor____
Scgw = 0.00018 / 0.0011 / dvwv
kg = Ga * Scgw ^ -0.666 * 1.1958 * (pd * Ga * 29 / 0.00018 / (1 - pa)) ^ -0.36
'mol/cm2-s
kgw = kg * moldensityac 'water, cm/s
h = cpair * kg * Mair * ratio 'cal/cm2-sec-C
'___cooling converg____
delH = theight * 100 / numbElements 'cm
' volelement = Carea * delH * pa 'cm3
' rtair = volelement / (Qg * 1000000) 'residence sec
consta = aOa * Carea * delH 'cm2 surface/element
constb = kg * aOa * Carea * delH 'mol transferred/s-element
Tmax = Tina: If Tmax < tinL Then Tmax = tinL
wetbulb Tina, Tmin, humid
If Tmin > tinL Then Tmin = tinL
acc = 1
deltstart = Abs(tinL - Tina)
Tl = Tina
tlo = Tl
numIter = 80
recycle = asgn8(3)
If recycle < 0.1 Then numIter = 20
For ii = 1 To numIter
tspecs = tinL * (1 - recycle) + recycle * tlo
GoSub estimatetemp 'returns TL<>TinL
'converg. criteria
If Tl > tspecs Then
If acc < 1 Then acc = acc
If acc > 0 Then acc = -acc / 2
Else
If acc > 1 Then acc = acc
If acc < 0 Then acc = -acc / 2
End If
Tl = tlo + acc 'adjusts temperature at base of tower
tlo = Tl
WATER9: SOURCE CODE
23-14
Next
Call humidity8(TG, 1, ywater, rhowater)
ASGN2(279) = Y / ywater * 100
'___compound____
Call KLP(pd, La, 0.00001, kl) 'Kl, cm/s
Call Kgp8(pd, pa, Ga, kg) 'Kg, cm/s
Keq = 40.9 * 0.00554
If kl > 0 And kg > 0 Then
KoL = 1 / (1 / kl + 1 / kg / Keq) 'cm/s
Else: KoL = 0
End If
Mtr = KoL * aOa 'cm/s* cm2/cm3* f /s
ASGN2(91) = kl
ASGN2(92) = kg
ASGN2(93) = Mtr
If Carea > 0 Then Vgas = 1000000 * Qg / Carea Else Vgas = -100 'cm/s
Exit Sub
estimatetemp:
TG = Tina
hum = humid
Call humidity8(TG, hum, ywater, rhowater)
Y = ywater
mol_water_psec = FLOWx * 55555 'mol/s
G = Qg * molDensitya 'mol/s
ASGN2(291) = Tl
summass = 0
If crossdraft = 1 Then
'assumes one stage crossflow,numbelements stages down
sumtempair = 0
sumyair = 0
sumwatertrans = 0
Tl = tspecs 'cools from top down
Gair = G / numbElements
For i = 1 To numbElements
Call humidity8(Tl, 1, ye, rhowater)
dmdt = (ye - ywater) * constb 'mols/s-element
dmdtmax = (ye - ywater) * Gair
If dmdt <> 0 Then dmdt = dmdtmax * (1 - Exp(-dmdt / dmdtmax))
yout = (ywater * Gair + dmdt) / (Gair + dmdt)
mol_water_psec = mol_water_psec - dmdt
dcdtV = dmdt * dH 'cal/s vaporization
DcdtT = h * consta * (Tl - TG) 'cal/s heat transfer
DcdtTv = dmdt * 18 * cpair * (Tl - TG) 'cal/s warm vapor added
dcdtTmaX = (Tl - TG) * cpair * Gair * Mair
If DcdtT <> 0 Then DcdtT = dcdtTmaX * (1 - Exp(-DcdtT / dcdtTmaX))
twater(numbElements - i + 1) = Tl 'ASGN2(290 + i)
Tl = Tl - (dcdtV + DcdtT) / (mol_water_psec * 18) 'temp liq into elem
tairout = TG + (DcdtT + DcdtTv) / (cpair * Gair * Mair) 'temp gas
out of element
tair(numbElements - i + 1) = tairout
sumwatertrans = sumwatertrans + DcdtTv
sumtempair = sumtempair + tairout
summass = summass + dmdt
sumyair = sumyair + yout


Next i
TG = sumtempair / numbElements
Y = sumyair / numbElements
tlo = Tl
TempWaterout = tlo
TempAirout = TG
Else
For i = 1 To numbElements
Call humidity8(Tl, 1, ye, rhowater)
dmdt = (ye - Y) * constb 'mols/s-element
dmdtmax = (ye - Y) * G
If dmdt <> 0 Then dmdt = dmdtmax * (1 - Exp(-dmdt / dmdtmax))
summass = summass + dmdt
Y = (Y * G + dmdt) / (G + dmdt)
mol_water_psec = mol_water_psec + dmdt
G = G + dmdt
dcdtV = dmdt * dH 'cal/s vaporization
DcdtT = h * consta * (Tl - TG) 'cal/s heat transfer
DcdtTv = dmdt * 18 * cpair * (Tl - TG) 'cal/s warm vapor added
dcdtTmaX = (Tl - TG) * cpair * G * Mair
If DcdtT <> 0 Then DcdtT = dcdtTmaX * (1 - Exp(-DcdtT / dcdtTmaX))
twater(i) = Tl 'ASGN2(290 + i)
Tl = Tl + (dcdtV + DcdtT) / (mol_water_psec * 18) 'temp liq into elem
TG = TG + (DcdtT + DcdtTv) / (cpair * G * Mair) 'temp gas out of element
If Tl > Tmax + 5 Then Tl = Tmax + 5 ' modified 2-17-00 limit temperature at
topIf Tl < Tmin Then Tl = Tmin
If TG > Tmax Then TG = Tmax
If TG < Tmin Then TG = Tmin
tair(i) = TG 'ASGN2(280 + i)
Next i
TempWaterout = twater(1)
TempAirout = tair(numbElements) 'ASGN2(290)
End If
enthalpy(1) = summass * dH
enthalpy(2) = TempAirout
enthalpy(3) = summass
delenthalpyair = (TempAirout - Tina) * cpair * G * Mair
delenthalpywater = (tspecs - TempWaterout) * 1 * mol_water_psec * 18
enthalpy(4) = delenthalpyair
enthalpy(5) = delenthalpywater
enthalpy(6) = sumwatertrans
Return
End Sub
Sub dropi8()
Rem $DYNAMIC
ND% = 2
ReDim ss$(ND% + 1), a$(ND% + 1, 1)
ss$(1) = " 1 1 9 45 8 10 1 droplet diameter (cm) ; .1"
ss$(2) = " 2 1 10 45 8 10 2 fall distance (cm); 10"
For i = 1 To ND%: asgn8(i) = 0: Next
ASGN2(99) = 24
WATER9: SOURCE CODE
23-16
ASGN2(99) = 0
End Sub
Sub droplet8(dp, vl, d, dv, dl, h, KoL)
' kol cm/s revised 9-13-90
' estimates mass transfer coefficient from a falling droplet
vl = 5.65: R = 0.0012: vc = 0.00018: re = 1.5
'___terminal velocity calculated_____
If dp < 0.0101 Then ' constants are selected
b1 = 24: n = 1 ' depending on the droplet size.
ElseIf dp < 0.051 Then
b1 = 18.5: n = 0.6
Else
b1 = 0.44: n = 0
End If
ae = 980 'cm/s
a = 1 / (2 - n)
vl = (4 * ae * dp ^ (1 + n) / (3 * b1 * vc ^ n * R ^ (1 - n))) ^ a
T = Sqr(2 * d / ae) ' time of free fall
vav = ae * T / 2 ' average free fall velocity
If vl > vav Then vl = vav ' limits to max gravity accel.
re = dp * vl * R / vc 'Reynolds number
Sc = vc / R / dv 'Schmidt number
Gr = ae * dp ^ 3 * R ^ 2 / vc ^ 2 'Grashof number
Sho = 2 + 0.596 * (Gr * Sc) ^ 0.25
Sh = Sho + 0.347 * (re * Sqr(Sc)) ^ 0.62 'Sherwood number
If dp > 0 Then kg = Sh * dv / dp Else kg = 0 'cm/s
If T > 0 Then
kl = Sqr(dl / 4 / T / 3.141592) 'cm/s
KoL = 1 / (1 / kl + 1 / (kg * h * 40.9)) 'cm/s
Else: KoL = 0
End If
End Sub
Rem $STATIC
Sub fract28(ci, X, vmax, Kother, kair, solids, dissol, rtime, Co, fbio, fair,
fraleft, plugflow, ksl, fads, cair)
' estimates the fraction removed by the various pathways
' Kother includes surface and submerged aeration
If aa.k1 > 0 Then ks = aa.biov / aa.k1 Else ks = 1
If plugflow = 0 Then 'backmixing specified
'___solve for exit concentration___
acc = Kother + 1 / rtime
AB = acc * ks + vmax * X - ci / rtime
ad = -ks * ci / rtime
Co = (-AB + (AB ^ 2 - 4 * acc * ad) ^ 0.5) / 2 / acc
If vmax = 0 Or ks = 0 Then
kbio = 0
Else
kbio = vmax * X / (ks + Co)
End If
Kall = Kother + 1 / rtime + kbio
If Kall = 0 Then fbio = 0 Else fbio = kbio / Kall
If Kall = 0 Then fair = 0 Else fair = Kother / Kall
ElseIf plugflow Then
WATER9: SOURCE CODE
23-17
fother = 0
timestep = 0.01
c = ci
xs = solids / 1000
times = 0
Counter2 = 0
While times < rtime And c > 1E-28 And Counter2 < 1000
Counter2 = Counter2 + 1
If dissol > 0 Then
delx = X * vmax * dt 'g/m3
X = X + delx / 2 / 1000
dissol = dissol - delx
End If
ratio = 1 / (1 + ksl * (X + xs))
kp = ratio * (Kother + vmax * X / (ks + c * ratio))
If kp > 0 Then dt = timestep / kp / ratio Else kp = 0
If dt > rtime - times Then dt = rtime - times
del = Exp(-kp * dt)
If kp > 0 Then fother = fother + Kother / kp * c * ratio * (1 - del) Else
fother = fother
c = c * del + cair * dt
times = times + dt
Wend
Co = c * ratio
If ci = 0 Then
fbio = 0
fair = 0
fads = 0
Else
fbio = (ci - fother - Co) / ci
If Kother > 0 Then fair = kair / Kother * fother / ci Else fair = 0
fads = (1 - ratio) * Co / ci
End If
End If
If ci > 0 Then fraleft = Co / ci
End Sub
Sub fractionoil(oilfract, owpc, owr, frinoil, nt%)
If nt% > 0 Then
If L(nt%).wflow > 0 Then
oilfract = (L(nt%).oil + L(nt%).sludge * 0.2 + L(nt%).solids * 0.2) /
L(nt%).wflow / 1000
Else
oilfract = 0 'fract oil
End If
End If
owpc = Exp(aa.low * 2.302585): If owpc = 1 Then owpc = 100
If oilfract > 0.999 Then
oilfract = 1
owr = 99999
Else
owr = owpc * oilfract / (1 - oilfract) ' g in oil / g in water
End If
frinoil = owr / (1 + owr) ' fraction in the oil phase
End Sub
WATER9: SOURCE CODE
23-18
Sub humidity8(temp, humid, ywater, rhowater)
'input temp C, H umidity (relative)
'output ywater Y, rho water g/cm3
a = 0.00393673: bx = -0.0001841304
If humid > 1 Then humid = 1
If temp < 0 Then
temp = 0: ywater = 0.006: Exit Sub
ElseIf temp >= 100 Then
temp = 100: ywater = 1: Exit Sub
Else: tp = temp
End If
vp = humid * Exp((1 / (tp + 273.16) - a) / bx)
ywater = vp / 760
rhowater = vp * humid / (tp + 273.16) * 0.000002888 'g/cm3 273.16/ 760 *
18/22400 /100
End Sub
Sub IMPOUND8(nt%)
'surface impoundment model
Call sets8(nt%, n%, ci, v, q, T)
l1 = asgn8(3) 'length, m
d = asgn8(4) 'depth, m
If d = 0 Then Exit Sub
wid = asgn8(5) 'width, m
xw = asgn8(6) 'g biomass /l
If asgn8(7) = 1 Then plugflow = 1
AREA = wid * l1 'surface of impoundment m2
le(nt%).AREA = AREA
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
volume = AREA * d 'm3
dia = Sqr(AREA * 4 / 3.14) 'width m
If q = 0 Then ' model is the same for either
plugflow = 1 ' plug flow or disposal
restime = asgn8(8) * 365.25 / 12 * 24 'holding time, hr
Else
restime = l1 * wid * d / q * n% / 3600 'holding time, hr
End If
fd! = dia / d 'Fetch to depth ratio
kg = KGC8(v, dia * 100, dv, 2) 'gm/c2-s
kl = KLC8(v, fd!, dl, T, 6, 0, 1)
ko = 1 / (1 / kl + 1 / Hl / 55555 / kg)
kair = ko * 0.18 / d * 3600 'hr-1
Kads = 0 'not considered %%
Kother = kair + Kads
solids = 0
dissol = 0
Call fract28(ci, xw, vmax, Kother, kair, solids, dissol, restime, Co, fbio,
fair, fraleft, plugflow, 0, fads, cair)
'__returns Co, Fbio, Fair, fra.left
total = ci * volume 'g in impound
'__print results_______
Call setx8(kg, kl, ko, fair, fbio, fdiff, fads)
If restime > 0 Then le(nt%).cin = total / restime / 3600 'g/s
WATER9: SOURCE CODE
23-19
Call sumrates8(nt%, fair, fbio, 0, AREA, 0)
If restime > 0 Then le(nt%).e = total * fair / restime / 3600
ASGN2(192) = ci * (1 - fair - fbio - fads)
End Sub
Sub k25dcorr(k, m25d)
If k > 30 Then
m25d = 0.93
ElseIf k > 3 Then
m25d = 0.75
ElseIf k > 0.5 Then
m25d = 0.44
ElseIf k > 0.1 Then
m25d = 0.15
ElseIf k > 0.01 Then
m25d = 0.124
Else: m25d = 0.02
End If
End Sub
Function KAC8(L, depth, RPMS, impd, dv, pow, eff, alpha, aag, na, dl, k, ta)
' agitator model for air emissions
' AAG is the area for each agit m2, ta total area m2
' pow is power per agitator in hp, depth cm
' returns overall turbulent k in m/s
' DATA FOR AGITATORS
If alph < 0.01 Then alph = 0.83
If eff < 0.01 Then eff = 0.83
j = 3
EP = 0.67
'__input data___
v = ld(50).dissol 'wind velocity, cm/s
T = asgn8(2) 'temperature, C
ldw = Sqr(ta / 3.141592) * 2 'effective fetch, m
fld = ldw / depth * 100
a = aag * na
'__quiescent surface_______
kl = KLC8(v, fld, dl, T, 6, 0, 1) 'gm/cm2-s
kg = KGC8(v, ldw * 100, dv, 2) 'gm/c2-s
'__agitated surface________
TPOW = pow * na 'total power
If na > 0 Then 'do not calculate if no agitator is specified
rads = RPMS * 360 / 57.3 / 60
If rads = 0 Then rads = 1200 / 60 * 360 / 57.3
If impd = 0 Then impd = 60
NRW = 0.001185 * impd ^ 2 * rads / 0.000178
NFR = rads ^ 2 * impd / 32.17 / 30.5
NPR = pow * eff * 550 * 32.17 / (impd / 30.5) ^ 5 / rads ^ 3 / 62.37
If dv > 0 Then nsch = 0.000178 / 0.001185 / dv Else nsch = 0
kgre = 0.000000135 * NRW ^ 1.42 * NPR ^ 0.4 * nsch ^ 0.5 * NFR ^ -0.21 * dv *
29 / impd * 0.00409
DO2 = 0.000024 'O2 diffusivity in water
' pow correction for efficiency
klag = 0.00822 * j * pow * 1.024 ^ (T - 20) * alph * 18 * (dl / DO2) ^ 0.5 /
aag / 10.764 / 0.18
If kgre > 0 And k > 0 And klag > 0 Then
ASGN2(174) = 1 / (1 / kgre / k + 1 / klag) * 0.18 'm/s
WATER9: SOURCE CODE
23-20
Else: ASGN2(174) = 0
End If
If showprint = 1 Then
ppprnt "The rotation speed is " + FORMP(rads) + " radians per second. "
ppprnt "The rotation factor NRW is " + FORMP(NRW) + "."
ppprnt "The power number NPR is " + FORMP(NPR) + "."
ppprnt "The rotation factor NFR is " + FORMP(NFR) + "."
ppprnt "kg (agitated)is estimated as " + FORMP(kgre * 240) + " m/s."
ppprnt "kl (agitated)is estimated as " + FORMP(klag * 0.18) + " m/s."
End If
Else: a = 0 ' no agited area if no agitators are specified
ASGN2(174) = 0
End If
If k = 0 Then k = 0.000000000000001
If kl > 0 Then ASGN2(177) = 1 / (1 / kg / k + 1 / kl) * 0.18 Else ASGN2(177) = 0
klo = ASGN2(177) * (ta - a) / ta + ASGN2(174) * a / ta
KAC8 = klo
ASGN2(172) = kgre / 0.00409
ASGN2(173) = klag * 0.18
ASGN2(175) = kg / 0.00409
ASGN2(176) = kl * 0.18
ASGN2(178) = klo
End Function
Function KGC8(v, Lz, dv, kF%)
' Gmol/cc-sec revised 3 9-6-90
' AIR COEF V elocity cm/s, Lz ength cm, DV dif coef
' multiply by 240 for m/s
If v < 0 Then KGC8 = 1E-20: Exit Function
If dv = 0 Then KGC8 = 1E-20: Exit Function
If kF% = 1 Then 'Jfactor
'l is characteristic length (diameter)
If Lz = 0 Then Exit Function
kg = 0.00000403 * v ^ 0.804 / Lz ^ 0.196 * dv ^ 0.667
ElseIf kF% = 2 Then 'Mackay and mat.
If Lz = 0 Then Exit Function
kg = 0.00482 * (v / 100) ^ 0.78 / (Lz / 100) ^ 0.11 * (0.000181 / 0.0012 /
dv) ^ -0.67 * 0.00409
ElseIf kF% = 3 Then 'Mackay and Yeun. 1983
nsch = 0.15 / dv
Us = (6.1 + 0.0063 * v) ^ 0.5 * v / 100 'cm/s
kg = 100 / 24300 * (0.001 + 0.000462 * Us / nsch ^ 0.67)
If showprint = 1 Then
ppprnt "The Schmidt number is " + FORMP(nsch) + "."
ppprnt "The friction velocity is " + FORMP(Us) + " m/s"
End If
ElseIf kF% = 4 Then 'Thibodeaux and Parker
If Lz = 0 Then Exit Function
kg = 0.00000099 * v * dv / Lz ^ 0.1
ElseIf kF% = 5 Then 'Jfactor modif for avg.veloc (=1/2 wind velocity)
'l is characteristic length (diameter),v avg vel
If Lz = 0 Then Exit Function
kg = 0.0000023 * v ^ 0.804 / Lz ^ 0.196 * dv ^ 0.667
ElseIf kF% = 6 Then 'turbulent surface
'assumes similar to aerated basin,see example 4.43
kg = 0.05 * (dv / 0.088) ^ 0.667 / 240


End If
If kg = 0 Then kg = 0.001 * 100 / 24300 'default, 4.1e-6
If showprint = 1 Then
ppprnt "kg is estimated as " + FORMP(kg * 240) + " m/s."
End If
KGC8 = kg
End Function
Sub Kgp8(pd, pa, G, kg)
'calculates the gas phase mass transfer coef for a packed bed.
'used for trickling filters : important for low Henry's law compounds
'Kg cm/s ; G gmol/s-cm2
If G = 0 Then kg = 0: Exit Sub
Scg = 0.00018 / 0.0011 / 0.088 '%% aa.dv
kg = G * Scg ^ -0.666 * 1.1958 * (pd * G * 29 / 0.00018 / (1 - pa)) ^ -0.36
kg = kg * 24000
End Sub
Function KLC8(v, Hl, dl, T, fx%, slope, rough)
' WIND EQUATIONS FOR LIQUID COEF (G-M/CM2/SEC)
' V elocity wind cm/s DL dif coef cm2/s
' T emperature C HL depth cm
' slope rise/run rough cm, 7=fraction flow
' __specifications
DBL = 0.00001
DOW = 0.000025
EP = 0.7
If dl = 0 Or v < 0 Or Hl <= 0 Then KLC = 1E-20: Exit Function
nsch = 0.01 / dl 'Schmidt number
Us = (6.1 + 0.0063 * v) ^ 0.5 * v / 100 'friction velocity cm/s
If fx% = 1 Then
REF = 48 * v ^ 1.25 / (Exp(56.6 / v ^ 0.25))
kl = (17.6 * REF ^ 0.195 - 7.72) * 0.00001 * (dl / DBL) ^ EP
ElseIf fx% = 2 Then 'owens edwards
kl = 3.314 * (1.024 ^ (T - 20)) * v ^ 0.67 / ((Hl / 3) ^ 0.85) * DOW * (dl /
DOW) ^ EP
ElseIf fx% = 3 Then 'owens,et.all 1964 (trench model)
'v=water velocity cm/s
kl = 0.000783 * (v) ^ 0.67 / (Hl) ^ 0.85 * (dl / DOW) ^ EP
ElseIf fx% = 4 Then 'churchill
kl = 0.00078 * (v) ^ 0.69 / (Hl) ^ 0.853 * (dl / DOW) ^ EP
ElseIf fx% = 5 Then 'Mackay and Yeun (1983)
If Us > 30 Then
kl = (0.000001 + 0.0000341 * Us * nsch ^ (-0.5)) * 100 / 18
Else
kl = (0.000001 + 0.0144 * (Us / 100) ^ 2.2 * nsch ^ (-0.5)) * 100 / 18
End If
ElseIf fx% = 6 Then 'springer
fd = Hl 'F/D IS EQUAL TO HL IN CASE 6
If v < 325 Then 'cm/s
If showprint = 1 Then ppprnt "The velocity is " + FORMP(v) + " cm/s."
kl = 0.00000278 * (dl / 0.0000085) ^ 0.66666
ElseIf fd > 51.11 Then
If showprint = 1 Then ppprnt "The fetch to depth ratio is " + FORMP(fd) +
"."
WATER9: SOURCE CODE
23-22
kl = 0.0000002611 * v * v / 10000 * (dl / 0.0000085) ^ 0.66666
ElseIf fd > 13.999 Then
If showprint = 1 Then ppprnt "The fetch to depth ratio is " + FORMP(fd) +
"."
kl = (0.000000002605 * fd + 0.0000001277) * v * v / 10000 * (dl /
0.0000085) ^ 0.66666
Else 'no correlation, use Mackay and Yeun (1983)
If showprint = 1 Then
ppprnt "Springer correlation does not apply, use Mackay and Yeun (1983)."
ppprnt "The friction velocity is " + FORMP(Us) + "cm/s."
ppprnt "The Schmidt number is " + FORMP(nsch) + "."
End If
If Us > 30 Then aaw = 0.0000341 * Us Else aaw = 0.0144 * (Us / 100) ^ 2.2
kl = (0.000001 + aaw * nsch ^ (-0.5))
End If
kl = kl * 5.5555
If showprint = 1 Then ppprnt "kl is estimated as " + FORMP(kl * 0.18) + "
m/s."
ElseIf fx% = 7 Then 'churchill 2 ,V=L Clarifier ,T = Q/Radius,rough=fraction
flow
kl = 0.000342 * (Hl) ^ -1.52 * rough ^ -2.52 * (T) ^ 0.67 * (dl / DOW) ^ EP
ElseIf fx% = 8 Then 'Parkhurst Pomeroy
'Kl=0.000267 (1+0.17F^2)(SU)^(3/8); F=u/g/d; u m/s;d m;s slope
'Jour.San.Eng. ASCE 98, SA1,8701,pp 101-124, 1972
'v is liquid flow rate cm/s
If slope = 0 Then slope = 0.002 'assumes slope of 0.002
If Hl = 0 Then
kl = 0
Else
kl = 0.000267 * (1 + 0.17 * v ^ 2 / 981 / Hl) * (v * 0.01 * slope) ^ (3 / 8)
/ 0.18
End If
kl = kl * (dl / DOW) ^ EP 'corrects for diffusion coef.
ElseIf fx% = 9 Then 'Turbulent flow model
' v is liquid flow rate cm/s
If Hl = 0 Then kl = 0: KLC8 = kl: Exit Function
rvr = 100 'rho/viscosity s/cm2
rough = 0 'smooth
findfrictvel v / 2, Hl, rvr, Us, rough
kl = 3 / 4 * dl * rvr * Us / 100 / (dl * rvr) ^ 0.5
ElseIf fx% = 10 Then 'Mackay modified for surface shear
rvr = 100 'rho/viscosity s/cm2
rough = 0 'smooth
findfrictvel v / 2, Hl, rvr, Us, rough
Usa = Us / 0.0345
If Usa > 30 Then
kl = (0.000001 + 0.0000341 * Usa * nsch ^ (-0.5)) * 100 / 18
Else
kl = (0.000001 + 0.0144 * (Usa / 100) ^ 2.2 * nsch ^ (-0.5)) * 100 / 18
End If
ElseIf fx% = 11 Then 'Corsi-Koziel
'Kl=0.000267 (1+0.17F^2)(SU)^(3/8); F=u/g/d; u m/s;d m;s slope
'v is liquid flow rate cm/s
If slope = 0 Then slope = 0.002 'assumes slope of 0.002
If Hl = 0 Then
kl = 0
Else
WATER9: SOURCE CODE
23-23
kl = 7.603 * nsch ^ (-1.047) * (Hl / 100) ^ 0.667 * (v * 0.01 * slope) ^
0.338 / 0.18
End If
kl = kl * (dl / DOW) ^ EP 'corrects for diffusion coef.
End If
KLC8 = kl
End Function
Sub KLP(pd, L, dl, kl)
' pd diameter of packing,cm; L is flow rate liquid, g mol/cm2-s
' dl is diffusion coefficient, cm2/s
' liquid mass transfer coefficient in a packed bed.
' used for trickling filter calculations
If L < 0 Then L = -L
If dl = 0 Then dl = 0.00001
NSc = 0.01 / 1 / dl
kl = dl / pd * 25.1 * NSc ^ 0.5 * (pd / 0.01 * L) ^ 0.45 'cm/s
End Sub
Sub OILFILM(nt%)
Call sets8(nt%, n%, ci, v, q, T)
Percent = asgn8(3) ' percent oil in total waste
If n% = 0 Or Percent = 0 Then Exit Sub
Lng = asgn8(4) ' length, m
wid = asgn8(5) ' width, m
d = asgn8(6) ' depth, m
densoil = asgn8(8): If densoil = 0 Then densoil = 0.7
mwtoil = asgn8(10)
AREA = wid * Lng
le(nt%).AREA = AREA
dia = Sqr(AREA * 4 / 3.14) 'width m
ASGN2(107) = dia
If asgn8(9) = 0 And q > 0 Then ' flow through specified
restime = AREA * d / q * n% 'sec
If showprint = 1 Then ppprnt "Flow through is assumed for the
calculations. "
Elserestime = asgn8(9) * 365.25 / 12 * 24 * 3600 'sec
If restime > 0 Then q = AREA * d * densoil / restime * n% 'Mg/sec
If showprint = 1 Then
ppprnt "Exposed waste for specified time is assumed for the
calculations. "
ppprnt "The equivalent flow rate is " + FORMP(q * 1000000#) + " g/s, "
+ FORMP(q / 0.0004536 * 3600 * 24) + " lb/day"
End If
End If
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
kg = KGC8(v, dia * 100, dv, 2) 'gm/c2-s
Keq = vp * 0.0012 * mwtoil / densoil / 28.8 / 760
k = Keq * kg * 240 ' m/s
xmtr = k / d * restime / Percent * 100
fair = 1 - Exp(-xmtr)
total = wid * Lng * d * ci 'g in impound
'__print results_______
Call setx8(kg, 0, 0, fair, fbio, fdiff, fads)
Call setx1(d * Percent, restime / 3600 / 24, 0, 0, 0, 0, 0, 0, 0, 0)
WATER9: SOURCE CODE
23-24
If restime > 0 Then le(nt%).cin = total / restime 'g/s
Call sumrates8(nt%, fair, 0, 0, ta, 0)
If restime > 0 Then le(nt%).e = total * fair / restime
ASGN2(192) = ci * (1 - fair - fbio - fads)
If showprint = 1 Then
ppprnt "The model assumes gas phase resistance only. " + FORMP(kg) +
"gm/c2-s"
ppprnt "The initial loss is " + FORMP(k / d / Percent * 100 * total) + "
g/s"
ppprnt "The model assumes first order decay during residence for emission
loss."
End If
End Sub
Sub PRETREATMENT(nt%)
'unit% 1 bar screen 2 grit chamber 3 open trench
Call sets8(nt%, n%, ci, v, q, T)
rough = 0.007 * 2.54 * 12 ' sewer wall roughness, ft to (cm)
If n% = 0 Or q = 0 Then e = 0: fair = 0: Exit Sub 'unit not present
'__input parameters__
Length = asgn8(4) ' length, m
AREA = asgn8(5) * Length * n% 'm2
le(nt%).AREA = AREA
ASGN2(107) = Sqr(AREA * 4 / 3.14) 'width m
d = asgn8(6) 'm
air = asgn8(7) 'm3/s air per unit
' If L(nt%).wflow > 0 Then oilfract = L(nt%).oil / L(nt%).wflow / 1000
oilsurf = asgn8(9) 'fraction of surface covered with oil
mwtoil = asgn8(10)
densoil = asgn8(11): If densoil = 0 Then densoil = 0.7
ventrate = asgn8(16) * asgn8(4) * asgn8(5) 'm3/s per m2 area * m2 area
cover% = asgn8(17)
MUNICIPAL% = asgn8(14)
controlf = asgn8(15)
If ld(50).waste2 = 2 Then 'WATER8 compatability mode
oilfract = asgn8(8) / 100
Call fractionoil(oilfract, owpc, owr, frinoil, 0)
Else
Call fractionoil(oilfract, owpc, owr, frinoil, n%)
End If
rtime = AREA * d / q 's
'__calculate mass transfer coef.__
vl = Length * 100 / rtime 'cm/s
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
kg = KGC8(v, Length * 100, dv, 3)
updateventin nt%, 0, ci, q, AREA * d, air, vair
If trenchunit = 1 Then 'trench
If MUNICIPAL% = 1 Then
'---Parkhurst and Pomeroy, 1972 g-m/cm2-s
kl = KLC8(vl, d * 100, dl, T, 8, 0, rough)
Else
kl = KLC8(vl, d * 100, dl, T, 3, 0, rough)
End If
WATER9: SOURCE CODE
23-25
Else
kl = asgn8(3) / 0.18 '__specified value
End If
If kl = 0 Then kl = 1E-20: kg = 1E-20 'covered surface
Hcorr = Hl * (1 - frinoil)
If Val(Hcorr) < 0.000000000000001 Then Hcorr = 0.00000000000001
If showprint = 1 Then ppprnt "The oil corrected HL is " + Format$(Hl * (1 -
frinoil) * 55555, "#.###e+00 (y/x)")
'_____water surface
ko = 1 / (1 / kl + 1 / Hcorr / 55555 / kg) * 0.18 'm/s
Mtr = ko * AREA * (1 - oilsurf) / q
'_____oil surface
Keq = vp * 0.0012 * mwtoil / densoil / 28.8 / 760
k = Keq * kg * 240 ' m/s
mtr3 = k * AREA * oilsurf * owpc / q
'_____air bubbles
airdens = 1 / 0.0224 * 273 / (273 + T) 'g-m/m3
mtr2 = air * n% * Hcorr * airdens / q
'_____vent under cover
Mtrvent = ventrate * n% * Hcorr * airdens / q
'_____summation
If cover% = 1 Then
'Mtr = 0: mtr3 = 0
If Mtr = 0 Then Mtr = 1E-20
fair2 = 1 - Exp(-mtr2) ' air bubbles
fair3 = 0
fairmaxVent = 1 - Exp(-Mtrvent) 'equilib vent under cover
fairmaxMtr = 1 - Exp(-(Mtr + mtr3)) 'mass transfer oil and water
If fairmaxVent > fairmaxMtr Then ftot = fairmaxMtr Else ftot = fairmaxVent
fair1 = (1 - fair2) * ftot * Mtr / (Mtr + mtr3)
fair3 = (1 - fair2) * ftot * mtr3 / (Mtr + mtr3)
fair = fair2 + fair1 + fair3
Else
controlf = 0
sumMtr = (Mtr + mtr2 + mtr3)
fair = 1 - Exp(-sumMtr)
fair1 = fair * Mtr / sumMtr
fair2 = fair * mtr2 / sumMtr
fair3 = fair * mtr3 / sumMtr
End If
'___print results
Call setx1(Hcorr * 55555, frinoil, fair3, 0, 0, 0, 0, 0, fair1, fair2)
Call setx8(kg, kl, ko / 0.18, fair, fbio, fdiff, fads)
Call sumrates8(nt%, fair, fbio, fads, AREA, controlf)
endptreat:
End Sub
Sub SEPARATOR(nt%)
'calculates separator
'_______ input variables___________
Call sets8(nt%, n%, ci, windvelocity, q, T)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
airdens = 1 / 0.0224 * 273 / (273 + T) ' g-m/m3
ta = asgn8(3) / 10000 'Total gas exit area m2
WATER9: SOURCE CODE
23-26
G = asgn8(4) * 0.00508 'gas velocity (m/s)
oilfract = asgn8(12) / 100 'fract oil
removal = asgn8(11) 'fraction oil recovery in unit
headspace1 = asgn8(9)
Lm = q * 1000000 / 18 / n% 'moles/s liquid
If q = 0 Then Exit Sub
Call fractionoil(oilfract, owpc, owr, frinoil, 0)
koc = (Hl * (1 - frinoil) * 55555)
'--mass transfer
ventperarea = asgn8(8) 'm2
widths = asgn8(6)
Length = asgn8(5)
depth = asgn8(7)
areasurface = Length * widths 'm2
le(nt%).AREA = areasurface
'If areasurface = 0 Then areasurface = 1 'minimum
If G > 0 Then
air = G * ta 'emitted air m3/s
ElseIf areasurface > 0 Then
air = ventperarea * areasurface
Else
air = ventperarea * 0.1 'minimum amount of air flow
End If
gm = airdens * air 'moles/s gas
'____equilibrium________
frapart = air * Hl * (1 - frinoil) * airdens / q * n%
faire = air * Hl * (1 - frinoil) * airdens / (q / n% + air * Hl * (1 -
frinoil) * airdens)
fair = faire 'assumes equilibrium with exit
'-------
restime = areasurface * depth / q * n% 's
If restime = 0 Then
frequil = 1
Else
vcl = Length / restime * 100 'water vel, cm/s
If widths * headspace1 = 0 Then
v = 10 'cm/s
Else
v = air / (headspace1 * widths) * 10000 'cm/s
End If
skg = KGC8(v, headspace1, dv, 3) 'gm/c2-s
skl = KLC8(vcl, depth * 100, dl, T, 4, 0, 1) 'gm/c2-s
If skl < 1E-20 Then skl = 1E-20
sko = 1 / (1 / skl + 1 / (Hl * (1 - fadso)) / 55555 / skg) 'gm/c2-s
Mtr = sko * 0.18 / depth * restime
sfair = 1 - Exp(-Mtr)
sko = 0.001 'm/s
koc = (Hl * (1 - frinoil) * 55555)
arear = areasurface 'area m2
kor = sko * 0.18 'm/s






Lm = q * 1000000 / 18 / n% 'moles/s liquid
ghx = gm * koc
skor = arear / q * n% * kor * (1 + Lm / ghx) 'm3/s per m3/s
fair = 1 - (ghx / (Lm + ghx) * Exp(-skor) + Lm / (Lm + ghx))
If Lm / ghx > 1E+20 Then
frequil = fair * Lm / ghx
Else
frequil = fair / (1 - Lm / (Lm + ghx))
End If
End If
'-------
oilrecovery = removal * oilfract * q / n%
If oilfract > 0 Then oilconc = frinoil * ci / oilfract Else oilconc = 0
fads = removal * frinoil * (1 - fair)
airconcg = ci * q / n% * fair / air 'g/m3
If aa.mwt = 0 Then aa.mwt = 100
airconc = airconcg / aa.mwt / airdens * 1000000
'____print the results______
Call setx1(T, 0, air, airconc, koc, frinoil, oilrecovery, oilconc, fads, fair)
ASGN2(182) = frequil
ASGN2(183) = skg / 0.00409
ASGN2(184) = skl * 0.18
ASGN2(185) = sko * 0.18
Call sumrates8(nt%, fair, 0, fads, ta, 0)
End Sub
Sub SetSteamK()
'finds K for steam stripper conditions
psiA! = asgn8(9)
sx$ = " .088.178 .51 1.27 2.22 4.74 11.5 14.7 20.8"
sy$ = " 0 10 27 43 54 71 93 100 110"
setvar 5, psiA!, tcolumn!, sx$, sy$
asgn8(3) = tcolumn! - 5 ' inlet feed 5 c less
HenryLaw tcolumn!, aa.n, steamstripK, h
If tcolumn! < 105 And tcolumn! > 95 Then steamstripK = aa.kss
pH = asgn8(19)
ionization steamstripK, pH
asgn8(13) = steamstripK
End Sub
Sub SteamLGcalc(gl!, lg!, n!, texit, lout!, gout!)
Ttop = asgn8(3)
Tfeedin = asgn8(8)
concin = asgn8(12)
gl! = asgn8(4) 'steam rate (mol gas/ mol liquid)
n! = asgn8(6)
psig! = asgn8(7)
psiA! = asgn8(9)
If gl! = 0 Then gl! = 0.1
lg! = 1 / gl!
l1! = 1 'assume 1 g/s feed
G! = gl! * l1!
lout! = l1!
WATER9: SOURCE CODE
23-28
cpl! = 1#
delhfeed! = (Ttop - Tfeedin) * l1! * cpl! 'cal/s to preheater
'STEAM PROPERTIES
sx$ = " 0 1.69 27.1 42.9 63 103 159 232 1000"
sy$ = " 1150 1160 1171 1176 1183 1190 1196 1201 1206"
setvar 5, psig!, delhsteamin!, sx$, sy$
'PRESSURE PROPERTIES
sx$ = " .088.178 .51 1.27 2.22 4.74 11.5 14.7 20.8"
sy$ = " 0 10 27 43 54 71 93 100 110"
setvar 5, psiA!, coltemp!, sx$, sy$
asgn8(14) = coltemp!
'frmStripper!label25.Caption = Str$(coltemp!)
'PRESSURE PROPERTIES
sx$ = " .088.178 .51 1.27 2.22 4.74 11.5 14.7 20.8"
sy$ = " 1076 1084 1097 1090 1118 1130 1145 1150 1157"
setvar 5, psiA!, ColBTU!, sx$, sy$
delhfeed1! = (coltemp! - Ttop) * 1.8 'Btu/lb heater to tray
' delhsteam! = (delhsteamin! - 1150) * g!'Btu/lb steam add heat
delhsteam! = (delhsteamin! - ColBTU!) * G! 'Btu/lb steam add heat
delhvap! = (ColBTU! - 180.07 + (100 - coltemp!) * 1.8) 'BTU/g steam to liq
gcol! = G! + delhsteam! / delhvap!
gout! = gcol! - delhfeed1! / delhvap!
lcol! = l1! + delhfeed1! / delhvap!
lout! = lcol! - delhsteam! / delhvap!
lg! = lcol! / gcol!
texit = coltemp! - delhfeed! / lout! / cpl!
End Sub
Public Sub stripper(G, Lm, k, bio, kl, n, rt, YI, Yo, xi, Xo, fair, fbio, cmtr)
On Error GoTo errorstrippers
' countercurrent gas-liquid flow with mass transfer and first-order reaction
'(G, l, k, b, kL, n, rt, yi, yo, xi, xo, fair, fbio)
' rewritten 8-6-99 yin can be different from 0
' backcalc from gas side
' check for overcalc
' Rt residence time in system sec
' N number of calculation elements
' Kl mass trans. /s
' Bio biorate constant /s
' G L moles/s
' k partition coefficient (y/x)
'fair <0 then refers to gas phase
' cmtr g/s transferred to liquid valid if bio=0
'
'_____clean up incorrect inputs
If YI < 0 Then YI = 0
fair = 0
cmtr = 0
If xi < 0 Then xi = 0
If Lm <= 0 Or G = 0 Then Yo = YI: Xo = xi: Exit Sub
maxmtr = (YI - xi * k) * G 'mols
maxyo = ki * k: If maxyo < YI Then maxyo = YI
If maxmtr > 0 Then maxxo = xi + maxmtr / Lm Else maxxo = xi
WATER9: SOURCE CODE
23-29
If k < 1E-20 Then k = 1E-20
n = Int(n)
If n > 99 Then n = 99 'limit to 100
dt = rt / n '__ estimate time step for each increment
EE = Lm / G
Ef = EE + EE * bio * dt
c1 = 1 / (kl * dt + 1 + dt * bio)
c2 = dt * kl * c1 / k
Dim aa1(100), aa2(100), bb1(100), bb2(100)
aa1(0) = xi
aa2(0) = 0
aa1(1) = xi * c1
aa2(1) = c2
bb1(1) = 0
bb2(1) = 1
'y(i)=bb1(i)+bb2(i)*yo
'x(i)=aa1(i)+aa2(i)*yo
biosuma = 0
biosumb = 0
For i = 1 To n
bb1(i + 1) = Ef * aa1(i) - EE * aa1(i - 1) + bb1(i)
bb2(i + 1) = Ef * aa2(i) - EE * aa2(i - 1) + bb2(i)
aa1(i + 1) = c1 * aa1(i) + c2 * bb1(i + 1)
aa2(i + 1) = c1 * aa2(i) + c2 * bb2(i + 1)
biosuma = biosuma + aa1(i)
biosumb = biosumb + aa2(i)
Next
Yo = (YI - bb1(n + 1)) / bb2(n + 1)
biosum = dt * bio * (biosuma + biosumb * Yo)
Xo = aa1(n) + aa2(n) * Yo
checkerrors:
totali = YI * G + xi * Lm
totalo = Yo * G + Xo * Lm
If totali > 0 Then fbio = Lm * biosum / totali Else fbio = 0 'mol/s bio per mol/s
transratio = kl * Lm / G / k
If Xo < 1E-20 Then Xo = 1E-20
'__check for errors in calculations__
error2 = 0
If Xo > maxxo Then
Xo = maxxo: error2 = 1
End If
yotest = (xi * Lm + YI * G - Xo * Lm) / G
If yotest <= maxyo And bio = 0 Then 'base yo on xo
Yo = yotest
ElseIf error2 = 1 Then 'base xo on yo
Yo = maxyo
Xo = (xi * Lm + YI * G - Yo * G) / Lm
End If
If Yo < 0 Then Yo = 0
If Xo < 0 Then Xo = 0
If YI > Yo Then
cmtr = aa.mwt * ((YI - Yo) * G) 'g/s %%%%
WATER9: SOURCE CODE
23-30
fair = 0
ElseIf xi > Xo Then
fair = (xi - Xo) / xi
Else
fair = 0
End If
Break = Break
Exit Sub
'_____________________
errorstrippers:
'equilibrium one stage if defective module
totali = YI * G + xi * Lm
If totali <= 0 Then totali = 0
If xi < 0 Then xi = 0
Xo = totali / (Lm + k * G)
Yo = Xo * k
Resume checkerrors
End Sub
Sub stripper8(G, L, k, b, kl, n, rt, YI, Yo, xi, Xo, fair, fbio)
'used for trickling filter model
' b = 0
'(G, l, k, b, kL, n, rt, yi, yo, xi, xo, fair, fbio)
' rewritten 9-17-93 assumes yin =0
' Rt residence time sec
' N number of calculation elements
' Kl mass trans. /s
' B biorate constant /s
' G L moles/s
' initial concentration in air=0
' k partition coefficient (y/x)
If k < 1E-20 Then k = 1E-20
n = Int(n)
dt = rt / n '__ estimate time step for each increment
a = 1 + b * dt + kl * dt
b2 = -kl / k * dt
c = k * G / (k * G + kl * L * dt)
d = c * kl * L / G * dt
'YI = 0 if by definition
xxl = 1: yg = YI 'initialize the values
For i = 1 To n
yg = c * yg + d * xxl
xxl = a * xxl + b2 * yg
Next
Xo = xi / xxl
Yo = yg * xi / xxl
total = xi * L + YI * G
If total = 0 Then total = 1
fair = (Yo - YI) * G / total
fbio = ((xi - Xo) * L - (Yo - YI) * G) / total
'__check for errors in calculations__
'If fair > 1! Then fair = 1
WATER9: SOURCE CODE
23-31
'If fair < 0 Then fair = 0
If fbio < 0.00001 Then fbio = 0
If L = 0 Then Yo = YI
End Sub
Sub TANK(nt%)
'$DYNAMIC
' 5-3-94
Call sets8(nt%, n%, ci, Vel, q, temp)
dt = asgn8(10) * 1.8 'deg F
Fp = asgn8(7) 'paint factor
storetime = asgn8(6) 'days
wid = asgn8(8) 'diameter m
h = asgn8(9) 'vapor space m
AREA = (wid) ^ 2 * 3.141592 / 4 'm2
le(nt%).AREA = AREA
depth = asgn8(11) 'm specified as tank height
DENS = asgn8(4) 'g/cc
mwt = asgn8(5)
vol = depth * AREA ' m3
areareal = asgn8(3)
oilfract = asgn8(12) / 100
If oilfract = 0 Then 'oil fraction not specified, estimated from inflow
Call fractionoil(oilfract, owpc, owr, frinoil, nt%)
Else
Call fractionoil(oilfract, owpc, owr, frinoil, 0)
End If
fair = 0: fairs = 0: fairb = 0: fairw = 0
If storetime = 0 And q = 0 Then
Exit Sub
ElseIf storetime = 0 Then
storetime = vol * n% / q / 3600 / 24
End If
If storetime = 0 Then Exit Sub
qan = n% * ci * vol * DENS / storetime * 365 'g yearly throughput
nturn = 365 / storetime
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
cmwt = aa.mwt: If cmwt = 0 Then cmwt = 80
If nturn <= 36 Then Kn = 1 Else Kn = (180 + nturn) / 6 / nturn
If mwt = 18 Or oilfract = 0 Then 'aqueous
p = 14.7 * Hl * ci / cmwt * (1 - frinoil) 'psia
pmax = vp / 760 * 14.7
If p > pmax Then p = pmax
aqueous$ = " Aqueous matrix"
If showprint = 1 Then ppprnt "The oil corrected aqueous HL is " +
Format$(Hl * (1 - frinoil) * 55555, "#.###e+00 (y/x)")
xw = 0
Else
coil = ci * frinoil / oilfract
xw = frinoil * coil / cmwt / (coil / cmwt + (1000000! - coil) / mwt)
p = xw * vp * 14.7 / 760 'psia
aqueous$ = " Oily matrix"
End If
If p < 0 Then p = 0
WATER9: SOURCE CODE
23-32
conc = p / 14.7 / 1000000! 'ppmv
If showprint = 1 Then
ppprnt "The concentration in the tank inlet is " + Format$(ci,
"#.###e+00 ppmw")
ppprnt "The flowrate of liquid is " + Format$(q, "#.###e+00") + "
M3/s"
ppprnt "liquid flowrate (from tank holding) is " + Format$(vol /
storetime / 24 / 3600, "#.###e+00") + " M3/s"
ppprnt "The total loading of the compound is " + Format$(qan,
"#.##e+00") + " g/yr."
End If
If areareal > 0 Then
If depth = 0 Then depth = 0.3
fd = wid / depth
kg = KGC8(Vel, wid * 100, dv, 2)
kl = KLC8(Vel, fd, dl, temp, 6, 0, 1)
ko = 1 / (1 / kl + 1 / Hl / 55555 / kg)
restime = storetime * 24 * 3600
Mtr = ko * 0.18 / depth * restime
fairs = 1 - Exp(-Mtr)
fair = fairs
Call setx8(kg, kl, ko, fairs, 0, 0, 0)
Else
'_____working losses
If depth = 0 Then 'assumes constant level tank, fixed roof
fairw = 0
Else
G = q * 1000 / 3.75 * 3600 * 24 'gal/day
v = vol * 1000 / 3.785 'gal
lw = 0.0000000109 * aa.mwt * p * v * Kn 'Mg/turnover
If ci > 0 Then fairw = 1000000! * lw / (ci * vol * DENS) Else fairw = 0
fairw = fairw / (1 + fairw)
If showprint = 1 Then
ppprnt "The working volume is " + Format$(vol, "#.###e+00 m3;") +
Format$(v, "#.###e+00 gal")
ppprnt "The mass lost per turnover is " + Format$(lw, "#.###e+00") +
" Mg/turnover"
ppprnt "The vapor pressure of the compound in solution is " +
Format$(p, "#.######") + " atm."
End If
End If
'_____breathing losses
dia = wid * 3.28 'ft
h = h * 3.28 'ft
If dia > 30 Then c = 1 Else c = 0.0771 * dia - 0.0013 * dia ^ 2 - 0.1334
Lb2 = 0.0000102 * aa.mwt * (p / (14.7 - p)) ^ 0.68 * dia ^ 1.73
Lb = n% * Lb2 * h ^ 0.51 * dt ^ 0.5 * Fp * c 'Mg/year
If qan > 0 Then fairb = 1000000 * Lb / qan Else fairb = 0
fairb = fairb / (1 + fairb)
fair = fairb + fairw * (1 - fairb)
Call setx8(0, 0, 0, 0, 0, 0, 0)
If showprint = 1 Then
ppprnt "MWT = " + Format$(aa.mwt, "###.#") + " dia= " +
Format$(dia, "####.# ft.")
ppprnt "Breathing: Lb = 0.0000102 * MWT * (p / (14.7 - p)) ^ 0.68 *
dia ^ 1.73 "
ppprnt "mass emissions= Lb * h ^ 0.51 * dt ^ 0.5 * Fp * c Mg/yr"
WATER9: SOURCE CODE
23-33
ppprnt "c = " + Format$(c, "#.###") + " h= " + Format$(h, "####.#
ft.")
ppprnt "dt = " + Format$(dt, "##.## ") + "deg.F Fp= " +
Format$(Fp, "##.###")
ppprnt "mass emissions= " + Format$(Lb, "##.##E+00 ") + " Mg/yr"
End If
End If
If showprint = 1 Then
ppprnt "The temperature in the tank is " + Format$(temp, "###.#") + " deg.C"
ppprnt "The type of liquid is " + aqueous$
ppprnt "The concentration in the liquid waste is " + Format$(ci, "#.###e+00
g/m3")
ppprnt "The fraction in the oil is " + Format$(frinoil, "#.#######")
ppprnt "The vapor pressure (p) is " + Format$(p, "#.###e+00 psia; ") + "(" +
Format$(vp, "#.###e+00 mmHg") + ")"
ppprnt "The fraction of the compound in oil phase is " + Format$(frinoil,
"#.#####") + "."
ppprnt "The residence time in the tank is " + Format$(storetime, "###.###") + "
days."
End If
Call setx1(storetime, Kn, fairw, fairb, fairs, concg, frinoil, 0, 0, 0)
Call sumrates8(nt%, fair, 0, 0, AREA, 0)
ASGN2(192) = ci * (1 - fair - fbio - fads)
End Sub
Sub Tcorr(xk1, vmax, dl, dv, vp, Hl, T, nt%)
If showprint = 1 Then
ppprnt "Properties of " + bTrim$(aa.cs) + " at " + Format$(T, "###.#") +
" deg.C"
End If
vmax = aa.biov * (1.046 ^ (T - 25))
d = ((T + 273.16) / 298.16)
dv = aa.dv * d ^ 1.75 'cm2/s
dl = aa.dl * d 'cm2/s
If aa.c + T = 0 Then c = 0 Else c = Exp(-aa.b * 2.30258 * (1 / (aa.c + T) - 1
/ (aa.c + 25)))
HL1 = aa.Hl
If nt% < 0 Then
pH = -nt%
ElseIf nt% > 0 Then
pH = L(nt%).pH
End If
If pH <> 0 And HL1 > 0 Then
If Mid$(aa.code, 6, 1) = "k" Or Mid$(aa.code, 6, 1) = "b" Then
Ka = aa.rh
If pH = 0 Or Ka = 0 Then
Else
oH3 = 10 ^ (-pH)
comp2 = Ka / oH3
If Mid$(aa.code, 6, 1) = "k" Then
HL1 = HL1 / (1 + comp2)
ElseIf Mid$(aa.code, 6, 1) = "b" Then
HL1 = HL1 * comp2 / (1 + comp2)
End If
WATER9: SOURCE CODE
23-34
If showprint = 1 Then
ppprnt " pH = " + FORMP(pH)
ppprnt " Ka = " + FORMP(Ka)
ppprnt " The pH adjusted active fraction is " + FORMP(HL1 / aa.Hl)
End If
End If
End If
End If
Hl = HL1 * c 'atm-m3/mol
vp = aa.vp * c 'torr
xk1 = aa.k1 * (1.046 ^ (T - 25)) 'L/g-hr
If Hl < 1E-25 Then Hl = 1E-25
If showprint = 1 Then
ppprnt " hl= " + FORMP(Hl) + " atm-m3/mol vp= " + FORMP(vp) +
" mmHg"
ppprnt " k1= " + FORMP(k1) + " L/g-hr dl= " + FORMP(dl) +
" cm2/s dv= " + FORMP(dv) + " cm2/s"
End If
If dv < 0.001 Then dv = 0.001
End Sub
Function difl(mwt, den)
If den = 0 Then den = 1
If mwt = 0 Then mwt = 90 '%%%
Vcm = mwt / den
diff = 0.0001518 * Vcm ^ -0.6
difl = Trunb(diff, 9)
End Function
Function difv(mwt!, den!, n%)
If mwt! = 0 Then mwt! = 90 'default value
If den! = 0 Then den! = 1 'default value
If n% = 0 Then 'not used
diff = 0.0067 * (300) ^ 1.5 * (0.034 + 1 / mwt!) ^ 0.5 * mwt! ^ -0.17 / ((mwt!
/ den / 2.5) ^ 0.33 + 1.81) ^ 2
ElseIf n% = 1 Then 'not used
diff = 0.0067 * (300) ^ 1.5 * (0.034 + 1 / mwt!) ^ 0.5 * mwt! ^ -1.7 * ((mwt! /
den / 2.5) ^ 0.33 + 1.81) ^ 2
ElseIf n% = 2 Then
mwtcor = (1 - 0.000015 * mwt! ^ 2): If mwtcor < 0.4 Then mwtcor = 0.4 'outside
data correlation
diff = 0.00229 * (25 + 273.16) ^ 1.5 * (0.034 + 1 / mwt!) ^ 0.5 * mwtcor *
((mwt! / den / 2.5) ^ 0.333 + 1.8) ^ -2
End If
difv = Trunb(diff, 9)
End Function
Sub TRENCHS(flowwater, depth, wid, dl, kl)
' m/s
' model for flow in junction box dl cm2/s
' flowwwater cm3/s depth cm wid cm Kl m/s
h = depth / 2.54 / 12 'depth feet
KKoRatio = (dl / 0.000021) ^ 0.83
If depth = 0 Or wid = 0 Then
WATER9: SOURCE CODE
23-35
VelFlow = 0
kal = 0
Else
VelFlow = flowwater / depth / wid / 12 / 2.54 'ft/s
If VelFlow < 0 Then VelFlow = 0
kal = 21.6 * VelFlow ^ 0.67 / h ^ 0.85 * KKoRatio 'reareation constant
(ft/day)
End If
Cfm = 12 * 2.54 / 100 'conversion feet to meters
Cds = 24! * 3600 'conversion days to seconds
kl = kal * Cfm / Cds 'm/s
If showprint = 1 Then
ppprnt "Trench model for mass transfer from a surface."
ppprnt " The effective depth of water flow (h) is " + FORMP(h) + " ft."
ppprnt " The water flow rate is " + FORMP(flowwater) + " cm3/s."
ppprnt " The velocity of the flow (v)is " + FORMP(VelFlow) + " ft/s."
ppprnt " The width of the unit is " + FORMP(wid) + " cm."
ppprnt " The oxygen diffusion coefficient (ratio) adjustment factor is " +
FORMP(KKoRatio) + "."
ppprnt " reareation constant(ft/day) = 21.6 * v ^ 0.67 / h ^ 0.85 * Ratio "
ppprnt " The liquid phase mass transfer coefficient from surface is " +
FORMP(kl) + " m/s."
End If
End Sub
Sub TRFILTER(nt%)
Call sets8(nt%, n%, ci, v, q, temp)
If q = 0 Or n% = 0 Then Exit Sub
loading = ci * q 'g/s inlet
q = q / n% 'corrects for number of units
'v Wind velocity (cm/s)
'q inlet flow rate waste (m3/s)
'temp Wastewater temperature (C)
'ci inlet conc (mg/l)
'n% Trickling filter number units
dp = asgn8(11) 'Droplet diameter of spray (cm)
d = asgn8(10) 'Droplet fall (cm)
flb = asgn8(12) 'Spray fractional loss,BENZENE (10 cm)
If aa.k1 > 0 Then ks = aa.biov / aa.k1 Else ks = 1
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
w = asgn8(14) ' Trickling filter width (m)
df = asgn8(7) ' depth (m)
vg = asgn8(8) ' Gas flow (m3/s)
kotf = asgn8(9) ' Mass transfer coef bed (/sec)
rrtf = asgn8(15) ' Recycle fraction in filter feed
lfvf = asgn8(4) ' Liquid hold-up (volume fraction)
Bi = asgn8(13) ' biomass in trickling filter (g/l)
dpak = asgn8(3) 'packing diameter cm
tp = asgn8(5) ' bed porosity
If dpak = 0 Then dpak = 1 '%%
Nelemen = df * 100 / dpak 'filter elements
If Nelemen > 25 Then Nelemen = 25
If tp = 0 Then tp = 0.5
qt = q / (1 - rrtf) ' corrects for recycle and units




















VelFlow = 0
kal = 0
Else
VelFlow = flowwater / depth / wid / 12 / 2.54 'ft/s
If VelFlow < 0 Then VelFlow = 0
kal = 21.6 * VelFlow ^ 0.67 / h ^ 0.85 * KKoRatio 'reareation constant
(ft/day)
End If
Cfm = 12 * 2.54 / 100 'conversion feet to meters
Cds = 24! * 3600 'conversion days to seconds
kl = kal * Cfm / Cds 'm/s
If showprint = 1 Then
ppprnt "Trench model for mass transfer from a surface."
ppprnt " The effective depth of water flow (h) is " + FORMP(h) + " ft."
ppprnt " The water flow rate is " + FORMP(flowwater) + " cm3/s."
ppprnt " The velocity of the flow (v)is " + FORMP(VelFlow) + " ft/s."
ppprnt " The width of the unit is " + FORMP(wid) + " cm."
ppprnt " The oxygen diffusion coefficient (ratio) adjustment factor is " +
FORMP(KKoRatio) + "."
ppprnt " reareation constant(ft/day) = 21.6 * v ^ 0.67 / h ^ 0.85 * Ratio "
ppprnt " The liquid phase mass transfer coefficient from surface is " +
FORMP(kl) + " m/s."
End If
End Sub
Sub TRFILTER(nt%)
Call sets8(nt%, n%, ci, v, q, temp)
If q = 0 Or n% = 0 Then Exit Sub
loading = ci * q 'g/s inlet
q = q / n% 'corrects for number of units
'v Wind velocity (cm/s)
'q inlet flow rate waste (m3/s)
'temp Wastewater temperature (C)
'ci inlet conc (mg/l)
'n% Trickling filter number units
dp = asgn8(11) 'Droplet diameter of spray (cm)
d = asgn8(10) 'Droplet fall (cm)
flb = asgn8(12) 'Spray fractional loss,BENZENE (10 cm)
If aa.k1 > 0 Then ks = aa.biov / aa.k1 Else ks = 1
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
w = asgn8(14) ' Trickling filter width (m)
df = asgn8(7) ' depth (m)
vg = asgn8(8) ' Gas flow (m3/s)
kotf = asgn8(9) ' Mass transfer coef bed (/sec)
rrtf = asgn8(15) ' Recycle fraction in filter feed
lfvf = asgn8(4) ' Liquid hold-up (volume fraction)
Bi = asgn8(13) ' biomass in trickling filter (g/l)
dpak = asgn8(3) 'packing diameter cm
tp = asgn8(5) ' bed porosity
If dpak = 0 Then dpak = 1 '%%
Nelemen = df * 100 / dpak 'filter elements
If Nelemen > 25 Then Nelemen = 25
If tp = 0 Then tp = 0.5
qt = q / (1 - rrtf) ' corrects for recycle and units
WATER9: SOURCE CODE
23-36
AREA = w ^ 2 / 4 * 3.141592 'm2
le(nt%).AREA = AREA
updateventin nt%, 2, YI, 1, 1, vg, vair
If showprint = 1 Then
ppprnt "mol fraction compound in inlet air " + FORMP(YI)
End If
volumegas = 0.0224 * (temp + 273) / 273
G = vg / volumegas ' moles gas per sec
l2 = qt / 18 * 1000000 ' moles liquid per sec
k = Hl * 55555!
T = AREA * df * lfvf / qt
ASGN2(107) = w 'width m
vol = df * AREA ' volume m3
arf = 6 / dpak * (1 - tp) 'area per volume (cm2/cc)
GTF = vg * 1000000 ' gasflow (cm3/s)
If showprint = 1 Then
ppprnt "area per volume in filter media " + FORMP(arf * 2.54 * 12) +
" ft2/ft3." ppprnt "superficial gas flow " + FORMP$(vg / AREA * 100) + " cm/s ("
+ FORMP$(vg / AREA * 100 / 2.54 / 12 * 60) + " ft/m)"
ppprnt "Inlet wastewater to unit " + FORMP(q * 1000) + "
l/s." ppprnt "Internal recycle wastewater to spray " + FORMP((qt - q) *
1000) + " l/s."
ppprnt "Total wastewater to spray___________ " + FORMP(qt * 1000) + "
l/s."
End If
'________spray losses____________________
If flb > 0 Then
F = 1 - Exp(Log(1 - flb) * d / 10 * Hl / 0.0055)
Else
Call droplet8(dp, vl, d, dv, dl, Hl, KoL!)
'Ko cm/s per x in liq.
tf = d / vl 'fall time s
ard = 6 / dp 'area per vol droplet cm-1
F = 1 - Exp(-KoL! * ard * tf)
End If
'_____estimate mass transfer coefficient______________
l1 = l2 / AREA / 10000 * 18 'g/cm2-s
g1 = G / AREA / 10000 'gmol/cm2-s
Call KLP(dpak, l1, dl, kl) 'Kl, cm/s
Call Kgp8(dpak, tp - lfvf, g1, kg) 'Kg, cm/s
Keq = 40.9 * Hl
kol8 = 1 / (1 / kl + 1 / kg / Keq)
If kotf = 0 Then kotf = kol8 * arf / lfvf
If showprint = 1 Then
ppprnt "mass transfer coefficient from surface and flow " +
FORMP(kol8 * arf / lfvf) + " /s."
ppprnt "mass transfer coefficient used in calculations " +
FORMP(kotf) + " /s."
End If
'_____equilibrium air and Max biorate________zero order________
qP = qt - q 'recycle flow m3/s
Eac = vg * k * 18 / 24400
Rb = vmax * Bi * vol / 3600 'g/s
WATER9: SOURCE CODE
23-37
If ci > ks And (G * k / l2) < 1 Then
' co = (ci * (q - (Eac + Edc) * (1 - f)) - Rb) / ((Eac + Edc) * f)
A11 = q + F * q - (q / qt) * (1 - F)
b11 = q + F * qP - Eac * (1 - F) * qP / qt
Co = (ci * A11 - Rb) / b11
Cip = (1 - rrtf) * ci + rrtf * Co
Cipp = Cip * (1 - F)
If Co > 0.5 * ks Then 'check exit concentration
fair = (Eac * Cipp + F * qt * Cip) / ci / q
fbio = Rb / ci / q: fbio3 = fbio
GoTo trfilt1
End If
End If
'______numerical model___________first order______________
xi = ci / aa.mwt / 55555
bw = k1 * Bi / lfvf / 3600
Call stripper8(G, l2, k, bw, kotf, Nelemen, T, YI, Yo, xi, Xo, fair3, fbio3)
fair = fair3: fbio = fbio3
R = rrtf / (1 - rrtf)
ftot = F + (1 - F) * (fair + fbio) 'one pass
If ftot > 0 Then bioratio = (1 - F) * fbio / ftot Else bioratio = 0
sumlost = ftot / (1 + ftot * R) * (1 + R)
fbio = bioratio * sumlost: fair = sumlost - fbio
Call sumrates8(nt%, fair, fbio, fads, AREA, 0)
Call setx1(F, fair3, fbio3, fair, fbio, 0, 0, 0, 0, 0)
If vg > 0 Then cexit = fair * loading / vg 'g/s per m3/s
If (F + fair3) > 0 Then ctop = cexit * fair3 * (1 - F) / (F + fair3 * (1 - F))
area_downwind = 1 * w
gas_downwind = v / 100 * area_downwind + vg 'm3/s
If showprint = 1 Then
exitconc = ci * (1 - sumlost)
loadingwo = exitconc * q
bioremoval = fbio * loading
loadingair = YI * G * aa.mwt
loadingao = fair * loading
ppprnt "____material balance__________________"
ppprnt " inlet water (g/s) " + FORMP(loading)
ppprnt " inlet air (g/s) " + FORMP(loadingair)
ppprnt " bioremoval (g/s) " + FORMP(bioremoval)
ppprnt " outlet water (g/s) " + FORMP(loadingwo) + " " + FORMP(exitconc)
+ " g/m3."
ppprnt " outlet air (g/s) " + FORMP(loadingao) + " " + FORMP(Yo *
1000000) + " ppmv."
ppprnt " SUM material balance (g/s) =" + FORMP(loading + loadingair -
bioremoval - loadingwo - loadingao)
ppprnt "____exit concentrations in gas________"
ppprnt " mol fraction compound in filter bed " + FORMP(Yo)
ppprnt " exit gas concentration over bed " + FORMP(ctop) + " g/m3."
ppprnt " avg. undiluted gas concentration over distributor " + FORMP(cexit)
+ " g/m3."
If gas_downwind > 0 Then
cdown = fair * loading / gas_downwind
arear = fair * loading / AREA 'g/m2-s
WATER9: SOURCE CODE
23-38
sigma w, 0, 4, arear, v / 100, dilution
ppprnt " wind speed over the bed " + FORMP(v / 100) + " m/s."
ppprnt " gaussian dispersion with neutral stability assumed."
ppprnt " diluted gas concentration directly downwind " +
FORMP(dilution) + " g/m3."
ppprnt " directly downwind 0 degrees wind offset " +
FORMP(dilution * 1000) + " ug/L."
ppprnt " dispersion with mixing into a 1 m high sector assumed."
ppprnt " diluted gas concentration directly downwind " +
FORMP(cdown) + " g/m3."
ppprnt " directly downwind 0 degrees wind offset " +
FORMP(cdown * 1000) + " ug/L."
End If
'sigma w * Cos(0.0174531 * 30), 0, 4, arear, v / 100, dilution
End If
trfilt1:
End Sub
Sub WATERFALL(nt%)
Call sets8(nt%, n%, ci, v, q, T)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
i = nt%
xb = 0 'no active biomass g/l
widthFall = asgn8(4) 'width m
drop = asgn8(5) 'waterfall dist cm
opensurf = asgn8(7)
le(nt%).AREA = widthFall * drop / 100 'vertical drop
'______no unit present
If n% = 0 Or widthFall = 0 Or q = 0 Then e = 0: fair = 0: fra = 1: Exit Sub
ASGN2(107) = widthFall 'width m
wkg = KGC8(v, widthFall * 100, dv, 3) ' gmol/c2-sec
Call WEIR(widthFall, q / n%, drop / 100, fair, wkl, dl, 0)
wkl = wkl / 0.18 'g m/cm-s
Break = Break
w = le(i).cin / q
If wkl = 0 Or Hl = 0 Or wkg = 0 Then e = 0: fair = 0: fra = 1: Exit Sub
wko = 1 / (1 / wkl + 1 / Hl / 55555 / wkg)
AREA = widthFall * drop / 100 'm2
Mtr = wko * 0.18 * AREA / q * n%
wfair = 1 - Exp(-Mtr)
wfair = wfair '%%%%corrects for adsorption during vol.
maxtransfer = wfair * q * w
If opensurf = 0 Then 'covered unit
If L(i).Uvent > 0 Then
yEntrance i, YI
Else
YI = le(i).yline
End If
flowwater = q 'cc/s
flowair = Abs(L(i).Uvent) 'cm3/s at saturation
koc = Hl * 55555
WATER9: SOURCE CODE
23-39
' uses a fractional equilibrium model, where fraction =.5
frae = Fairemis(flowwater, flowair, koc, T, w, wo, YI, Yo, gmair, 0.5)
'check if mass transfer is greater than maximum transfer in open air
transfercompound = (wo - w) * q 'g/s to waterflow
If showprint = 1 Then
ppprnt " Covered waterfall. UNIT: " + Str$(isect)
ppprnt " Fractional approach to equilibrium " + FORMP$(frasat)
ppprnt " Mass transfer fraction in open air " + Format(wfair,
"#.#######")
End If
'transfer to gas
If transfercompound < 0 Then
If transfercompound < -Abs(maxtransfer) Then 'limit transfer to maximum
transfercompound = -Abs(maxtransfer)
If showprint = 1 Then ppprnt " Transfer limited to maximum mass
transfer"
End If
'transfer from gas
ElseIf transfercompound > 0 Then
If YI > 0 Then frfromgas = (YI - Yo) / YI Else frfromgas = 0
If frfromgas > wfair Then 'limit transfer to maximum fraction
frfromgas = wfair
If showprint = 1 Then ppprnt " Transfer from gas limited to maximum
fraction from mass transfer"
End If
transfercompound = frfromgas * YI * gmair * aa.mwt
End If
'transfer from gas
le(i).unittrans = transfercompound
le(i).feunit = 0
L(i).vent = 0
If L(i).Uvent > 0 Then
le(i).yline = Yo
Else
If le(i).gmvent < 0 Then le(i).yin = Yo
If le(i).gmup < 0 Then le(i).yup = Yo
End If
le(i).cline = le(i).cin + transfercompound '9-16-99 le(i).cin * (1 - fra)
If showprint = 1 Then
ppprnt " Mass transfer is based upon closed headspace system."
ppprnt " Fractional approach to equilibrium " + FORMP$(frasat)
End If
Else
If showprint = 1 Then
ppprnt " Mass transfer is based upon loss from waterfall to air."
End If
If le(i).gmvent < 0 Then le(i).yin = 0
le(i).cline = le(i).cin * (1 - wfair)
If le(i).gmline > 0 Then
le(i).vline = 0
le(i).yline = 0
End If
le(i).unittrans = 0
le(i).feunit = wfair
WATER9: SOURCE CODE
23-40
Call setx1(AREA, wkg / 0.00409, wkl * 0.18, wko * 0.18, wfair, 0, 0, 0,
0, 0)
Call sumrates8(nt%, wfair, fbio, fadso, AREA, 0)
End If
ASGN2(188) = wfair
ASGN2(187) = fadso
End Sub
Sub WEIR(cir, q, drop, fair, kl, dl, n%)
'cir m, q m3/s, drop m ,Kl m/s
If q < 0 Then q = 0
If cir = 0 Or drop = 0 Or q = 0 Then
R = 1
ElseIf n% = 5 Or n% = 20 Then
model$ = "Pincince 11/7/89 primary model"
R = Exp(0.042 * drop ^ 0.872 * (q * 3600 / cir) ^ 0.509 * (dl / 0.000025) ^
0.67)
ElseIf n% = 11 Then
model$ = "Pincince 11/7/89 secondary model"
R = Exp(0.077 * drop ^ 0.623 * (q * 3600 / cir) ^ 0.66 * (dl / 0.000025) ^
0.67)
ElseIf n% = 0 Then
model$ = "Nakasone model with a pool depth .5 m "
R = Exp(0.0785 * drop ^ 1.31 * (q * 3600 / cir) ^ 0.428 * 0.3 ^ 0.31 * (dl /
0.000024) ^ 0.67)
End If
If R = 0 Then fair = 1 Else fair = 1 - 1 / R
If fair > 0 Then
kl = fair * q / cir / drop
Else: kl = 1E-20
End If
If showprint = 1 Then
ppprnt "Weir mass transfer is estimated from the " + model$
ppprnt " The water drop was " + FORMP(drop) + " m."
ppprnt " The water flow rate was " + FORMP(q) + " m3/s."
ppprnt "The unadjusted fraction lost with no gas resistance is " +
FORMP(fair) + "."
ppprnt "The mass transfer coefficient from the water drop is " + FORMP(kl) +
" m/s."
End If
End Sub
Sub COOLINGTOWER(nt%)
numbElements = 100
crossdraft = asgn8(14)
If crossdraft = 1 Then numbElements = 5
Dim twater(100), tair(100), enthalpy(10)
Call coolcalcsa8(nt%, twater(), tair(), numbElements, enthalpy())
showprints = showprint
showprint = 0
Call sets8(nt%, n%, ci, v, q, templ)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, templ, nt%)
tempg = asgn8(9)
k = 1
frr = asgn8(3) 'fraction recycle <1
WATER9: SOURCE CODE
23-41
Widt = asgn8(4) 'm
leng = asgn8(5) 'm
ht = asgn8(6) 'm
aOa = asgn8(7) / 100 'cm2/cm3
mold = 1 / 0.0224 / (tempg + 273.16) * 273.16 'mols/m3
If crossdraft = 1 Then
FLOWx = q / n% / 2 / (1 - frr) 'one side m3/s
CAREAM = leng * ht 'crossdraft based on side areas
Else
FLOWx = q / n% / (1 - frr) 'm3/s
CAREAM = Widt * leng
End If
gv = asgn8(11) * CAREAM 'gas m3/s
Crossarea = leng * Widt
CAREAM = Widt * leng 'm2
le(nt%).AREA = CAREAM
If q = 0 Then R = 0 Else R = k * (gv * mold) / (FLOWx * 1000000 / 18)
'----------
blowdown = asgn8(13) 'fraction blowdown for closed loop operation
If blowdown > 0 Then
q2 = asgn8d(3) / blowdown - asgn8d(3) 'assumes q/n specified
=blowdown rate
asgn8(20 * nt% + 18) = q2
asgn8(20 * nt% + 19) = nt%
End If
' coolstrip f, kla
'------------------
G = gv * mold 'mol/s
mol_water_psec = FLOWx * 55555 'mol/s including recycle
Carea = CAREAM * 10000 'cm2
delH = ht / numbElements * 100 'cm in element
constb = aOa * Carea * delH
kl = ASGN2(91) * (dl / 0.0000098) ^ 0.67 'cm/s
kg = ASGN2(92) * (dv / 0.088) ^ 0.67 'cm/s
If kl = 0 Or kg = 0 Then F = 0: Exit Sub
Y = 0 'inlet air
x0 = 0.0001
xlL = x0
If crossdraft = 1 Then
yin = Y
For i = numbElements To 1 Step -1
temp = twater(i)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
KoL = 1 / (1 / kl + 1 / kg / 40.9 / Hl) 'cm/s
k = Hl * 55555
dmdt = KoL / 100 / 0.18 * (xlL - yin / k) * constb 'moles vapor per sec in
element
dmdtmax = (xlL * k - yin) * G
dmdt = dmdtmax * (1 - Exp(-dmdt / dmdtmax))
yout = yin + dmdt / G
xlL = xlL - dmdt / mol_water_psec
Next i
FremovalOnepass = (x0 - xlL) / x0
fonepass = xlL / x0
F = 1 - (fonepass * (1 - frr) / (1 - fonepass * frr))
Break = Break
Else
WATER9: SOURCE CODE
23-42
For i = 1 To numbElements '10
temp = twater(i)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
KoL = 1 / (1 / kl + 1 / kg / 40.9 / Hl) 'cm/s
k = Hl * 55555
dmdt = KoL / 100 / 0.18 * (xlL - Y / k) * constb 'moles vapor per sec in
element
dmdtmax = (xlL * k - Y) * G
dmdt = dmdtmax * (1 - Exp(-dmdt / dmdtmax))
Y = Y + dmdt / G
xlL = xlL + dmdt / mol_water_psec
Next i
fremoval = (xlL - x0) / xlL
fonepass = x0 / xlL
F = 1 - (fonepass * (1 - frr) / (1 - fonepass * frr))
End If
showprint = showprints
If showprint = 1 Then
ppprnt "__Temperatures in the cooling tower at various heights"
ppprnt " height temperature air temperature water"
ppprnt " (m) (C) (C)"
If crossdraft = 1 Then stepsize = -1 Else stepsize = -5
For ik = numbElements To 1 Step stepsize
datasets$ = String(80, 32)
Mid$(datasets$, 5) = FORMP(delH / 100 * ik)
Mid$(datasets$, 20) = Format(tair(ik), "###.##")
Mid$(datasets$, 42) = Format(twater(ik), "###.##")
ppprnt datasets$
Next
ppprnt "__Overall mass and heat flows____________________"
ppprnt "Mass flow rate of air (gmol/s) " + FORMP(G)
ppprnt "Mass flow rate of water (gmol/s) " + FORMP(mol_water_psec)
ppprnt "Moles water evaporation (gmol/s) " + FORMP(enthalpy(3))
ppprnt "Percent of water evaporated per pass " + Format(enthalpy(3) /
mol_water_psec, "##.### %")
ppprnt "Heat of evaporation from water (cal/s) " + FORMP(enthalpy(1))
ppprnt "Heat gain in air (cal/s) " + FORMP(enthalpy(4))
ppprnt "Heat gain water transfer to air (cal/s) " + FORMP(enthalpy(6))
ppprnt "Heat loss from from water (cal/s) " + FORMP(enthalpy(5))
ppprnt "heat balance " + Format((-enthalpy(4) +
enthalpy(5) - enthalpy(1)) / enthalpy(5), "###.## %")
ppprnt " "
End If
kla = KoL * aOa '/s
'------------------
FR = 1 - F 'fract remain
Call setx1(kl / 100, kg / 100, KoL / 100, kla, ASGN2(291), enthalpy(2),
ASGN2(279), F, 0, 0)
Call sumrates8(nt%, F, 0, 0, CAREAM, 0)
End Sub
Public Sub clarifierRec(nt%)
Call sets8(nt%, n%, ci, v, q, T)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nt%)
WATER9: SOURCE CODE
23-43
xb = 0 'no active biomass g/l
depth = asgn8(3) 'flow depth cm
d = asgn8(4) 'Depth m
eff = asgn8(5) 'effectiveness solids removal
wl = asgn8(6) 'waterfall dist cm
ww = asgn8(7) 'width overflow m
Lng = asgn8(8) 'length m
w = asgn8(9) 'width m
totalin nt%, sumSolids, sumBiomass, sumOil, sumDiss 'g/s
solids = L(nt%).sludge 'suspended solids mg/l
' fremsusp(nt%) = eff %%%%
controlf = asgn8(15)
ventperarea = asgn8(16) 'm3/s per m2 area
cover% = asgn8(17)
If cover% = 1 Then
v = ventperarea * 200 'cm/s 0.5 meter depth
If showprint = 1 Then
ppprnt "The retangular clarifier has a cover."
ppprnt "A headspace distance of 50 cm is assumed under the cover."
ppprnt "The gas flow rate is " + FORMP(v) + " cm/s based on 200 * the
vent rate per area " + FORMP(ventperarea) + " m3/s per m2"
Call printprop2(k1, vmax, dl, dv, vp, Hl, T)
End If
End If
If n% = 0 Or q = 0 Then e = 0: fair = 0: fra = 1: Exit Sub 'no unit present
ASGN2(107) = w 'width m
If cover% = 1 Then ASGN2(107) = 0.33
SAREA = Lng * w 'm2
ventrate = SAREA * ventperarea
le(nt%).AREA = SAREA
'________emissions from clarifier surface_____
ksl = 10 ^ (0.67 * aa.low - 2.61) + 0.099 'g/m3 per g/Kg biomass
fadso = solids * ksl / (solids * ksl + 1000) 'fraction sorbed
flowwater = q * 1000000 / n%
Call TRENCHS(flowwater, depth, w * 100, dl, kl)
' model for flow in junction box dl cm2/s
' flowwwater cm3/s depth cm wid cm Kl m/s
restime = SAREA * d / q * n% 's
vcl = w / 2 / restime * 100 'water vel, cm/s
skg = KGC8(v, Lng * 100, dv, 3) 'open gm/c2-s
If kl = 0 Then
MsgBox "kl = 0, a default of 1e-20 is assumed"
kl = 1E-20
End If
skl = kl / 0.18 'gm/c2-s
sko = 1 / (1 / skl + 1 / (Hl * (1 - fadso)) / 55555 / skg) 'gm/c2-s
Mtr = sko * 0.18 / d * restime
If showprint = 1 Then
ppprnt "The residence time in the clarifier is " + Format$(restime /
3600, "###.###") + " hrs."
ppprnt "The Henry's law constant of " + FORMP(Hl) + " atm-m3/mol is
multiplied by"
ppprnt " a adsorption factor of " + FORMP(1 - fadso) + "."
ppprnt "The gas phase mass transfer is estimated using correlation 3,
MacKay (1983)."
ppprnt "Gas phase mass transfer " + FORMP(skg) + " g mol/ cm2-s."



















WATER9: SOURCE CODE
23-47
If molSteamrate > 0 Then noncondc = noncond / molSteamrate Else noncondc =
0
Call setx1(gl!, ntheo!, 100, 0, concout, molSteamrate, 100, noncondc,
le(isect).yvent * 1000000, frem!) '2-11 asgn2(172-
End Sub
Public Sub packedair(nt%, mpswater, cwater, cgas, Gmps)
Call sets8(nt%, n%, ci, v, q, temp)
If q = 0 Or n% = 0 Then Exit Sub
q = q / n% 'corrects for number of units
'Lmps liquid (m3/s)
'Gmps gas inlet (m3/s)
'q inlet flow rate waste (m3/s)
'temp Wastewater temperature (C)
'ci,cwater inlet conc (mg/l in)
'n% packed air modules, number units
If mpswater > 0 Then q = mpswater
If cgas > 0 Then YI = cgas Else YI = le(nt%).yup
airdens = 1 / 0.0224 * 273 / (273 + temp) ' g-m/m3
If Gmps = 0 Then Gmps = le(nt%).gmup / airdens
If cwater > 0 Then ci = cwater 'overwrites actual
dp = asgn8(11) 'Droplet diameter of spray (cm)
d = asgn8(10) 'Droplet fall (cm)
flb = asgn8(12) 'Spray fractional loss,BENZENE (10 cm)
If aa.k1 > 0 Then ks = aa.biov / aa.k1 Else ks = 1
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
df = asgn8(7) ' depth (m)
kotf = asgn8(9) ' Mass transfer coef bed (/sec)
dia = asgn8(14) ' packed bed diameter (m)
rrtf = asgn8(15) ' internal recycle ratio
lfvf = asgn8(4) ' Liquid hold-up (volume fraction)
Bi = asgn8(13) ' biomass in packed bed (g/l)
dpak = asgn8(3) 'packing diameter cm
tp = asgn8(5) ' bed porosity
If rrtf > 0.99 Then rrtf = 0.99
If dpak = 0 Then dpak = 1 '%%
Nelemen = 100 'df * 100 / dpak 'filter elements
If Nelemen > 75 Then Nelemen = 75
If tp = 0 Then tp = 0.5
qt = q / (1 - rrtf) ' corrects for recycle and units
vg = Gmps ' Gas flow (m3/s)
If vg = 0 Then Exit Sub
AREA = dia ^ 2 / 4 * 3.141592 'm2
sgv = vg / AREA 'm3/s per m2
le(nt%).AREA = AREA
If aa.mwt = 0 Then aa.mwt = 100
xi = ci / aa.mwt / 55555
xiinit = xi
G = Gmps * airdens ' moles gas per sec
lm1 = q / 18 * 1000000
l2 = qt / 18 * 1000000 ' moles liquid per sec
moleratio = G / l2
volratio = Gmps / qt
k = Hl * 55555!
T = AREA * df * lfvf / qt 'holdup time sec
WATER9: SOURCE CODE
23-48
ASGN2(107) = Sqr(w * Length) 'width m
vol = df * AREA ' volume m3
arf = 6 / dpak * (1 - tp) 'area per volume (cm2/cc)
GTF = vg * 1000000 ' gasflow (cm3/s)
'________spray losses____________________
If flb > 0 Then
F = 1 - Exp(Log(1 - flb) * d / 10 * Hl / 0.0055)
Else
Call droplet8(dp, vl, d, dv, dl, Hl, KoL!)
'Ko cm/s per x in liq.
If vl > 0 And dp > 0 Then
tf = d / vl 'fall time s
ard = 6 / dp 'area per vol droplet cm-1
Else:
tf = 0
ard = 0
End If
F = 1 - Exp(-KoL! * ard * tf)
End If
'_____estimate mass transfer coefficient______________
If AREA > 0 Then
l1 = l2 / AREA / 10000 * 18 'g/cm2-s
g1 = G / AREA / 10000 'gmol/cm2-s
Else
Exit Sub
End If
Call KLP(dpak, l1, dl, kl) 'Kl, cm/s
Call Kgp8(dpak, tp - lfvf, g1, kg) 'Kg, cm/s
Keq = 40.9 * Hl
kol8 = 1 / (1 / kl + 1 / kg / Keq)
If kotf = 0 Then kotf = kol8 * arf / lfvf
'_____equilibrium air and Max biorate________zero order________
qP = qt - q 'recycle flow m3/s
Eac = vg * k * 18 / 24400
Rb = vmax * Bi * vol / 3600 'g/s
If ci > ks And (G * k / l2) < 1 Then
' co = (ci * (q - (Eac + Edc) * (1 - f)) - Rb) / ((Eac + Edc) * f)
A11 = q + F * q - (q / qt) * (1 - F)
b11 = q + F * qP - Eac * (1 - F) * qP / qt
Co = (ci * A11 - Rb) / b11
Cip = (1 - rrtf) * ci + rrtf * Co
Cipp = Cip * (1 - F)
If Co > 0.5 * ks Then 'check exit concentration
fair = (Eac * Cipp + F * qt * Cip) / ci / q
fbio = Rb / ci / q: fbio3 = fbio
GoTo trfilt11
End If
End If
'______numerical model___________first order______________
bw = k1 * Bi / lfvf / 3600
For iter = 1 To 20
WATER9: SOURCE CODE
23-49
xold = xi
Call stripper(G, l2, k, bw, kotf, Nelemen, T, YI, Yo, xi, Xo, fair3, fbio3,
cmtr)
If (Yo < YI) Then 'g/s added mass transfer from gas to liquid
le(nt%).unittrans = (-Xo + xi) * lm1 * aa.mwt 'g/s
Else
le(nt%).unittrans = 0
End If
xnew = (xiinit * (1 - rrtf) + Xo * rrtf) * (1 - F)
xi = xold * 0.5 + xnew * 0.5
Next
fair = fair3
fbio = fbio3
R = rrtf / (1 - rrtf)
ftot = F + (1 - F) * (fair + fbio)
If ftot > 0 Then
bioratio = (1 - F) * fbio / ftot 'bioremoval/total removal
Else: bioratio = 0
End If
sumlost = ftot / (1 + ftot * R) * (1 + R)
fbio = bioratio * sumlost
fair = sumlost - fbio
If YI > 0 Then fgrem = (YI - Yo) / YI Else fgrem = 0
trfilt11:
'correct yo for spray loss
Yo = (Yo * G + xi * l2 * F) / G
If YI > 0 Then fyair = (YI - Yo) / YI Else fyair = 0
If (l2 * xi + G * YI) = 0 Or bw = 0 Then 'no inlet concentrations
tfbio = 0
Else
tfbio = ((lm1 * xiinit + G * YI) - (lm1 * Xo + G * Yo)) / (lm1 * xiinit + G *
YI)
End If
Call setx1(F, fair3, fbio3, fair, fbio, kotf, Yo, Xo, fyair, tfbio)
le(nt%).yout = Yo
le(nt%).gmout = G
le(nt%).vout = G * Yo * aa.mwt
le(nt%).ventt = temp
Break = Break
If showprint = 1 Then
ppprnt "The ratio of gas to liquid is " + Format$(moleratio, "####.####") + "
mol ratio; " + Format$(volratio, "####.####") + " vol ratio"
ppprnt "The surface area per volume of the bed is " + Str$(arf) + " (cm2/cc)."
ppprnt "The superficial gas velocity is " + Str$(sgv) + " (m/s)."
ppprnt "The effective biomass in the bed is " + Format$(Bi, "##.###### g/L")
'mg /cm2
ppprnt "The bioremoval fraction (material balance) is " + Format$(tfbio,
"#.######")
ppprnt "The mol fraction in the inlet gas is " + Format$(YI, "#.###e+00")
ppprnt "The mol fraction in the outlet gas is " + Format$(Yo, "#.###e+00")
ppprnt "The mol fraction in the inlet liquid is " + Format$(xi, "#.###e+00")
ppprnt "The mol fraction in the outlet liquid is " + Format$(Xo, "#.###e+00")
ppprnt "The fractional removal of compound fron the inlet gas is " +
Format$(fgrem, "#.###e+00")
ppprnt "The exit gas rate is " + Format$(Yo * G * aa.mwt, "#.###e+00") + "
g/s."
WATER9: SOURCE CODE
23-50
End If
cout = Xo * l2 * (1 - rrtf) * aa.mwt
le(nt%).cline = cout 'g/s liquid
le(nt%).cout = cout 'g/s liquid
le(nt%).feline = 0
le(nt%).crem = tfbio * (ci * q + YI * G * aa.mwt)
le(nt%).fbio = tfbio
le(nt%).fsludge = 0
le(nt%).foil = 0
If Xo > xiinit Then
le(nt%).unittrans = lm1 * (Xo - xiinit) * aa.mwt
le(nt%).feunit = 0
Else
le(nt%).unittrans = 0
le(nt%).feunit = fair
End If
le(nt%).eunit = 0
le(nt%).vout = Yo * G * aa.mwt
le(nt%).e = 0
ASGN2(179) = Xo
ASGN2(180) = fgrem
ASGN2(181) = tfbio
ASGN2(178) = Yo
findexitvent isect 'also updates flows in vent, air
End Sub
Public Sub poroussol(isect)
'44 and 45
' nn% is the munber of the compound in the tagged set
' time days after initial time
' e air emissions, g/sec.
' Wid is the width of the plot (cm)
nn% = isect
Call sets8(nn%, n%, ci, v, q, T)
Call Tcorr(k1, vmax, dl, dv, vp, Hl, T, nn%)
TimeExp = asgn8(7)
ea = asgn8(5) 'air porosity
et = asgn8(4) 'Total porosity
k = Hl * 55555
wasteflow = L(isect).wflow * 1000 'g/s waste
oilflow = L(isect).oil 'g/s
solidflow = L(isect).solids 'g/s
If wasteflow = 0 Then
Call setx8(0, 0, 0, 0, 0, 0, 0) ' 13-19
Call sumrates8(nn%, 0, 0, 0, ta, 0)
Exit Sub
End If
oilfract = oilflow / wasteflow 'weight fraction oil in waste
solidfract = solidflow / wasteflow 'weight fraction solids in waste
wfwater = (wasteflow - solidflow - oilflow) / wasteflow 'weight fraction water
in liquid
compflow = le(isect).cin 'g/s
ppmw = compflow / wasteflow * 1000000 'compound mg/l in waste
WATER9: SOURCE CODE
23-51
ppmwo = compflow / wasteflow / (1 - solidfract) * 1000000 'compound mg/l in
liquid
el = et - ea: If el <= 0 Then el = 0.1 'default
rhol = 0.9
' timetofill = volume * el * rhol / wasteflow / 3600 / 24 / 365.25 * 12 'months
' solidmass = volume * (1 - et) * 2 / 1000 'default solid density=2 g/cc
' wastemass = solidflow / 1000 * timetofill * 3600 * 24 * 365.25 / 12 'kg
' stabilize = (solidmass - wastemass) / timetofill 'kg/month
Call fractionoil(oilfract, owpc, owr, frinoil, 0)
aqfg = asgn(79) 'aqueous flag: organic waste, =1; aqueous waste, =0
mwt = aa.mwt 'molecular weight of constituent
mwtl = asgn8(6) 'molecular weight of solvent
tk = T + 273.16 'temperature in soil, deg Kelvin
R = 82.05 'atm-cm3/g mol-k
c = asgn8(9) 'waste in matrix g/cm3
oilload = c * oilfract 'oil in matrix g/cm3
de = difvv(dv, et, ea, 1) 'diffusivity in soil (cm2/s)
Lz = asgn8(3) 'depth of contamination cm
If Lz = 0 Then Exit Sub
Mo = c * Lz * ppmw / 1000000! 'loading g constituent/cm2 soil surface
Co = Mo / Lz
bz = asgn8(8) 'g/l biomass density
AREA = asgn8(11) * 10000 'cm2
wid = (AREA) ^ 0.5 'cm
' mass transfer at surface of soil, cm/s
kg = KGC8(v, wid, dv, 2) * 240 * 100 'cm/s
If bz < 1E-30 Or vmax < 1E-30 Then
tb = 10000000000#
Else:
'based upon .01 g/cc biomass conc.
tb = 86400! * 5.58823 / bz / vmax 'rate const. biological, (s)
End If
' calculate temperature correction factor for vapor pressure
' If ac.b < 1 Then ac.b = 1000: ac.c = 273.16
' vpref = Exp((-ac.b / (25 + ac.c)) * 2.302528)
' vpspec = Exp((-ac.b / (asgn8(9) + ac.c)) * 2.302528)
' Tcorr = vpspec / vpref
' If asgn(79) = 0 Then 'aqueous flag: organic waste, =1; aqueous waste, =0
' keq = Tcorr * ac.vp / 760 * MWoil / r / Tk * ea / asgn(75) 'solvent is
oil
' Else: keq = Tcorr * ac.hl * 1000000! / r / Tk * ea / asgn(75) 'solvent is
water
' End If
If mwt = 0 Then mwt = 100
If frinoil < 0.2 Then 'aqueous
cw = compflow * (1 - frinoil) / wasteflow / wfwater ' ppmw in water
Xz = cw / ((1000000 - cw) * mwt / 18 + cw) 'mole fract in water
kr = k
Y = kr * Xz
If c = 0 Then Exit Sub
'keq = kr * ea / r / tk / c 'ratio comp in gas/comp in porous solids
gv = ea / R / tk
Keq = kr * gv / (kr * gv + c / 18)
Oilywaste = 0
Else 'oily
WATER9: SOURCE CODE
23-52
coil = compflow * frinoil / wasteflow / oilfract * 1000000 'ppmw in oil
Xz = coil / ((1000000 - coil) * mwt / mwtl + coil)
kr = vp / 760
Y = Xz * kr
gv = ea / R / tk
Keq = kr * gv / (kr * gv + oilload / mwtl) 'ratio comp in gas/comp in porous
solids
Oilywaste = 1
End If
' cs = concvapor(kr, MWT, Xz, T) 'g/cc
keqdef = 0.0002106
If Keq <= 0 Then Keq = keqdef
'___________
'T = 8! * 3600!: GoSub landtreat1: X(4) = e ' 8 hr calculation
'T = 15! * 60!: GoSub landtreat1: X(5) = einst '15 min calculation
Tms = TimeExp * 24 * 3600 'sec 'time specified calculation
Kv = Keq * de / Lz / Lz
Z2 = Tms / tb
Kd = Pi * Pi / 4 * Kv
KvT = Kv * Tms
KvTb = Kv * tb
KdTb = Kd * tb
Kdt = Kd * Tms
' instantaneous emissions
If KvT < 0.213 Then
e = Mo / Lz / (ea / Keq / kg + (Pi * Tms / de / Keq) ^ 0.5) * Exp(-Z2)
'g/cm2-s
Else: e = Mo * (2 * Kv) * Exp(-Kdt - Z2)
End If
einst = e * AREA 'g/cm2-s*cm2 >> g/s
' average emissions
If KdTb > 0.62 Then
fa = 0.811 * KdTb / (KdTb + 1) + 0.1878
Else: fa = (KvTb) ^ 0.5
End If
ltfbio = 1 - fa
If fa < 0.33 Then
'Fatp = Fa * (1 - EXP(-Kdt))
Fatp = (KvT / Pi) ^ 0.5 * 2: gerf = 1
fat = fa * (1 - Exp(-Kdt - Z2))
ElseIf KvT > 0.22 Then
Fatp = 8 / Pi / Pi * (1 - Exp(-Kdt)) + 0.1878: gerf = 2
fat = 8 / Pi / Pi / (1 + 1 / KdTb) * (1 - Exp(-Kdt - Z2)) + 0.1878
Else
Fatp = (KvT / Pi) ^ 0.5 * 2: gerf = 3
fat = Fatp * (1 - 1 / 3 * Z2)
End If
fremain = (1 - Fatp) * Exp(-Z2)
fbio = 1 - fat - fremain:
If fbio > (1 - fa) Then fbio = (1 - fa): fremain = 1 - fat - fbio
e = fat * AREA * Mo / Tms
fair = fat
If showprint = 1 Then
If Oilywaste = 0 Then
ppprnt "Aqueous waste characteristics are assumed."













Else
ppprnt "Oily waste characteristics and Raoult's law are assumed."
End If
ppprnt "The waste loading is " + Format$(c, "##.####") + " g/cc."
ppprnt "The oil loading is " + Format$(oilload, "##.####") + " g/cc."
ppprnt "The area of the unit is " + Str$(AREA) + " cm2."
ppprnt "The instantaneous rate is " + Format$(einst, "##.###### g/s")
ppprnt "The long term bioremoval fraction is " + Format$(ltfbio, "#.######")
End If
'192,191
Call setx1(Keq, tb, de, 1 - fa, fremain, fat, (1 - fbio - fat) * ppmwo, fbio,
einst, oilload) '2-11
Call setx8(0, 0, 0, fair, fbio, 0, fads) ' 13-19
Call sumrates8(nn%, fair, fbio, fads, ta, 0)
le(isect).e = fair * le(isect).cin
le(isect).AREA = AREA / 10000 'm2
le(isect).cremain = le(isect).cin * fremain
le(isect).cout = 0 'le(isect).cin * fremain
le(isect).crem = le(isect).cin * fbio
End Sub
Public Sub fill(isect)
mwt = aa.mwt
temp = asgn8(2)
ea = asgn8(4)
et = asgn8(5)
AREA = asgn8(6) * 10000 'cm2
porl = asgn8(7) 'air porosity in waste
delp = asgn8(8) 'mbar
deltemp = asgn8(9) 'change in ref.temperature with diurnal change
timeperiod = asgn8(10) 'time for barometric pumping sec.
oilinL = asgn8(11) 'fraction oil in liquid after stabilization
rhol = asgn8(12) 'waste liquid in waste after stabilization g/cc
mwtl = asgn8(13)
timexp = asgn8(14) * 2629800! 'sec
d = asgn8(15) * 100 'depth (cm)
volg = asgn8(16) 'cc gas per g converted
biomass = asgn8(17) 'g/cc
totalbiorate = asgn8(18) 'mg/g bio-hr
volume = AREA * d 'cc
le(isect).AREA = asgn8(6) 'm2
wasteflow = L(isect).wflow * 1000 'g/s waste
oilflow = L(isect).oil
solidflow = L(isect).solids
If wasteflow = 0 Then Exit Sub
oilfract = oilflow / wasteflow 'weight fraction oil in waste
solidfract = solidflow / wasteflow 'weight fraction solids in waste
wfwater = (wasteflow - solidflow - oilflow) / wasteflow 'weight fraction water
in liquid
compflow = le(isect).cin
c = compflow / wasteflow / (1 - solidfract) 'compound mg/l in liquid
Co = rhol * compflow / wasteflow 'g/cc in waste
el = et - ea: If el <= 0 Then el = 0.1 'default
timetofill = volume * rhol / wasteflow / 3600 / 24 / 365.25 * 12 'months
WATER9: SOURCE CODE
23-54
If timetofill = 0 Then Exit Sub
solidmass = volume * 2 / 1000000 'Mg, default solid density=2 g/cc
wastemass = solidflow / 1000000 * timetofill * 3600 * 24 * 365.25 / 12 'Mg
stabilize = (solidmass - wastemass) / timetofill 'Mg/month
If aa.k1 > 0 Then ks = aa.biov / aa.k1 Else ks = 0
nt% = isect
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
Call fractionoil(oilfract, owpc, owr, frinoil, 0)
k = Hl * 55555
total = volume * Co 'total grams volatile compound in landfill
R = 82.05 'cc-atm/mol-deg K
dvf = difvv(dv, et, ea, 1)
If mwt < 2 Then mwt = 100 'default
If frinoil < 0.5 Then 'aqueous present
c = compflow * (1 - frinoil) / wasteflow / wfwater * 1000000 ' ppmw in water
Xz = c / ((1000000 - c) * mwt / 18 + c)
kr = k
Y = kr * Xz
Else 'oily
c = compflow * frinoil / wasteflow / oilfract * 1000000 'ppmw in oil
If mwt < 2 Then mwt = 100
Xz = c / ((1000000 - c) * mwt / mwtl + c)
kr = vp / 760
Y = Xz * kr
End If
cs = concvapor(kr, mwt, Xz, temp) 'g/cc
'caq is ratio of mg/l in water to g/cc air
caq = R * (temp + 273.16) / 18 / k * 1000000
'rates of release in cc/sec
gasrate = volg * biomass * volume * totalbiorate / 1000 / 3600
If (ks + caq * cs) > 0 Then
biorate1 = vmax * biomass * volume * caq / (ks + caq * cs) / 3600 / 1000
Else: biorate1 = 0
End If
baropump = volume * porl * (Abs(delp) / 1013) / 24 / 3600 'cm3-s
'If asgn8(3) = 0 Then Exit Sub
difrate = dvf / asgn8(3) * AREA
lamda = (difrate + gasrate + baropump + biorate1)
fbio = biorate1 / lamda
fair = 1 - fbio
If total > 0 Then
fremain = Exp(-lamda * cs * timexp / total)
Else: fremain = 0
End If
einst = fair * lamda * cs * fremain 'g/s inst
If timexp = 0 Or AREA = 0 Then Exit Sub
e = fair * total * (1 - fremain) / timexp
Ef = e / AREA 'g/cc-s
avgftoair = fair * (1 - fremain)
asgn(107) = Sqr(AREA * 4 / 3.14) 'width m
If showprint = 1 Then
ppprnt "The waste loading rate is " + Str$(le(isect).cin) + " g/s"
ppprnt "The time to fill the unit is " + Str$(timetofill) + " months."
WATER9: SOURCE CODE
23-55
ppprnt " The liquid is obtained from the wastes: the solids and water are
specified with the unit. "
ppprnt "The area of the unit is " + Str$(AREA) + " cm2."
ppprnt "The total compound amount is " + Format$(total, "#.##E+00 g")
ppprnt "The concentration in the gas phase is " + Format$(cs, "#.##E+00 g/cc")
ppprnt "The concentration in landfill is " + Format$(Co, "#.##E+00 g/cc")
ppprnt "The gas release from biodegradation is " + Format$(gasrate, "#.##E+00
cc/s")
ppprnt "The compound cap diffusivity is " + Format$(dvf, "#.###### cm2/s")
ppprnt "The compound release from bio. is " + Format$(cs * gasrate, "#.##E+00
g/s")
ppprnt "The gas release from barometric pumping is " + Format$(baropump,
"#.##E+00 cc/s")
ppprnt "The compound release from barometric pumping is " + Format$(cs *
baropump, "#.##e+00 g/s.")
ppprnt "The cap diffusion rate of release is " + Format$(cs * difrate,
"#.##E+00 g/s")
ppprnt "The instantaneous rate is " + Format$(einst, "##.###### g/s") +
Format$(einst * 3.6 * 2.4 * 3.6525, " ##.###### Mg/y.")
ppprnt "The average rate is " + Format$(e, "##.###### g/s; ") + Format$(e *
3.6 * 2.4 * 3.6525, " ##.###### Mg/y.")
ppprnt "The rate constant for release, lamda (/month) is " + Str$(lamda * cs /
total * 3600 * 24 * 365 / 12)
ppprnt "The long term bioremoval fraction is " + Format$(fbio, "#.######")
End If
x11 = difrate / lamda * (1 - fremain)
x10 = baropump / lamda * (1 - fremain)
x7 = gasrate / lamda * (1 - fremain)
x5 = biorate1 / lamda * (1 - fremain)
x6 = volume / 1000000
If lamda > 0 And cs > 0 Then
x2 = total / lamda / 2629800! / cs
Else: x2 = 0
End If
Call setx1(x2, cs, Xz, timetofill, stabilize, x5, x6, x7, x10, x11) '2-11
Call setx2(fremain, fair, asgn8(14), avgftoair, 0, 0, 0, 0, 0, 0) '
Call sumrates8(nt%, avgftoair, fbio * (1 - fremain), 0, ta, 0)
le(isect).vout = le(isect).cin * fair * (1 - fremain)
le(isect).crem = le(isect).cin * fbio * (1 - fremain)
le(isect).cremain = le(isect).cin * fremain
le(isect).cout = 0
le(isect).feunit = fair * (1 - fremain)
le(isect).e = e
ASGN2(184) = asgn8(14) 'months
End Sub
Public Function difvv(dv, et, ea, CD%)
If et < ea Then et = ea
If ea = 0 Then ea = et
If CD% = 0 Then CD% = 1
If CD% = 1 Then 'farmer
If et > 0 Then a = dv * ea ^ 3.33 / et / et
ElseIf CD% = 2 Then
a = dv * ea ^ 1.333
End If
WATER9: SOURCE CODE
23-56
difvv = a
End Function
Public Function concvapor(k, mwt, XM, T)
'k partition factor atm/mole fraction
'xm mole fraction
'mwt compound molecular weight
concvapor = k * mwt * XM / 22400 * 273 / (273 + T)
End Function
Public Sub containerload(nt%)
mwt = aa.mwt
temp = ASGN2(52)
tempr = temp * 1.8 + 32 + 460 'deg R
s = ASGN2(53) 'saturation factor
drumflag% = ASGN2(54) 'drums =1
If drumflag% = 1 Then sf = 0.0001 Else sf = 0.00001
volume = ASGN2(51) 'm3
q = ASGN2(50) 'm3/yr
mwtl = ASGN2(55)
density = ASGN2(56)
Co = asgn(73) 'ppmw in liquid
If mwtl < 25 Then FLAG% = 1 'aqueous
Call Tcorr(k1, vmax, dl, dv, vp, Hl, temp, nt%)
'%%%% compile error truevp = tvp(hl, vp, Co, temp, density, mwtl, X, Y, FLAG%)
* 14.7 'pressure (psia)
fractloss.spill = sf
qs = q * Co / 365.25 / 24 / 3600 ' VO throughput (g/s)
loadingloss = 12.46 / tempr * s * aa.mwt * truevp
metricloss = loading.loss * 453.6 / 3.785
fractlossLoading = metricloss / Co / density
e = (fractlossLoading + fractlossSpill) * qs 'g/s
Ef = 0 'g/cc-s
asgn(107) = 0 'width m
ASGN2(157) = Ef
Dim X(14):
X(3) = fractlossLoading: X(4) = fractlossSpill: X(6) = X(3) + X(4)
' Call printshort2("container loading and spills", 6, X(), e, ef, aa.cs, iff)
End Sub
Public Sub specControl(nt%)
'this unit is a control device with a vent to the atmosphere
findinvent nt%
gps = le(nt%).vup
temp = le(nt%).tup
ventf = le(nt%).gmup * 22400 * (273 + temp) / 273
L(nt%).vent = ventf 'cc/s in treated vent
L(nt%).Uvent = 0 'not used
WATER9: SOURCE CODE
23-57
frem = L(nt%).leng 'specified
le(nt%).gmvent = le(nt%).gmup 'mol gas /s
L(nt%).temp = temp
le(nt%).vcontrol = frem * gps
le(nt%).cout = 0 'g/s out liquid flow
le(nt%).vout = gps * (1 - frem) 'g/s
le(nt%).yvent = le(nt%).yup * (1 - frem) 'y out
gasflow = ventf * 1000 'cc/s
findexitvent nt%
If le(nt%).gmup > 0 Then noncond = le(nt%).nonupmps / le(nt%).gmup Else noncond =
0
For i = 170 To 192: ASGN2(i) = 0: Next
' 172 3 4 5 6 7
Call setx1(gasflow, noncond, gps, frem, gasflow, le(nt%).yvent, 0, 0, 0, 0)
End Sub
Public Sub wetbulb(drytemp, wettemp, humid)
'input air temp C, Humidity 0-1 in dry bulb air
'output drytemp dry bulb temp
a = 0.00393673: bx = -0.0001841304
If humid > 1 Then humid = 1
If humid < 0 Then humid = 0
vp = humid * Exp((1 / (drytemp + 273.16) - a) / bx)
delt = -2
wettemp = drytemp
For i = 1 To 20
wettemp = wettemp + delt
vptest = Exp((1 / (wettemp + 273.16) - a) / bx)
Break = Break
If vptest > vp Then 'temp too high
delt = -Abs(delt) * 0.6
ElseIf vptest < vp Then 'temp too low
delt = Abs(delt) * 0.6
End If
Next
End Sub
Public Sub sigma(width, offset, sclass, arear, v, dilution)
'estimated dispersion depth over a surface
'width is path over area in m
'offset is distance downwind from unit
'sclass is stability class, 4=neutral
'v wind velocity m/s
'arear area rate g/s per m2
'dilution is g/m3 downwind based upon uniform dispersion in +/- 1 sd
If v = 0 Or sclass = 0 Or arear = 0 Then dilution = 0: Exit Sub
If sclass < 3 Then
p1 = 0.24: p2 = 0.001: Expp = 0.5
ElseIf sclass = 3 Then
p1 = 0.2: p2 = 0: Expp = 1
ElseIf sclass = 4 Then
p1 = 0.14: p2 = 0.0003: Expp = -0.5
Else
p1 = 0.08: p2 = 0.0015: Expp = -0.5
WATER9: SOURCE CODE
23-58
End If
dilution = 0
For i = 1 To 20
Xpath = width / 20 * i + offset
sigmaz = p1 * Xpath * (1 + p2 * Xpath) ^ Expp
dilution = dilution + width * arear / 20 / (2 * sigmaz) / v 'g/m3 downwind
Next
End Sub
Public Sub kadjust(Hl, nt%)
If Mid$(aa.code, 6, 1) = "k" Then
pH = L(nt%).pH
Ka = aa.rh
If pH = 0 Or Ka = 0 Then
Else
oH3 = 10 ^ (-pH)
comp2 = Ka / oH3
Hl = Hl / (1 + comp2)
If showprint = 1 Then
ppprnt " pH = " + FORMP(pH)
ppprnt " Ka = " + FORMP(Ka)
ppprnt " The adjusted Hl is " + FORMP(Hl) + " atm-m3/g mol"
End If
End If
End If
End Sub
Public Sub ionization(HL1, pH)
If pH <> 0 And HL1 > 0 Then
If Mid$(aa.code, 6, 1) = "k" Or Mid$(aa.code, 6, 1) = "b" Then
Ka = aa.rh
If pH = 0 Or Ka = 0 Then
Else
oH3 = 10 ^ (-pH)
If oH3 = 0 Then
MsgBox "error in pH"
Exit Sub
End If
comp2 = Ka / oH3
If Mid$(aa.code, 6, 1) = "k" Then
HL1 = HL1 / (1 + comp2)
ElseIf Mid$(aa.code, 6, 1) = "b" Then
HL1 = HL1 * comp2 / (1 + comp2)
End If
End If
End If
End If
Break = Break
End Sub
Public Sub hlestimate(code$, ppmw, vp, mwt, Ka, Hl)
'concentration ppm in water
'vp pressure in mm Hg
'estimates hl from vp and solubility
'corrects for ionization of species
If ppmw = 0 Or vp <= 0 Then Hl = 0: Exit Sub
If Ka = 0 Then
WATER9: SOURCE CODE
23-59
conhls = 18! * 760! / 1000000!
Hl = vp / ppmw / conhls * mwt ' y/x
If showprint = 1 Then
ppprnt "The value of Ka is zero. The estimation equation is "
ppprnt "vp / ppmw / (18! * 760! / 1000000!) * mwt"
End If
ElseIf code$ = "b" Then 'basic compound
molarity = ppmw / mwt / 1000
constant = 0.00000000000001 / Ka / (molarity)
fract = (-constant + Sqr(constant * constant + 4 * constant)) / 2
conhls = 18! * 760! / 1000000!
Hl = vp / (ppmw * (1 - fract)) / conhls * mwt ' y/x
If showprint = 1 Then
ppprnt "The compound is basic. "
ppprnt "The solubility (ppmw) is " + FORMP(ppmw)
ppprnt "The molarity at solubility is " + Format$(molarity, "0.000e+00")
pH = 14 + Log(molarity * fract) / 2.30258
ppprnt "The estimated pH is " + Format(pH, " ##.##")
ppprnt "The fraction not ionized is " + FORMP(1 - fract)
ppprnt "The adjusted Hl is estimated with the following equation."
ppprnt "vp / (ppmw*fract) / (18! * 760! / 1000000!) * mwt"
End If
ElseIf code$ = "k" Then 'acid compound
molarity = ppmw / mwt / 1000
constant = Ka / molarity
fract = (-constant + Sqr(constant * constant + 4 * constant)) / 2
conhls = 18! * 760! / 1000000!
Hl = vp / (ppmw * (1 - fract)) / conhls * mwt ' y/x
If showprint = 1 Then
ppprnt "The compound is a acidic. "
ppprnt "The solubility (ppmw) is " + FORMP(ppmw)
ppprnt "The molarity at solubility is " + Format$(molarity, "0.000e+00")
pH = -Log(molarity * fract) / 2.30258
ppprnt "The estimated pH is " + Format(pH, " ##.##")
ppprnt "The fraction not ionized is " + FORMP(1 - fract)
ppprnt "The unadjusted Hl is " + FORMP(vp / ppmw / conhls * mwt)
ppprnt "The adjusted Hl is estimated with the following equation."
ppprnt "vp / (ppmw*fract) / (18! * 760! / 1000000!) * mwt"
End If
End If
End Sub
