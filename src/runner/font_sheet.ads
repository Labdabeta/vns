with SDL;
pragma Style_Checks (Off);
package Font_Sheet is
    Raw_Data : aliased SDL.Raw_Image_Data := (
1..1545|
1560..1580|
1589..1608|
1624..1638|
1656..1680|
1689..1702|
1723..1735|
1755..1768|
1781..1797|
1818..1830|
1850..1871|
1881..1894|
1915..1925|
1932..1941|
1948..1956|
1973..1990|
1994..2004|
2011..2036|
2043..2061|
2067..2067|
2073..2086|
2091..2116|
2122..2134|
2141..2149|
2155..2158|
2165..2197|
2203..2229|
2235..2252|
2258..2259|
2265..2278|
2283..2308|
2314..2327|
2334..2350|
2357..2389|
2396..2421|
2427..2442|
2449..2451|
2457..2470|
2475..2499|
2505..2516|
2526..2542|
2549..2581|
2587..2612|
2618..2633|
2639..2643|
2649..2662|
2667..2691|
2697..2706|
2718..2734|
2741..2773|
2779..2803|
2809..2824|
2830..2835|
2841..2854|
2859..2883|
2889..2895|
2903..2904|
2910..2926|
2933..2963|
2970..2985|
2998..3014|
3020..3027|
3033..3046|
3064..3075|
3080..3084|
3092..3096|
3102..3118|
3125..3154|
3161..3177|
3193..3205|
3211..3219|
3225..3238|
3258..3267|
3273..3273|
3281..3288|
3294..3310|
3317..3344|
3351..3379|
3387..3396|
3401..3411|
3417..3444|
3452..3459|
3470..3480|
3486..3502|
3509..3534|
3541..3574|
3580..3586|
3592..3603|
3609..3638|
3644..3651|
3659..3672|
3678..3694|
3701..3724|
3731..3767|
3773..3777|
3807..3830|
3837..3844|
3850..3863|
3869..3886|
3893..3913|
3921..3959|
3965..3969|
3999..4022|
4028..4036|
4043..4054|
4060..4078|
4085..4103|
4110..4150|
4156..4179|
4185..4213|
4220..4229|
4236..4244|
4251..4270|
4277..4293|
4300..4340|
4347..4371|
4377..4403|
4411..4423|
4442..4453|
4477..4484|
4510..4517|
4537..4563|
4569..4581|
4600..4617|
4631..4645|
4669..4676|
4702..4709|
4726..4755|
4761..4773|
4789..7693|
7707..7715|
7741..7752|
7769..7784|
7800..7820|
7829..7844|
7864..7882|
7899..7907|
7933..7942|
7951..7954|
7964..7974|
7983..7985|
7994..8011|
8022..8036|
8059..8072|
8080..8119|
8125..8133|
8139..8150|
8156..8164|
8171..8181|
8187..8203|
8208..8208|
8214..8228|
8235..8245|
8252..8262|
8269..8310|
8316..8324|
8330..8343|
8349..8355|
8362..8374|
8380..8394|
8400..8401|
8407..8420|
8427..8438|
8444..8453|
8459..8501|
8507..8516|
8523..8534|
8541..8547|
8553..8567|
8573..8585|
8591..8594|
8600..8612|
8619..8630|
8636..8645|
8650..8691|
8698..8709|
8716..8725|
8732..8739|
8745..8759|
8765..8776|
8782..8786|
8793..8804|
8811..8822|
8828..8836|
8842..8847|
8854..8882|
8889..8902|
8911..8914|
8922..8931|
8937..8951|
8957..8968|
8974..8979|
8985..8996|
9003..9012|
9019..9028|
9050..9073|
9080..9097|
9112..9124|
9131..9143|
9149..9159|
9165..9172|
9178..9188|
9208..9220|
9230..9236|
9244..9264|
9271..9290|
9304..9317|
9327..9328|
9341..9350|
9356..9364|
9371..9380|
9402..9411|
9418..9431|
9437..9455|
9462..9479|
9487..9489|
9499..9511|
9527..9527|
9533..9542|
9548..9557|
9563..9572|
9579..9588|
9596..9604|
9610..9624|
9630..9646|
9653..9669|
9676..9685|
9692..9719|
9725..9733|
9739..9750|
9756..9764|
9771..9783|
9789..9796|
9802..9816|
9822..9837|
9843..9860|
9866..9878|
9885..9910|
9916..9924|
9949..9956|
9963..9975|
9982..9988|
9994..10008|
10014..10028|
10034..10051|
10058..10071|
10078..10101|
10107..10115|
10141..10148|
10155..10167|
10174..10181|
10187..10199|
10205..10219|
10225..10243|
10250..10263|
10269..10292|
10298..10307|
10313..10328|
10334..10340|
10347..10359|
10365..10374|
10380..10390|
10396..10410|
10416..10436|
10443..10454|
10461..10480|
10489..10498|
10504..10520|
10527..10532|
10539..10549|
10556..10567|
10587..10601|
10607..10629|
10651..10661|
10678..10689|
10696..10713|
10720..10724|
10746..10761|
10776..10792|
10798..10824|
10840..10853|
10865..10881|
10887..10906|
10912..10916|
10934..13835|
13853..13859|
13878..13894|
13916..13926|
13948..13963|
13981..13987|
13993..14008|
14014..14024|
14045..14051|
14074..14086|
14108..14118|
14140..14152|
14173..14179|
14185..14200|
14206..14214|
14222..14235|
14237..14243|
14249..14260|
14268..14278|
14284..14310|
14316..14342|
14350..14371|
14377..14392|
14398..14405|
14412..14435|
14441..14454|
14461..14470|
14476..14502|
14508..14532|
14539..14563|
14569..14584|
14590..14596|
14603..14627|
14633..14647|
14654..14662|
14668..14694|
14700..14723|
14730..14755|
14761..14776|
14782..14787|
14794..14819|
14825..14840|
14846..14854|
14860..14886|
14892..14915|
14921..14947|
14953..14968|
14974..14979|
14985..15011|
15017..15032|
15039..15046|
15052..15078|
15084..15106|
15113..15139|
15145..15160|
15166..15171|
15177..15203|
15209..15224|
15231..15238|
15259..15270|
15291..15298|
15304..15312|
15326..15331|
15358..15363|
15369..15395|
15401..15417|
15423..15430|
15451..15462|
15483..15490|
15496..15504|
15518..15523|
15550..15555|
15561..15587|
15593..15608|
15615..15622|
15628..15654|
15660..15682|
15688..15704|
15710..15715|
15721..15736|
15742..15747|
15753..15779|
15785..15800|
15806..15814|
15820..15846|
15852..15874|
15880..15896|
15902..15907|
15913..15928|
15934..15939|
15946..15971|
15977..15992|
15998..16006|
16012..16038|
16044..16067|
16073..16088|
16094..16099|
16105..16120|
16126..16132|
16139..16163|
16169..16183|
16189..16198|
16204..16230|
16236..16259|
16266..16280|
16286..16291|
16297..16312|
16318..16325|
16332..16355|
16361..16373|
16380..16390|
16396..16422|
16428..16452|
16459..16472|
16478..16483|
16489..16504|
16510..16518|
16527..16538|
16541..16547|
16553..16562|
16571..16582|
16588..16614|
16620..16645|
16654..16664|
16670..16675|
16681..16696|
16702..16712|
16733..16739|
16760..16774|
16796..16806|
16812..16839|
16862..16867|
16873..16888|
16894..16907|
16924..16931|
16948..16966|
16988..16998|
17004..17035|
17052..17059|
17065..17080|
17086..19973|
19996..20006|
20026..20036|
20042..20054|
20061..20071|
20077..20099|
20107..20118|
20126..20131|
20140..20152|
20158..20165|
20188..20198|
20218..20228|
20234..20244|
20251..20263|
20269..20291|
20300..20309|
20318..20323|
20333..20344|
20350..20365|
20372..20404|
20410..20420|
20426..20434|
20441..20455|
20461..20483|
20492..20500|
20505..20505|
20510..20515|
20526..20536|
20542..20557|
20564..20596|
20602..20612|
20618..20625|
20631..20647|
20653..20675|
20680..20680|
20685..20692|
20697..20697|
20702..20707|
20713..20713|
20719..20728|
20734..20749|
20756..20788|
20794..20804|
20810..20815|
20822..20839|
20845..20867|
20872..20872|
20878..20883|
20888..20889|
20894..20899|
20905..20906|
20912..20920|
20926..20941|
20948..20980|
20986..20996|
21002..21005|
21012..21031|
21037..21059|
21064..21065|
21071..21074|
21079..21081|
21086..21091|
21097..21099|
21105..21112|
21118..21133|
21140..21172|
21178..21188|
21194..21196|
21202..21223|
21229..21250|
21256..21258|
21263..21265|
21270..21273|
21278..21283|
21289..21291|
21298..21304|
21310..21325|
21332..21364|
21370..21380|
21386..21386|
21392..21415|
21421..21442|
21448..21451|
21456..21456|
21461..21465|
21471..21475|
21481..21484|
21491..21496|
21502..21517|
21524..21556|
21562..21572|
21584..21607|
21613..21634|
21640..21644|
21653..21657|
21663..21667|
21673..21677|
21684..21688|
21694..21709|
21716..21748|
21754..21764|
21770..21771|
21778..21799|
21805..21826|
21832..21836|
21844..21849|
21855..21859|
21865..21870|
21877..21880|
21886..21901|
21908..21940|
21946..21956|
21962..21965|
21972..21991|
21997..22018|
22024..22029|
22035..22041|
22047..22051|
22057..22063|
22070..22072|
22078..22093|
22100..22132|
22138..22148|
22154..22158|
22165..22183|
22189..22210|
22215..22233|
22239..22243|
22249..22256|
22263..22264|
22270..22285|
22292..22324|
22330..22340|
22346..22352|
22359..22375|
22381..22402|
22407..22425|
22431..22435|
22441..22449|
22456..22456|
22462..22477|
22484..22515|
22521..22532|
22538..22546|
22553..22567|
22573..22594|
22599..22617|
22623..22627|
22633..22642|
22648..22648|
22654..22669|
22676..22693|
22696..22706|
22713..22724|
22730..22739|
22746..22759|
22765..22786|
22791..22809|
22815..22819|
22825..22835|
22846..22853|
22876..22885|
22903..22916|
22922..22933|
22940..22951|
22973..22978|
22983..23001|
23007..23011|
23017..23028|
23038..23045|
23068..23078|
23092..23108|
23114..23126|
23134..23143|
23165..23169|
23175..23194|
23199..23203|
23209..23221|
23230..26121|
26137..26148|
26168..26185|
26201..26213|
26231..26248|
26267..26274|
26302..26310|
26331..26340|
26363..26374|
26395..26405|
26426..26438|
26459..26466|
26494..26501|
26508..26517|
26525..26532|
26539..26549|
26557..26565|
26572..26581|
26589..26597|
26603..26612|
26619..26628|
26635..26669|
26676..26692|
26698..26711|
26718..26724|
26731..26743|
26749..26756|
26762..26775|
26782..26789|
26795..26805|
26812..26820|
26826..26861|
26868..26883|
26889..26904|
26910..26916|
26923..26935|
26942..26947|
26953..26968|
26974..26981|
26987..26998|
27004..27012|
27018..27053|
27060..27074|
27081..27097|
27103..27108|
27115..27128|
27134..27138|
27144..27161|
27167..27173|
27179..27189|
27196..27204|
27211..27245|
27252..27266|
27272..27289|
27295..27300|
27307..27319|
27325..27330|
27336..27353|
27359..27365|
27371..27380|
27387..27397|
27406..27437|
27444..27458|
27464..27481|
27487..27492|
27499..27510|
27517..27522|
27528..27545|
27551..27557|
27577..27591|
27604..27629|
27636..27650|
27656..27673|
27679..27684|
27691..27699|
27707..27714|
27720..27737|
27743..27749|
27766..27787|
27800..27821|
27828..27842|
27848..27865|
27871..27876|
27897..27906|
27912..27929|
27935..27941|
27947..27951|
27958..27984|
27995..28013|
28020..28034|
28040..28057|
28063..28068|
28084..28098|
28104..28121|
28127..28133|
28139..28145|
28152..28180|
28188..28205|
28212..28226|
28233..28248|
28255..28260|
28267..28290|
28297..28312|
28319..28325|
28331..28339|
28345..28374|
28381..28397|
28404..28419|
28425..28440|
28446..28452|
28459..28483|
28489..28504|
28510..28517|
28523..28532|
28538..28567|
28573..28589|
28596..28611|
28618..28631|
28637..28644|
28651..28675|
28682..28695|
28701..28709|
28715..28725|
28731..28759|
28765..28781|
28788..28805|
28812..28821|
28828..28836|
28843..28868|
28876..28884|
28892..28901|
28907..28918|
28924..28931|
28933..28949|
28956..28973|
28980..28998|
29018..29028|
29035..29062|
29082..29093|
29099..29111|
29117..29123|
29147..29165|
29172..29193|
29207..29220|
29227..29257|
29271..29285|
29291..29304|
29310..29315|
29335..29357|
29364..29453|
29459..29646|
29652..29839|
29846..29851|
29856..30032|
30049..30228|
30237..32259|
32265..32280|
32286..32289|
32295..32314|
32320..32321|
32327..32346|
32352..32354|
32361..32376|
32382..32385|
32392..32409|
32416..32420|
32445..32451|
32457..32472|
32478..32481|
32488..32505|
32512..32514|
32519..32538|
32543..32548|
32555..32566|
32573..32578|
32585..32600|
32607..32612|
32637..32643|
32649..32664|
32670..32674|
32681..32697|
32703..32706|
32711..32730|
32735..32741|
32748..32757|
32764..32771|
32778..32791|
32798..32821|
32828..32835|
32841..32856|
32862..32867|
32873..32888|
32894..32898|
32903..32922|
32927..32935|
32941..32948|
32954..32965|
32971..32982|
32988..33012|
33019..33027|
33033..33048|
33054..33059|
33066..33079|
33085..33090|
33096..33114|
33119..33128|
33135..33138|
33145..33158|
33165..33173|
33179..33202|
33209..33219|
33225..33240|
33246..33252|
33259..33270|
33277..33282|
33288..33305|
33311..33321|
33328..33329|
33335..33351|
33358..33364|
33370..33393|
33400..33411|
33417..33432|
33438..33445|
33451..33462|
33468..33474|
33480..33486|
33491..33497|
33503..33515|
33526..33544|
33551..33554|
33561..33584|
33590..33603|
33609..33624|
33630..33638|
33644..33653|
33659..33666|
33672..33677|
33684..33689|
33694..33708|
33717..33737|
33744..33745|
33752..33774|
33781..33795|
33801..33816|
33822..33830|
33837..33844|
33850..33859|
33864..33868|
33877..33881|
33886..33900|
33908..33931|
33942..33965|
33971..33987|
33993..34008|
34014..34023|
34029..34036|
34042..34051|
34056..34059|
34064..34064|
34070..34073|
34078..34091|
34102..34124|
34133..34155|
34162..34179|
34185..34200|
34206..34216|
34222..34227|
34233..34243|
34248..34251|
34256..34257|
34262..34265|
34270..34282|
34288..34288|
34295..34317|
34324..34346|
34353..34371|
34377..34392|
34398..34408|
34415..34418|
34424..34435|
34440..34442|
34447..34449|
34455..34457|
34462..34472|
34479..34482|
34489..34509|
34516..34536|
34543..34563|
34569..34584|
34590..34601|
34608..34610|
34615..34627|
34633..34633|
34638..34642|
34648..34649|
34654..34663|
34670..34675|
34682..34701|
34708..34727|
34734..34756|
34762..34775|
34781..34794|
34800..34801|
34807..34819|
34825..34825|
34830..34835|
34841..34841|
34846..34853|
34860..34868|
34875..34893|
34900..34918|
34924..34948|
34955..34965|
34972..34987|
34998..35011|
35021..35028|
35033..35033|
35037..35044|
35051..35062|
35069..35085|
35092..35108|
35115..35142|
35163..35179|
35189..35204|
35212..35220|
35229..35235|
35242..35255|
35262..35277|
35284..35299|
35326..35336|
35352..35372|
35381..35396|
35403..35413|
35421..35425|
35432..35448|
35456..35469|
35476..35491|
35518..38025|
38030..38217|
38229..38254|
38259..38412|
38423..38446|
38451..38610|
38617..38638|
38643..38700|
38709..38804|
38810..38830|
38835..38892|
38901..38997|
39003..39022|
39027..39084|
39093..39189|
39195..39214|
39219..39279|
39282..39380|
39387..39406|
39411..39571|
39578..39598|
39603..39755|
39768..39782|
39803..39814|
39835..39947|
39956..39974|
39995..40006|
40027..40140|
40145..40166|
40187..40198|
40219..40268|
40275..40332|
40337..40366|
40371..40430|
40435..40459|
40468..40524|
40529..40558|
40563..40620|
40629..40652|
40661..40750|
40755..40812|
40821..40845|
40853..40942|
40947..41005|
41012..41038|
41044..41099|
41106..41134|
41139..41229|
41235..41290|
41298..41326|
41331..41418|
41426..41483|
41490..41518|
41523..41606|
41615..41710|
41715..41798|
41800..43008=>(0, 0, 0, 0),
1546..1559|
1581..1588|
1609..1623|
1639..1655|
1681..1688|
1703..1722|
1736..1754|
1769..1780|
1798..1817|
1831..1849|
1872..1880|
1895..1914|
1926..1931|
1942..1947|
1957..1972|
1991..1993|
2005..2010|
2037..2042|
2062..2066|
2068..2072|
2087..2090|
2117..2121|
2135..2140|
2150..2154|
2159..2164|
2198..2202|
2230..2234|
2253..2257|
2260..2264|
2279..2282|
2309..2313|
2328..2333|
2351..2356|
2390..2395|
2422..2426|
2443..2448|
2452..2456|
2471..2474|
2500..2504|
2517..2525|
2543..2548|
2582..2586|
2613..2617|
2634..2638|
2644..2648|
2663..2666|
2692..2696|
2707..2717|
2735..2740|
2774..2778|
2804..2808|
2825..2829|
2836..2840|
2855..2858|
2884..2888|
2896..2902|
2905..2909|
2927..2932|
2964..2969|
2986..2997|
3015..3019|
3028..3032|
3047..3063|
3076..3079|
3085..3091|
3097..3101|
3119..3124|
3155..3160|
3178..3192|
3206..3210|
3220..3224|
3239..3257|
3268..3272|
3274..3280|
3289..3293|
3311..3316|
3345..3350|
3380..3386|
3397..3400|
3412..3416|
3445..3451|
3460..3469|
3481..3485|
3503..3508|
3535..3540|
3575..3579|
3587..3591|
3604..3608|
3639..3643|
3652..3658|
3673..3677|
3695..3700|
3725..3730|
3768..3772|
3778..3806|
3831..3836|
3845..3849|
3864..3868|
3887..3892|
3914..3920|
3960..3964|
3970..3998|
4023..4027|
4037..4042|
4055..4059|
4079..4084|
4104..4109|
4151..4155|
4180..4184|
4214..4219|
4230..4235|
4245..4250|
4271..4276|
4294..4299|
4341..4346|
4372..4376|
4404..4410|
4424..4441|
4454..4476|
4485..4509|
4518..4536|
4564..4568|
4582..4599|
4618..4630|
4646..4668|
4677..4701|
4710..4725|
4756..4760|
4774..4788|
7694..7706|
7716..7740|
7753..7768|
7785..7799|
7821..7828|
7845..7863|
7883..7898|
7908..7932|
7943..7950|
7955..7963|
7975..7982|
7986..7993|
8012..8021|
8037..8058|
8073..8079|
8120..8124|
8134..8138|
8151..8155|
8165..8170|
8182..8186|
8204..8207|
8209..8213|
8229..8234|
8246..8251|
8263..8268|
8311..8315|
8325..8329|
8344..8348|
8356..8361|
8375..8379|
8395..8399|
8402..8406|
8421..8426|
8439..8443|
8454..8458|
8502..8506|
8517..8522|
8535..8540|
8548..8552|
8568..8572|
8586..8590|
8595..8599|
8613..8618|
8631..8635|
8646..8649|
8692..8697|
8710..8715|
8726..8731|
8740..8744|
8760..8764|
8777..8781|
8787..8792|
8805..8810|
8823..8827|
8837..8841|
8848..8853|
8883..8888|
8903..8910|
8915..8921|
8932..8936|
8952..8956|
8969..8973|
8980..8984|
8997..9002|
9013..9018|
9029..9049|
9074..9079|
9098..9111|
9125..9130|
9144..9148|
9160..9164|
9173..9177|
9189..9207|
9221..9229|
9237..9243|
9265..9270|
9291..9303|
9318..9326|
9329..9340|
9351..9355|
9365..9370|
9381..9401|
9412..9417|
9432..9436|
9456..9461|
9480..9486|
9490..9498|
9512..9526|
9528..9532|
9543..9547|
9558..9562|
9573..9578|
9589..9595|
9605..9609|
9625..9629|
9647..9652|
9670..9675|
9686..9691|
9720..9724|
9734..9738|
9751..9755|
9765..9770|
9784..9788|
9797..9801|
9817..9821|
9838..9842|
9861..9865|
9879..9884|
9911..9915|
9925..9948|
9957..9962|
9976..9981|
9989..9993|
10009..10013|
10029..10033|
10052..10057|
10072..10077|
10102..10106|
10116..10140|
10149..10154|
10168..10173|
10182..10186|
10200..10204|
10220..10224|
10244..10249|
10264..10268|
10293..10297|
10308..10312|
10329..10333|
10341..10346|
10360..10364|
10375..10379|
10391..10395|
10411..10415|
10437..10442|
10455..10460|
10481..10488|
10499..10503|
10521..10526|
10533..10538|
10550..10555|
10568..10586|
10602..10606|
10630..10650|
10662..10677|
10690..10695|
10714..10719|
10725..10745|
10762..10775|
10793..10797|
10825..10839|
10854..10864|
10882..10886|
10907..10911|
10917..10933|
13836..13852|
13860..13877|
13895..13915|
13927..13947|
13964..13980|
13988..13992|
14009..14013|
14025..14044|
14052..14073|
14087..14107|
14119..14139|
14153..14172|
14180..14184|
14201..14205|
14215..14221|
14236..14236|
14244..14248|
14261..14267|
14279..14283|
14311..14315|
14343..14349|
14372..14376|
14393..14397|
14406..14411|
14436..14440|
14455..14460|
14471..14475|
14503..14507|
14533..14538|
14564..14568|
14585..14589|
14597..14602|
14628..14632|
14648..14653|
14663..14667|
14695..14699|
14724..14729|
14756..14760|
14777..14781|
14788..14793|
14820..14824|
14841..14845|
14855..14859|
14887..14891|
14916..14920|
14948..14952|
14969..14973|
14980..14984|
15012..15016|
15033..15038|
15047..15051|
15079..15083|
15107..15112|
15140..15144|
15161..15165|
15172..15176|
15204..15208|
15225..15230|
15239..15258|
15271..15290|
15299..15303|
15313..15325|
15332..15357|
15364..15368|
15396..15400|
15418..15422|
15431..15450|
15463..15482|
15491..15495|
15505..15517|
15524..15549|
15556..15560|
15588..15592|
15609..15614|
15623..15627|
15655..15659|
15683..15687|
15705..15709|
15716..15720|
15737..15741|
15748..15752|
15780..15784|
15801..15805|
15815..15819|
15847..15851|
15875..15879|
15897..15901|
15908..15912|
15929..15933|
15940..15945|
15972..15976|
15993..15997|
16007..16011|
16039..16043|
16068..16072|
16089..16093|
16100..16104|
16121..16125|
16133..16138|
16164..16168|
16184..16188|
16199..16203|
16231..16235|
16260..16265|
16281..16285|
16292..16296|
16313..16317|
16326..16331|
16356..16360|
16374..16379|
16391..16395|
16423..16427|
16453..16458|
16473..16477|
16484..16488|
16505..16509|
16519..16526|
16539..16540|
16548..16552|
16563..16570|
16583..16587|
16615..16619|
16646..16653|
16665..16669|
16676..16680|
16697..16701|
16713..16732|
16740..16759|
16775..16795|
16807..16811|
16840..16861|
16868..16872|
16889..16893|
16908..16923|
16932..16947|
16967..16987|
16999..17003|
17036..17051|
17060..17064|
17081..17085|
19974..19995|
20007..20025|
20037..20041|
20055..20060|
20072..20076|
20100..20106|
20119..20125|
20132..20139|
20153..20157|
20166..20187|
20199..20217|
20229..20233|
20245..20250|
20264..20268|
20292..20299|
20310..20317|
20324..20332|
20345..20349|
20366..20371|
20405..20409|
20421..20425|
20435..20440|
20456..20460|
20484..20491|
20501..20504|
20506..20509|
20516..20525|
20537..20541|
20558..20563|
20597..20601|
20613..20617|
20626..20630|
20648..20652|
20676..20679|
20681..20684|
20693..20696|
20698..20701|
20708..20712|
20714..20718|
20729..20733|
20750..20755|
20789..20793|
20805..20809|
20816..20821|
20840..20844|
20868..20871|
20873..20877|
20884..20887|
20890..20893|
20900..20904|
20907..20911|
20921..20925|
20942..20947|
20981..20985|
20997..21001|
21006..21011|
21032..21036|
21060..21063|
21066..21070|
21075..21078|
21082..21085|
21092..21096|
21100..21104|
21113..21117|
21134..21139|
21173..21177|
21189..21193|
21197..21201|
21224..21228|
21251..21255|
21259..21262|
21266..21269|
21274..21277|
21284..21288|
21292..21297|
21305..21309|
21326..21331|
21365..21369|
21381..21385|
21387..21391|
21416..21420|
21443..21447|
21452..21455|
21457..21460|
21466..21470|
21476..21480|
21485..21490|
21497..21501|
21518..21523|
21557..21561|
21573..21583|
21608..21612|
21635..21639|
21645..21652|
21658..21662|
21668..21672|
21678..21683|
21689..21693|
21710..21715|
21749..21753|
21765..21769|
21772..21777|
21800..21804|
21827..21831|
21837..21843|
21850..21854|
21860..21864|
21871..21876|
21881..21885|
21902..21907|
21941..21945|
21957..21961|
21966..21971|
21992..21996|
22019..22023|
22030..22034|
22042..22046|
22052..22056|
22064..22069|
22073..22077|
22094..22099|
22133..22137|
22149..22153|
22159..22164|
22184..22188|
22211..22214|
22234..22238|
22244..22248|
22257..22262|
22265..22269|
22286..22291|
22325..22329|
22341..22345|
22353..22358|
22376..22380|
22403..22406|
22426..22430|
22436..22440|
22450..22455|
22457..22461|
22478..22483|
22516..22520|
22533..22537|
22547..22552|
22568..22572|
22595..22598|
22618..22622|
22628..22632|
22643..22647|
22649..22653|
22670..22675|
22694..22695|
22707..22712|
22725..22729|
22740..22745|
22760..22764|
22787..22790|
22810..22814|
22820..22824|
22836..22845|
22854..22875|
22886..22902|
22917..22921|
22934..22939|
22952..22972|
22979..22982|
23002..23006|
23012..23016|
23029..23037|
23046..23067|
23079..23091|
23109..23113|
23127..23133|
23144..23164|
23170..23174|
23195..23198|
23204..23208|
23222..23229|
26122..26136|
26149..26167|
26186..26200|
26214..26230|
26249..26266|
26275..26301|
26311..26330|
26341..26362|
26375..26394|
26406..26425|
26439..26458|
26467..26493|
26502..26507|
26518..26524|
26533..26538|
26550..26556|
26566..26571|
26582..26588|
26598..26602|
26613..26618|
26629..26634|
26670..26675|
26693..26697|
26712..26717|
26725..26730|
26744..26748|
26757..26761|
26776..26781|
26790..26794|
26806..26811|
26821..26825|
26862..26867|
26884..26888|
26905..26909|
26917..26922|
26936..26941|
26948..26952|
26969..26973|
26982..26986|
26999..27003|
27013..27017|
27054..27059|
27075..27080|
27098..27102|
27109..27114|
27129..27133|
27139..27143|
27162..27166|
27174..27178|
27190..27195|
27205..27210|
27246..27251|
27267..27271|
27290..27294|
27301..27306|
27320..27324|
27331..27335|
27354..27358|
27366..27370|
27381..27386|
27398..27405|
27438..27443|
27459..27463|
27482..27486|
27493..27498|
27511..27516|
27523..27527|
27546..27550|
27558..27576|
27592..27603|
27630..27635|
27651..27655|
27674..27678|
27685..27690|
27700..27706|
27715..27719|
27738..27742|
27750..27765|
27788..27799|
27822..27827|
27843..27847|
27866..27870|
27877..27896|
27907..27911|
27930..27934|
27942..27946|
27952..27957|
27985..27994|
28014..28019|
28035..28039|
28058..28062|
28069..28083|
28099..28103|
28122..28126|
28134..28138|
28146..28151|
28181..28187|
28206..28211|
28227..28232|
28249..28254|
28261..28266|
28291..28296|
28313..28318|
28326..28330|
28340..28344|
28375..28380|
28398..28403|
28420..28424|
28441..28445|
28453..28458|
28484..28488|
28505..28509|
28518..28522|
28533..28537|
28568..28572|
28590..28595|
28612..28617|
28632..28636|
28645..28650|
28676..28681|
28696..28700|
28710..28714|
28726..28730|
28760..28764|
28782..28787|
28806..28811|
28822..28827|
28837..28842|
28869..28875|
28885..28891|
28902..28906|
28919..28923|
28932..28932|
28950..28955|
28974..28979|
28999..29017|
29029..29034|
29063..29081|
29094..29098|
29112..29116|
29124..29146|
29166..29171|
29194..29206|
29221..29226|
29258..29270|
29286..29290|
29305..29309|
29316..29334|
29358..29363|
29454..29458|
29647..29651|
29840..29845|
29852..29855|
30033..30048|
30229..30236|
32260..32264|
32281..32285|
32290..32294|
32315..32319|
32322..32326|
32347..32351|
32355..32360|
32377..32381|
32386..32391|
32410..32415|
32421..32444|
32452..32456|
32473..32477|
32482..32487|
32506..32511|
32515..32518|
32539..32542|
32549..32554|
32567..32572|
32579..32584|
32601..32606|
32613..32636|
32644..32648|
32665..32669|
32675..32680|
32698..32702|
32707..32710|
32731..32734|
32742..32747|
32758..32763|
32772..32777|
32792..32797|
32822..32827|
32836..32840|
32857..32861|
32868..32872|
32889..32893|
32899..32902|
32923..32926|
32936..32940|
32949..32953|
32966..32970|
32983..32987|
33013..33018|
33028..33032|
33049..33053|
33060..33065|
33080..33084|
33091..33095|
33115..33118|
33129..33134|
33139..33144|
33159..33164|
33174..33178|
33203..33208|
33220..33224|
33241..33245|
33253..33258|
33271..33276|
33283..33287|
33306..33310|
33322..33327|
33330..33334|
33352..33357|
33365..33369|
33394..33399|
33412..33416|
33433..33437|
33446..33450|
33463..33467|
33475..33479|
33487..33490|
33498..33502|
33516..33525|
33545..33550|
33555..33560|
33585..33589|
33604..33608|
33625..33629|
33639..33643|
33654..33658|
33667..33671|
33678..33683|
33690..33693|
33709..33716|
33738..33743|
33746..33751|
33775..33780|
33796..33800|
33817..33821|
33831..33836|
33845..33849|
33860..33863|
33869..33876|
33882..33885|
33901..33907|
33932..33941|
33966..33970|
33988..33992|
34009..34013|
34024..34028|
34037..34041|
34052..34055|
34060..34063|
34065..34069|
34074..34077|
34092..34101|
34125..34132|
34156..34161|
34180..34184|
34201..34205|
34217..34221|
34228..34232|
34244..34247|
34252..34255|
34258..34261|
34266..34269|
34283..34287|
34289..34294|
34318..34323|
34347..34352|
34372..34376|
34393..34397|
34409..34414|
34419..34423|
34436..34439|
34443..34446|
34450..34454|
34458..34461|
34473..34478|
34483..34488|
34510..34515|
34537..34542|
34564..34568|
34585..34589|
34602..34607|
34611..34614|
34628..34632|
34634..34637|
34643..34647|
34650..34653|
34664..34669|
34676..34681|
34702..34707|
34728..34733|
34757..34761|
34776..34780|
34795..34799|
34802..34806|
34820..34824|
34826..34829|
34836..34840|
34842..34845|
34854..34859|
34869..34874|
34894..34899|
34919..34923|
34949..34954|
34966..34971|
34988..34997|
35012..35020|
35029..35032|
35034..35036|
35045..35050|
35063..35068|
35086..35091|
35109..35114|
35143..35162|
35180..35188|
35205..35211|
35221..35228|
35236..35241|
35256..35261|
35278..35283|
35300..35325|
35337..35351|
35373..35380|
35397..35402|
35414..35420|
35426..35431|
35449..35455|
35470..35475|
35492..35517|
38026..38029|
38218..38228|
38255..38258|
38413..38422|
38447..38450|
38611..38616|
38639..38642|
38701..38708|
38805..38809|
38831..38834|
38893..38900|
38998..39002|
39023..39026|
39085..39092|
39190..39194|
39215..39218|
39280..39281|
39381..39386|
39407..39410|
39572..39577|
39599..39602|
39756..39767|
39783..39802|
39815..39834|
39948..39955|
39975..39994|
40007..40026|
40141..40144|
40167..40186|
40199..40218|
40269..40274|
40333..40336|
40367..40370|
40431..40434|
40460..40467|
40525..40528|
40559..40562|
40621..40628|
40653..40660|
40751..40754|
40813..40820|
40846..40852|
40943..40946|
41006..41011|
41039..41043|
41100..41105|
41135..41138|
41230..41234|
41291..41297|
41327..41330|
41419..41425|
41484..41489|
41519..41522|
41607..41614|
41711..41714|
41799..41799=>(255, 255, 255, 255));
end Font_Sheet;
