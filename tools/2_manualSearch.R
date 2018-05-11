# Opening raw data files
load(file = "0_output/rareComics.Rdata") # Raw data directory
load(file = "0_output/rareSales.Rdata") # Raw data directory

# Generating a random sample for manual search
set.seed(69)
manual.sales <- rare.sales[as.integer(runif(200,1,nrow(rare.sales))),]
write.csv(x = manual.sales, file = "0_output/manualSearch.csv", na = "")

lookUp <- function(keyword) {
  index <- grep(pattern = keyword, x = rare.comics$Issues_Title, ignore.case = TRUE)
  scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))
  return(scope)
}

# 1 friendly neighborhood spider-man
index <- grep(pattern = "spider", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 2 bart simpson comics
index <- grep(pattern = "simpson", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 3 lady death
index <- grep(pattern = "lady", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 4 star wars legacy
index <- grep(pattern = "wars", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 5 marvel zombies army of darkness
index <- grep(pattern = "zombies", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 6 marvel adventures avengers
index <- grep(pattern = "avengers", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 7 battlestar galactica
index <- grep(pattern = "galactica", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 8 x-factor
index <- grep(pattern = "factor", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 9 coven vol. 2
index <- grep(pattern = "coven", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 10 boneyard
index <- grep(pattern = "boneyard", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 11 batman dark victory
index <- grep(pattern = "batman", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 12 g.i. joe
index <- grep(pattern = "joe", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 13 daredevil ninja
index <- grep(pattern = "daredevil", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 14 supergirl
index <- grep(pattern = "super", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 15 avengers celestial quest
index <- grep(pattern = "avengers", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 16 betty
index <- grep(pattern = "betty", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 17 teen titans titans around world
index <- grep(pattern = "titans", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 18 batman rip deluxe edition hc
index <- grep(pattern = "batman", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 19 orson scott cards speaker for dead
index <- grep(pattern = "orson", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 20 buffy the vampire slayer season 8 vol. 2 no future for you
index <- grep(pattern = "buffy", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 21 infinity abyss
index <- grep(pattern = "abyss", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 22 johnny the homicidal maniac (o/a)
index <- grep(pattern = "johnny", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 23 mighty thor
index <- grep(pattern = "thor", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 24 walking dead vol. 13 too far gone
index <- grep(pattern = "walking", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 25 swamp thing
index <- grep(pattern = "swamp", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 26 walking dead vol. 15 we find ourselves
index <- grep(pattern = "walking", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 27 world of warcraft bloodsworn
index <- grep(pattern = "warcraft", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 28 black widow
index <- grep(pattern = "widow", x = rare.comics$Issues_Title, ignore.case = TRUE)
scope <- subset(x = rare.comics[index,], select = c("ID", "Issues_Title", "FullIssue", "Publisher", "Cover.Price", "CoverDate", "ComicType"))

# 29 archie double digest
scope <- lookUp("archie")

# 30 ragemoor
scope <- lookUp("ragemoor")

# 31 star wars jedi council
scope <- lookUp("star")

# 32 shadow show
scope <- lookUp("show")

# 33 lucifer
scope <- lookUp("lucifer")

# 34 teen titans go
scope <- lookUp("titans")

# 35 ultimate marvel team up spider-man & iron man
scope <- lookUp("team")

# 36 yu yu hakusho vol. 8
scope <- lookUp("hakusho")

# 37 ultraman tiga
scope <- lookUp("ultraman")

# 38 kevin smith kato
scope <- lookUp("kato")

# 39 incorruptible
scope <- lookUp("incorruptible")

# 40 lady death
scope <- lookUp("lady")

# 41 angela asgards assassin
scope <- lookUp("angela")

# 42 walking dead vol.13 too far gone
scope <- lookUp("walking")

# 43 mega man
scope <- lookUp("mega")

# 44 jla annual
scope <- lookUp("jla")

# 45 halo blood line
scope <- lookUp("halo")

# 46 love & rockets vol. 2
scope <- lookUp("rockets")

# 47 chastity love bites
scope <- lookUp("chastity")

# 48 eightball
scope <- lookUp("eightball")

# 49 ghost in shell stand alone complex vol. 4
scope <- lookUp("ghost")

# 50 talon
scope <- lookUp("talon")

# 51 batman beyond
scope <- lookUp("batman")

# 52 real ghostbusters omnibus vol. 1
scope <- lookUp("ghostbusters")

# 53 wolverine standard cvr
scope <- lookUp("wolverine")

# 54 "marvel's greatest comics: agents of atlas"
scope <- lookUp("agents")

# 55 astonishing x-men
scope <- lookUp("astonishing")

# 56 black panther
scope <- lookUp("panther")

# 57 green lantern new guardians
scope <- lookUp("guardians")

# 58 archer & armstrong
scope <- lookUp("armstrong")

# 59 thundercats enemys pride
scope <- lookUp("thunder")

# 60 archie
scope <- lookUp("archie")

# 61 ms marvel vol. 4 monster smash
scope <- lookUp("marvel")

# 62 robo dojo
scope <- lookUp("dojo")

# 63 scooby doo
scope <- lookUp("doo")

# 64 kabuki agents scarab
scope <- lookUp("kabuki")

# 65 bad kitty
scope <- lookUp("kitty")

# 66 ultimate marvel sampler
scope <- lookUp("ultimate")

# 67 trinity of sin pandora
scope <- lookUp("trinity")

# 68 super hero squad
scope <- lookUp("squad")

# 69 eeek vol. 1
scope <- lookUp("eeek")

# 70 kingdom come new ptg
scope <- lookUp("kingdom")

# 71 thor son of asgard
scope <- lookUp("thor")

# 72 walking dead hc vol. 2
scope <- lookUp("walking")

# 73 star wars darth maul
scope <- lookUp("darth")

# 74 incredible hulk
scope <- lookUp("hulk")

# 75 scooby doo
scope <- lookUp("scooby")

# 76 street fighter legends ibuki vol. 3
scope <- lookUp("street")

# 77 xxxholic rei vol. 1
scope <- lookUp("xxxholic")

# 78 batman: legends of the dark knight
scope <- lookUp("batman")

# 79 castle waiting vol. 2
scope <- lookUp("castle")

# 80 dragonball vol. 2
scope <- lookUp("dragon")

# 81 morgana book 1 hc heavens gate
scope <- lookUp("morgana")

# 82 sam & twitch writer
scope <- lookUp("twitch")

# 83 outsiders
scope <- lookUp("outsiders")

# 84 superman wonder woman hc vol. 1 power couple
scope <- lookUp("wonder")

# 85 i love trouble
scope <- lookUp("trouble")

# 86 raijin comics
scope <- lookUp("raijin")

# 87 walking dead vol. 14 no way out
scope <- lookUp("walking")

# 88 locke & key hc vol. 5 clockworks
scope <- lookUp("locke")

# 89 hero squared
scope <- lookUp("squared")

# 90 no need for tenchi part five
scope <- lookUp("tenchi")

# 91 thor by j michael straczynski vol. 1
scope <- lookUp("straczynski")

# 92 spider-man blue
scope <- lookUp("spider")

# 93 supergirl
scope <- lookUp("super")

# 94 sandman mystery theatre vol. 8
scope <- lookUp("mystery")

# 95 battlestar galactica
scope <- lookUp("battlestar")

# 96 dragon ball z
scope <- lookUp("dragon")

# 97 "off handbk marvel universe fantastic four 2005"
scope <- lookUp("off")

# 98 kick ass
scope <- lookUp("kick")

# 99 john carpenters snake plissken
scope <- lookUp("carpenter")

# 100 phantom
scope <- lookUp("phantom")

# 101 knights of dinner table
scope <- lookUp("knights")

# 102 superman redemption
scope <- lookUp("superman")

# 103 batman
scope <- lookUp("batman")

# 104 wolverine
scope <- lookUp("wolverine")

# 105 starcraft ghost academy vol. 2
scope <- lookUp("craft")

# 106 ex machina
scope <- lookUp("machina")

# 107 battle chasers
scope <- lookUp("battle")

# 108 americas best comics
scope <- lookUp("best")

# 109 g.i. joe
scope <- lookUp("joe")

# 110 gross point
scope <- lookUp("gross")

# 111 "ultimate fantastic four vol. 5 crossover"
scope <- lookUp("fantastic")

# 112 fear itself home front
scope <- lookUp("fear")

# 113 flash
scope <- lookUp("flash")

# 114 uncanny x-men vol. 2 broken
scope <- lookUp("uncanny")

# 115 black beetle no way out hc vol. 1
scope <- lookUp("beetle")

# 116 bazooka jules
scope <- lookUp("bazooka")

# 117 after the cape
scope <- lookUp("cape")

# 118 star wars knights of the old republic
scope <- lookUp("star")

# 119 bprd hell on earth
scope <- lookUp("hell")

# 120 blue beetle shellshocked
scope <- lookUp("beetle")

# 121 marvel 70th anniversary
scope <- lookUp("marvel")

# 122 banzai girl
scope <- lookUp("banzai")

# 123 walking dead hc vol. 8
scope <- lookUp("walking")

# 124 path
scope <- lookUp("path")

# 125 amazing fantasy
scope <- lookUp("amazing")

# 126 star wars blood ties vol. 2 boba fett is dead
scope <- lookUp("star")

# 127 dragonball vol. 1
scope <- lookUp("dragon")

# 128 betty
scope <- lookUp("betty")

# 129 genus
scope <- lookUp("genus")

# 130 avengers vs x-men avx
scope <- lookUp("avengers")

# 131 walking dead vol. 9 here we remain
scope <- lookUp("walking")

# 132 mars attacks
scope <- lookUp("mars")

# 133 human defense corps
scope <- lookUp("corp")

# 134 infinity abyss
scope <- lookUp("abyss")

# 135 terry moores echo
scope <- lookUp("moore")

# 136 one piece vol. 25
scope <- lookUp("piece")

# 137 preacher vol. 2 until the end of the world new edition
scope <- lookUp("preacher")

# 138 danger girl mayday
scope <- lookUp("danger")

# 139 star wars knights of the old republic
scope <- lookUp("star")

# 140 simpsons comics colossal compendium vol. 1
scope <- lookUp("simpsons")

# 141 sergio aragones actions speak
scope <- lookUp("sergio")

# 142 "ax vol. 1 collection of alternative manga"
scope <- lookUp("ax")

# 143 wolverine prem death of wolverine
scope <- lookUp("wolverine")

# 144 fashion beast
scope <- lookUp("fashion")

# 145 lost boys reign of frogs
scope <- lookUp("lost")

# 146 iron man extremis hc dm var ed
scope <- lookUp("iron")

# 147 detective comics
scope <- lookUp("detective")

# 148 bleach vol. 53
scope <- lookUp("bleach")

# 149 walking dead vol. 2 miles behind us
scope <- lookUp("walking")

# 150 betty & veronica
scope <- lookUp("veronica")

# 151 activity vol. 2
scope <- lookUp("activity")

# 152 superman
scope <- lookUp("superman")

# 153 jla incarnations
scope <- lookUp("jla")

# 154 batgirl
scope <- lookUp("bat")

# 155 sonic the hedgehog
scope <- lookUp("sonic")

# 156 walking dead vol. 2 miles behind us
scope <- lookUp("walking")

# 157 "battlestar galactica cylon apocalypse"
scope <- lookUp("galactica")

# 158 a g super erotic anthology
scope <- lookUp("[a|g]")

# 159 g.i. joe frontline
scope <- lookUp("joe")

# 160 invincible
scope <- lookUp("invincible")

# 161 buffy the vampire slayer
scope <- lookUp("buffy")

# 162 dmz vol. 1 on the ground
scope <- lookUp("dmz")

# 163 generation x
scope <- lookUp("generation")

# 164 "dungeons & dragons tempests gate"
scope <- lookUp("dungeons")

# 165 spider-girl
scope <- lookUp("spider")

# 166 walking dead vol. 17 something to fear
scope <- lookUp("walking")

# 167 perhapanauts vol. 1
scope <- lookUp("perhapanaut")

# 168 new teen titans vol. 1
scope <- lookUp("titans")

# 169 red hood lost days
scope <- lookUp("hood")

# 170 hawkgirl
scope <- lookUp("girl")

# 171 x-men dark phoenix saga
scope <- lookUp("men")

# 172 dark tower gunslinger born prem hc
scope <- lookUp("gunslinger")

# 173 muppet show
scope <- lookUp("muppet")

# 174 pvp
scope <- lookUp("pvp")

# 175 dfe star wars infinities a new hope
scope <- lookUp("star")

# 176 spawn (o/a)
scope <- lookUp("spawn")

# 177 strange adventures of h.p. lovecraft
scope <- lookUp("strange")

# 178 hitman
scope <- lookUp("hitman")

# 179 countdown 7 days vol. 1
scope <- lookUp("count")

# 180 harley quinn vengeance unlimited
scope <- lookUp("harley")

# 181 y the last man vol. 1 unmanned
scope <- lookUp("last")

# 182 annihilation conquest star lord
scope <- lookUp("annihilation")

# 183 walking dead vol. 8 made to suffer
scope <- lookUp("walking")

# 184 garth ennis battlefields dear billy
scope <- lookUp("battlefield")

# 185 sword
scope <- lookUp("sword")

# 186 batman chronicles
scope <- lookUp("batman")

# 187 sabrina the teenage witch
scope <- lookUp("sabrina")

# 188 jla
scope <- lookUp("jla")

# 189 adventure time vol. 2
scope <- lookUp("adventure")

# 190 flash
scope <- lookUp("flash")

# 191 james obarrs original sins
scope <- lookUp("obarr")

# 192 sandman presents thessaliad
scope <- lookUp("sandman")

# 193 x-factor
scope <- lookUp("factor")

# 194 stan lee meets dr doom
scope <- lookUp("lee")

# 195 x-men
scope <- lookUp("men")

# 196 robert e howard hawks of outremer
scope <- lookUp("robert")

# 197 captain america by ed brubaker prem hc vol. 4
scope <- lookUp("america")

# 198 y the last man vol. 9 motherland
scope <- lookUp("the last man")

# 199 thunderbolts
scope <- lookUp("thunder")

# 200 batman dark victory
scope <- lookUp("batman")