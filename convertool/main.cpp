#include "tooltools.hpp"
#include <fstream>


string malestr = "M";
string femalestr = "F";
string czfemalestr = "Z";

string unvacclabel = "_unvaccinated";
string uninflabel = "_uninfected";
string noimmunitylabel = "_noimmunity";
string nodccilabel = "_nodcci";

string dcci2str(int dcci)
{
    assert(dcci>=0);
    if(dcci==0)
        return "0";
    if(dcci==1)
        return "1";
    return "2+";
}

string gender2str(bool male)
{
    return male ? "M" : "F";
}

enum eagegroups { v0011, v1215, v1617, v1824, v2529, v3034, v3539, v4044,v4549, v5055,v5559, v6054,v6559, v7074,v7579, v80plus, enumagegroups };




constexpr unsigned lastage = 100;

reldate dateoffirstczsohalfyear = date2int("2020-01-01");
vector<reldate> dccidates =
{
    date2int("2020-01-01"),
    date2int("2021-01-01"),
    date2int("2022-01-01"),
    date2int("2023-01-01")
};

unsigned numpeopleofage(unsigned age, bool male, unsigned halfyear)
{
#include "ages.inc"

    assert(age <= lastage);
    assert(menincz.size()==lastage+1);
    assert(womenincz.size()==lastage+1);
    return male ? menincz[age][min(halfyear,static_cast<unsigned>(menincz[age].size()-1))]
            : womenincz[age][min(halfyear,static_cast<unsigned>(womenincz[age].size()-1))];
}

string grouplabel(unsigned vk)
{
    static vector<string> g{"0-11", "12-15","16-17","18-24","25-29","30-34","35-39",
    "_40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"};
    assert(vk<g.size());
    return g[vk];
}

enum eseverity { enotsevereorunknown, esymptoms, ehospital, enumseverities };


string severitylabel(eseverity s)
{
    switch(s)
    {
    case enotsevereorunknown:
        return "_no_or_unknown_severity";
        break;
    case esymptoms:
        return "symptoms";
        break;
    case ehospital:
        return "hospitalized";
        break;
    default:
        throw "unknown severity";
    }
}

string age2group(unsigned age)
{
    static vector<string> g = {"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"12-15",	"12-15",	"12-15",	"12-15",	"16-17",	"16-17",	"18-24",	"18-24",	"18-24",	"18-24",	"18-24",	"18-24",	"18-24",	"25-29",	"25-29",	"25-29",	"25-29",	"25-29",	"30-34",	"30-34",	"30-34",	"30-34",	"30-34",	"35-39",	"35-39",	"35-39",	"35-39",	"35-39",	"_40-44",	"_40-44",	"_40-44",	"_40-44",	"_40-44",	"45-49",	"45-49",	"45-49",	"45-49",	"45-49",	"50-54",	"50-54",	"50-54",	"50-54",	"50-54",	"55-59",	"55-59",	"55-59",	"55-59",	"55-59",	"60-64",	"60-64",	"60-64",	"60-64",	"60-64",	"65-69",	"65-69",	"65-69",	"65-69",	"65-69",	"70-74",	"70-74",	"70-74",	"70-74",	"70-74",	"75-79",	"75-79",	"75-79",	"75-79",	"75-79",	"80+"	};
    if(age >= g.size())
       return g[g.size()-1];
    else
       return g[age];
}

string fourage2group(unsigned age)
{
    if(age < 25)
        return "0-24";
    else if(age < 40)
        return "25-39";
    else if(age < 65)
        return "_40-64";
    else
        return "65+";
}

unsigned age2groupnum(unsigned age)
{
    static vector<unsigned> v={	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	1,	1,	2,	2,	3,	3,	3,	3,	3,	3,	3,	4,	4,	4,	4,	4,	5,	5,	5,	5,	5,	6,	6,	6,	6,	6,	7,	7,	7,	7,	7,	8,	8,	8,	8,	8,	9,	9,	9,	9,	9,	10,	10,	10,	10,	10,	11,	11,	11,	11,	11,	12,	12,	12,	12,	12,	13,	13,	13,	13,	13,	14,	14,	14,	14,	14,	15	};

    if(age >= v.size())
       return v[v.size()-1];
    else
       return v[age];
}


unsigned fouragegroup(unsigned a)
{
    if(a < 25)
        return 0;
    else if(a < 40)
        return 1;
    else if(a < 65)
        return 2;
    else
        return 3;
}


// pasted from vakciny_ciselnik
struct vaccinerecord
{	string code;
    string description;
    int numshots;
    bool consorfull;
    bool consorboost;
    string abbrev;
};

vector<vaccinerecord> vaccines={
    {"CO01","Comirnaty",2,false,false,"P"},
    {"CO02","SPIKEVAX",2,false,false,"M"},
    {"CO03","VAXZEVRIA",2,false,false,"A"},
    {"CO04","COVID-19 Vaccine Janssen",1,false,false,"J"},
    {"CO06","Comirnaty 5-11",2,false,false,"P511"},
    {"CO07","Nuvaxovid",2,false,false,"N"},
    {"CO10","Covovax",2,false,false,"C"},
    {"CO11","Sinopharm",2,false,false,"string abbrev"},
    {"CO12","Sinovac",2,false,false,"V"},
    {"CO13","COVAXIN",2,false,false,"X"},
    {"CO14","Covishield",2,false,false,"S"},
    {"CO08","Comirnaty Original/Omicron BA.1",2,false,false,"PO1"},
    {"CO09","Comirnaty Original/Omicron BA.4/BA.5",2,false,false,"PO4"},
    {"CO15","Spikevax bivalent Original/Omicron BA.1",2,false,false,"MB"},
    {"CO05","Sputnik V",2,false,false,"U"},
    {"CO16","Comirnaty 6m-4",2,false,false,"P6"},
    {"CO17","Valneva",2,false,false,"N"}
};

unsigned eunknownvaccine = vaccines.size();
unsigned numvaccines = vaccines.size();

unsigned vacc2vacc(string code)
{
    for(unsigned i=0; i<numvaccines; i++)
        if(code==vaccines[i].code)
            return i;
    return eunknownvaccine;
}

struct variantrecord
{
  string codeindata;
  string codeinoutput;
  string codeincovariate;
  string commandlineid;
  string supcategory;
  vector<string> startdates90;
  vector<string> enddates90;
};

enum evariant {evNA,evDelta,evBA12,evBA45, evOmicron, evAlpha, evBeta, evEarly, ev2023, enumvariants};

vector<variantrecord> variants
{
    { "", "NA_Other","NA","n","NA",{},{}},
    { "Delta", "Delta","Delta", "d", "DELTA",{"2021-07-19"}, {"2021-12-20"}},
    { "Omikron BA.1/2", "BA12", "BA12", "x", "OMICRON",{"2022-01-31"},{"2022-05-23"}},
    { "Omikron BA.4/5", "BA45", "BA45", "y", "OMICRON",{"2022-08-01"},{"2022-10-24"}},
    { "Omikron", "Omicron", "Omicron", "o", "OMICRON",{"2022-05-24"},{"2022-07-31"}},
    { "Alfa", "Alpha", "Alpha", "a", "ALPHA",{"2021-02-15"},{"2021-06-21"}},
    { "Beta", "Beta", "Beta", "b", "NA",{},{}},
    { "$$$" /*never happens */, "Early", "Early", "e", "EARLY",{"2020-01-01"},{"2021-01-04"}},
    { "$$$" /*never happens */, "2023", "2023", "z", "OMICRON",{"2022-10-25"},{"2023-12-31"}}
};

auto navariant = 0;

struct preprocessparams
{
    /// least date possible (others reported as errors
    int zerodate = date2int("2020-01-01");

    int vaccstartdate = date2int("2021-01-01");
    int boosterstartdate = date2int("2021-11-01");
    int secboosterstartdate = date2int("2022-07-18");
    int reinfstartdate = date2int("2020-04-01");

    /// sensitivity parameters
    bool discernsecboosts = false;

    /// in number of covs
    int secboostlatedatetreshold = 9*31;
    bool discern23boost = false;

    bool checkzeroimmunitycovs = false;


    /// delay after the first, second, third and fourth shot takes effect
    int regulardelay = 14;
    int boostdelay = 7;

    /// lenght of the window after infection within which hospitalization is examined
    /// (after this limit, the subject is censored in h analysis TBD CHECK THIS!!)
    int outcomelimit = 15;

    int safetydate = date2int("2023-03-01");

//    /// time delay during which new positive tests are not regarded as reinfections
//    int infectionsgap = 60;

    /// number of the last postinf covariate (0 = not infected, 1 - first ...)
    int numinfcovariates = 6;

    /// duration of 1st  postinf covariate (event not regarded as reinfections)
    int firstcovreinfduration = 61;

    /// duration of 2nd ... \p (naxinfstatus-1)-th - postinf covariate (the last one lasts till infty)
    int covreinfduration = 61;

    /// durations of the other than the last postvacc covariates
    int regularcovvaccduration = 61;


    /// number of covariates following partial (including the last one)
    int numpartialcovs = 2;

    /// number of covariates following full vacc (including the last one)
    int numfinalcovs = 4;

    int numboostercovs = 3;

    int numsecboostercovs = 2;

    int maxvacccovs = 10;
    int maxinfcovs = 10;


    /// time after the test for the treatment to be assigned to the test
    ///
    int hosplimit = 30;


    /// if true, then only four age categories are used instead of the finer division
    bool fourages = false;

    /// if true then vaccines are grouped in the VaccStatus field
    bool groupvaccs = true;

    // not used in the present time
    bool groupvariants = false;

    /// using supcategory rather than codeincovariate
    bool generalvariants = false;


    ///
    bool useintervalsofdominance = false;

    bool ignorehospcovidflag = false;

    bool discern45booster = false;
    string abbrev45 = "PO4";

    /// Inputs that should be provided

    /// start of the sutedy (time 0)
    int firstdate;

    /// horzion of the study (time T - once hosps are determined by corresponding
    /// infections, only events (infections) hosplimit is taken from T to get new
    /// horizon (so that the fact of hosp could be decided, similarly deaths and deathlimit)
    int lastdate;


    ///
    unsigned everyn = 1;

    ///
    vector<bool> conditioning = vector<bool>(enumvariants,true);
    vector<bool> isoutcome = vector<bool>(enumvariants,true);
//    vector<bool> compared0 = vector<bool>(enumvariants,false);
//    vector<bool> compared1 = vector<bool>(enumvariants,false);
};

/*

void addto(vector<string>& labels, vector<unsigned>& counts, const string lbl)
{
    for(unsigned i=0; i<labels.size(); i++)
    {
        if(labels[i] == lbl)
        {
            counts[i]++;
            return;
        }
    }
    assert(counts.size());
    counts[counts.size()-1]++;
}*/


enum o2rmodes { einfections, eseriouscovidproxy, ehospitalization, elongcovidevent, elongcovidinfection,
                ecomparison, enumo2rmodes };

vector<string> mdelabels = { "infections", "seriouscovidproxy",
                             "hospitalization", "longcovidevent",
                             "longcovidinfection","comparison" };

struct covstatrecord
{
    string label;
    int n = 0;
    int events = 0;
};

struct covstat
{
    vector<covstatrecord> infprior;
    vector<covstatrecord> vaccstatus;
    vector<covstatrecord> immunity;
    vector<covstatrecord> dcci;
    vector<covstatrecord> agegroup;
};

void output(string heading, ostream& o,
            const vector<covstatrecord>& s, bool filter,
            unsigned& ncovs, unsigned& nevents)
{
    ncovs = nevents = 0;
    o << heading << ",N,events" << endl;
    for(unsigned i=0; i<s.size(); i++)
    {
        if(!filter || s[i].events > 0)
        {
            o << s[i].label << "," << s[i].n << ","
              << s[i].events << endl;
            nevents += s[i].events;
            ncovs++;
        }
    }
    o << endl;
}

void recordcov(string cov, int event, vector<covstatrecord>& s)
{
    for(unsigned i=0; i<s.size(); i++)
    {
        if(cov == s[i].label)
        {
            s[i].n++;
            s[i].events += event;
            return;
        }
    }
    s.push_back({cov,1,event});
}

covstatrecord findcov(const string cov, const vector<covstatrecord>& s)
{
    for(unsigned i=0; i<s.size(); i++)
    {
        if(cov == s[i].label)
            return s[i];
    }
    cout << "Cannot find covariate label " << cov << " in list" << endl;
    throw;
}

void ockodata2R(csv<';'>& data, string outputlabel,
                bool dostat,
                covstat& stat,
                o2rmodes mode,
                unsigned minage, unsigned maxage,
                preprocessparams ppp,
                bool omitczso) // rozdelane
{
    if(ppp.safetydate <= ppp.lastdate)
        throw "Safety date < last date";

    string output = dostat ? (outputlabel + "_full.csv") : outputlabel;

    cout << "Writing to " << output << endl;

    ofstream o(output);
    if(!o)
    {
        cout << "Cannot open " + output << endl;
        throw;
    }

    ofstream oe(output + ".events.csv");
    if(!oe)
    {
        cout << "Cannot open " + output + ".events.csv" << endl;
        throw;
    }

    ofstream ou(output + ".unprocessed.csv");
    if(!ou)
    {
        cout << "Cannot open " + output + ".unprocessed.csv" << endl;
        throw;
    }

    ou << "error,desc";


/*    ofstream tex(output + ".tex");
    if(!tex)
    {
       cout << "Cannot open " + output + ".tex" << endl;
       throw;
    }*/

    ostringstream header;

    header << "ID,T1,T2,Infected,Hospitalized,SeriousCovidProxy,LongCovid,DeadByCovid,DeadByOther,";
    header << "VariantOfInf,InfPrior,VaccStatus,Immunity,DCCI,Age,AgeGr,Sex";
    header << ",InfPriorTime,LastVaccTime";

    o << header.str() << endl;
    oe << header.str();


    enum egender { emale, efemale, enumgenders };

    enum evaccorder { novacc, firstdose, finaldose, firstbooster, secbooster, enumvaccorders, eunknownvaccorder = enumvaccorders };




    cout << "Importing from input, total " << data.r()-1 <<
         " records." << endl;

    static vector<string> labels = {
       "PripadId",   "ID", "NovyHash",	"infekce",	"pohlavi",	"vek",	"Kraj_bydliste",	"ORP_Bydliste",	"Datum_pozitivity",	"DatumVysledku",	"Vylecen",	"Umrti",	"symptom",	"typ_testu",	"PrvniDavka",	"DruhaDavka",	"Ukoncene_ockovani",	"Extra_davka",	"Druha_extra_davka",	"OckovaciLatkaKod1",	"OckovaciLatkaKod2",	"OckovaciLatkaKod3",	"OckovaciLatkaKod4", "PrimPricinaHospCOVID",
       "bin_Hospitalizace",	"min_Hospitalizace",	"dni_Hospitalizace",	"max_Hospitalizace",	"bin_JIP",	"min_JIP",	"dni_JIP",	"max_JIP",	"bin_STAN",	"min_STAN",	"dni_STAN",	"max_STAN",	"bin_Kyslik",	"min_Kyslik",	"dni_Kyslik",	"max_Kyslik",	"bin_HFNO",	"min_HFNO",	"dni_HFNO",	"max_HFNO",	"bin_UPV_ECMO",	"min_UPV_ECMO",	"dni_UPV_ECMO",	"max_UPV_ECMO",	"Mutace",	"DatumUmrtiLPZ", "Long_COVID",
       "ODB_Long_COVID","kraj_icz_Long_COVID","kraj_pacient_Long_COVID","DCCI_r2010","DCCI_r2011","DCCI_r2012","DCCI_r2013","DCCI_r2014","DCCI_r2015","DCCI_r2016","DCCI_r2017","DCCI_r2018","DCCI_r2019","DCCI_r2020","DCCI_r2021","DCCI_r2022"

    };


    enum elabels {PripadId, ID,	NovyHash, infekce,	pohlavi,	vek,	Kraj_bydliste,	ORP_Bydliste,	Datum_pozitivity,	DatumVysledku,	Vylecen,	Umrti,	symptom,	typ_testu,	PrvniDavka,	DruhaDavka,	Ukoncene_ockovani,	Extra_davka,	Druha_extra_davka,	OckovaciLatkaKod1,	OckovaciLatkaKod2,	OckovaciLatkaKod3,	OckovaciLatkaKod4,	PrimPricinaHospCOVID, bin_Hospitalizace,	min_Hospitalizace,	dni_Hospitalizace,	max_Hospitalizace,	bin_JIP,	min_JIP,	dni_JIP,	max_JIP,	bin_STAN,	min_STAN,	dni_STAN,	max_STAN,	bin_Kyslik,	min_Kyslik,	dni_Kyslik,	max_Kyslik,	bin_HFNO,	min_HFNO,	dni_HFNO,	max_HFNO,	bin_UPV_ECMO,	min_UPV_ECMO,	dni_UPV_ECMO,	max_UPV_ECMO,	Mutace,	DatumUmrtiLPZ, Long_COVID,
                  ODB_Long_COVID,kraj_icz_Long_COVID,kraj_pacient_Long_COVID,DCCI_r2010,
                  DCCI_r2011,DCCI_r2012,DCCI_r2013,DCCI_r2014,DCCI_r2015,DCCI_r2016,DCCI_r2017,DCCI_r2018,DCCI_r2019,
                  firstDCCI = DCCI_r2019,
                  DCCI_r2020,DCCI_r2021,DCCI_r2022, lastDCCI = DCCI_r2022,
                  enumlabels};

    for(unsigned i=0; i<enumlabels; i++)
    {
        oe << "," << labels[i];
        ou << "," << labels[i];
    }
    oe << endl;
    ou << endl;

    bool openerror = false;
    if(data.c(0) != enumlabels)
    {
        cout << "Wrong number of columns. Expected "
             << enumlabels << " found " << data.c(0) << endl;
        openerror = true;
    }
    else
    {
        for(unsigned i=0; i<enumlabels; i++)
        {
            if(data(0,i) != labels[i])
            {
                cout << "Wrong format: " << labels[i]
                     << " expected, " << data(0,i) << " found."
                     << endl;
                openerror = true;
            }
        }
    }

    if(openerror)
    {
        cout << "Expected header is:" << endl;
        for(unsigned i=0; i<enumlabels; i++)
            cout << " " << labels[i];
        cout << endl;
        cout << "Actual header is:" << endl;
        for(unsigned i=0; i<enumlabels; i++)
            cout << " " << data(0,i);
        cout << endl;
        throw;
    }


    vector<unsigned> men(lastage+1,0);
    vector<unsigned> women(lastage+1,0);

const int numweeks = 200;


    vector<string> throwedvariants;

    struct variantstatrecord
    {
        variantstatrecord(unsigned i) : infections(0), outcomes(0), calendar(i,0) {}
        unsigned infections;
        unsigned outcomes;
        vector<unsigned> calendar;
    };

    vector<variantstatrecord> variantsfound(enumvariants, ppp.lastdate-ppp.zerodate+1);

    cout << "Processing records from input, excluding inconsistent records:" << endl;

//    vector<vector<unsigned>> variants(ppp.lastdate-ppp.zerodate+1,vector<unsigned>(enumvariants,0));

    struct vaccinationrecord {
                      reldate t = maxreldate;
                      evaccorder vaccorder = eunknownvaccorder;
                      unsigned vac = eunknownvaccine;
     };


    struct infectionrecord
    {
        int t;
        unsigned variant;
        unsigned variantunadjusted;
        bool hospitalized = false;
        bool seriouscovidproxy = false;
        bool longcovid = false;
    };

vector<statcounter> lclengths(numweeks);
vector<statcounter> lccounts(numweeks);

    unsigned firstnext;
    unsigned maxid = 0;
    int T = ppp.lastdate - ppp.firstdate;
    unsigned outputcounter = 0;
    unsigned peopleexported = 0;
    for(unsigned i=1; i<data.r(); i=firstnext )
    {

        reldate deathcoviddate = maxreldate;
        reldate deathotherdate = maxreldate;

        vector<infectionrecord> infections;

        vector<vaccinationrecord> vaccinations;

        vector<int> dccis;

// when used, all these macros have to be enclused in braces

#define REPORT(X,Y) for(unsigned j=0; j<is.size(); j++) \
        { \
            ou << "\"" << X << "\"," << "\"" << Y << "\""; \
            for(unsigned k=0; k<enumlabels; k++) \
                ou << "," << data(is[j],k); \
            ou << endl; \
        }

//cout << "a" << endl;
        string idstr = data(i,ID);
        unsigned id;
        try
        {
            id = stoul(idstr);
            if(id > maxid)
                maxid = id;
        }
        catch (...)
        {
            cout << "Cannot convert ID '" << idstr << "' to unsigned" << endl;
            throw;
        }
        constexpr int norecord = -1;
        vector<int> is;
        for(unsigned j=i; j<data.r(); j++)
        {
            string newidstr = data(j,ID);
            if(newidstr!=idstr)
                break;
            is.push_back(norecord);
        }
//        bool error = false;
        bool emptyfound = false;
//cout << "b" << endl;

        for(unsigned j=i; j<i+is.size(); j++)
        {
            if(data(j,infekce)=="")
            {
                if(emptyfound)
                {
                    cout << "row " << j << " two empty lines" << endl;
                    throw;
                }
                if(j!=i)
                {
                    cout << "row" << j << "empty line on non-first place" << endl;
                    throw;
                }
                emptyfound = true;
                is[j-i]=j;
            }
            else
            {
                int infindex = data(j,infekce)[0] - '1';
                if(infindex < 0 || infindex >= is.size())
                {
                    cout << "field infekce = " << data(j,infekce) << endl;
                    throw;
                }
                is[infindex] = j;
            }
        }
        for(unsigned k=0; k<is.size(); k++)
            if(is[k] == norecord)
            {
                cout << "invalid record group starting at " << i << endl;
                throw;
            }

        bool throwthisid = false;
        string errorstring = "";
        string errordetail = "";

    #define THROW(X,W) { throwthisid = true; REPORT(X,"") ; W;  }
    #define THROWS(X,Y,W) { ostringstream o; o<< Y; REPORT(X,o.str()); throwthisid = true; W; }
    #define GETDATE(Y,X,W) { bool error; Y=date2int(X,ppp.zerodate, ppp.safetydate, error, errorstring,errordetail)-ppp.firstdate; if(error) { throwthisid = true; REPORT(errorstring, errordetail)  W; }   }
    #define GETVACC(V,S,L,W) { V = vacc2vacc(data(j,L)); S = vaccines[V].numshots == 1; if(V==eunknownvaccine) { throwthisid = true; errorstring = "7nknonw vaccine code.", errordetail = data(j,L); REPORT(errorstring, errordetail); W; } }


        firstnext = i + is.size();
        if(++outputcounter % ppp.everyn )
            continue;


        bool firstrecord = true;
        bool isdead = false;
        reldate disttofirst = maxreldate;
        unsigned relevantrecord;

        reldate longcoviddate = maxreldate;


        unsigned oldi=i;
        for(unsigned k=0; k<is.size(); k++)
        {
            if(isdead)
            {
                THROW("Extra line after a death record",break);
            }
            unsigned j = is[k];

            reldate infdate;
            string infdatestr = data(j,Datum_pozitivity);

            if(infdatestr == "")
                infdate = maxreldate;
            else
            {
                GETDATE(infdate,infdatestr,break);
                if(emptyfound)
                {
                    THROW("Empty infection order but non-empty date", break)
                }
            }


            reldate relevantvaccdate = maxreldate;
            if(firstrecord)
            {
                reldate firstvaccdate;
                string firstvaccdatestr = data(j,PrvniDavka);
                if(firstvaccdatestr == "")
                    firstvaccdate = maxreldate;
                else
                {
                    GETDATE(firstvaccdate,firstvaccdatestr, break);
                    bool s;
                    unsigned v;
                    GETVACC(v,s,OckovaciLatkaKod1,break);
                    assert(v<vaccines.size());
                    vaccinations.push_back(
                                 { /* t */ firstvaccdate  + ppp.regulardelay ,
                                   /* vaccorder */s ? finaldose : firstdose,
                                   /* vac */ v
                                  });
                }

                relevantvaccdate = firstvaccdate;

                reldate secvaccdate;
                string secvaccdatestr = data(j,DruhaDavka);
                if(secvaccdatestr == "")
                    secvaccdate = maxreldate;
                else
                {
                    if(firstvaccdate == maxreldate)
                    {
                        THROW("First shot missing", break);
                    }
                    GETDATE(secvaccdate,secvaccdatestr,break);
                    bool s;
                    unsigned v;
                    GETVACC(v,s,OckovaciLatkaKod2,break);
                    assert(v<vaccines.size());
                    if(s)
                    {
                        THROW("Second shot by single shot vaccine", break);
                    }
                    vaccinations.push_back(
                                 { /* t */ secvaccdate + ppp.regulardelay,
                                   /* vaccorder */ finaldose,
                                   /* vac */ v
                                  });
                }

                reldate firstextravaccdate;
                string firstextravaccdatestr = data(j,Extra_davka);
                if(firstextravaccdatestr == "")
                    firstextravaccdate = maxreldate;
                else
                {
                    if(firstvaccdate == maxreldate)
                    {
                        THROW("First shot missing", break);
                    }
                    assert(vaccinations.size());
                    if(vaccinations[vaccinations.size()-1].vaccorder != finaldose)
                    {
                        THROW("No final dose in presence of booster", break);
                    }
                    GETDATE(firstextravaccdate,firstextravaccdatestr, break);
                    bool s;
                    unsigned v;
                    GETVACC(v,s,OckovaciLatkaKod3, break);
                    assert(v<vaccines.size());
                    vaccinations.push_back(
                                 { /* t */ firstextravaccdate + ppp.boostdelay,
                                   /* vaccorder */ firstbooster,
                                   /* vac */ v
                                  });

                }
                reldate secextravaccdate;
                string secextravaccdatestr = data(j,Druha_extra_davka);
                if(secextravaccdatestr == "")
                    secextravaccdate = maxreldate;
                else
                {
                    if(firstextravaccdate == maxreldate)
                    {
                        THROW("First extra vacc missing", break);
                    }
                    GETDATE(secextravaccdate,secextravaccdatestr, break);
                    bool s;
                    unsigned v;
                    GETVACC(v,s,OckovaciLatkaKod4, break);
                    assert(v<vaccines.size());
                    vaccinations.push_back(
                                 { /* t */ secextravaccdate + ppp.boostdelay,
                                   /* vaccorder */ secbooster,
                                   /* vac */ v
                                  });
                }
                for(unsigned k=0; k+1<vaccinations.size(); k++)
                    if(vaccinations[k].t >= vaccinations[k+1].t)
                    {
                        THROW("Wrong odering of shot dates.", break);
                    }

                string longcoviddatestr = data(j,Long_COVID);
                if(longcoviddatestr != "")
                    GETDATE(longcoviddate,longcoviddatestr,break);

                if(data(j,lastDCCI) != "")
                {
                   for(int k=firstDCCI; k<=lastDCCI; k++)
                   {
                       int score;
                       try
                       {
                           score = stoul(data(j,k));
                       }
                       catch (...)
                       {
                           cout << "Cannot convert DCCI '"
                                << data(j,k) << "' to int" << endl;
                           throw;
                       }
                       dccis.push_back(score);
                   }
                }
            }


            if(infdate < maxreldate)
            {

                string variantstr = data(j,Mutace);
                unsigned k = 1; // zero is novariant
                for(; k<variants.size(); k++)
                {
                    if(variantstr==variants[k].codeindata)
                        break;
                    if(!ppp.useintervalsofdominance)
                        continue;

                    assert(variants[k].startdates90.size()==variants[k].enddates90.size());
                    bool foundininterval = false;
                    for(unsigned j=0; j<variants[k].startdates90.size(); j++)
                    {
                        reldate start = date2int(variants[k].startdates90[j])-ppp.firstdate;
                        reldate stop = date2int(variants[k].enddates90[j])-ppp.firstdate;
                        if(infdate >= start && infdate <= stop)
                        {
                            foundininterval = true;
                            break;
                        }
                    }
                    if(foundininterval)
                        break;
                }
                assert(ppp.conditioning.size() == enumvariants);
                if(k==variants.size())
                {
                    k = navariant;

                    unsigned l=0;
                    for(;l<throwedvariants.size(); l++)
                    {
                        if(variantstr == throwedvariants[l])
                            break;
                    }
                    if(l==throwedvariants.size())
                        throwedvariants.push_back(variantstr);
                }
                unsigned v = ppp.conditioning[k] ? k : navariant;

                reldate hospdate;
                string hospdatestr = data(j,min_Hospitalizace);
                if(hospdatestr != "")
                {
                    GETDATE(hospdate,hospdatestr,break)
                }
                else
                    hospdate = maxreldate;

                reldate oxygendate;
                string oxygendatestr = data(j,min_Kyslik);
                if(oxygendatestr != "")
                {
                    GETDATE(oxygendate,oxygendatestr,break)
                }
                else
                    oxygendate = maxreldate;
                reldate upvecmodate;
                string upvecmodatestr = data(j,min_UPV_ECMO);
                if(upvecmodatestr != "")
                {
                    GETDATE(upvecmodate,upvecmodatestr, break );
                }
                else
                    upvecmodate = maxreldate;

                bool hosp = (data(i,PrimPricinaHospCOVID)=="1" ||
                             ppp.ignorehospcovidflag) && hospdate - infdate <= ppp.hosplimit;
                bool proxy = 
                 ( data(i,PrimPricinaHospCOVID)=="1" || ppp.ignorehospcovidflag) &&
                        ((data(i,bin_Kyslik) == "1" && oxygendate - infdate <= ppp.hosplimit)
                                   || (data(j,bin_UPV_ECMO) == "1" && upvecmodate - infdate <= ppp.hosplimit)) ;
//if(longcovid && longcoviddate < infdate)
//    cout <<id << " - " << longcoviddatestr << " vs " << infdatestr << endl;
if(id==407)
    id = 407;
                if(infections.size())
                {
                     if(infdate <= infections[infections.size()-1].t)
                     {
                        THROW("Wrong ordering of infections", break);
                     }
                     if(infdate - infections[infections.size()-1].t
                            <= ppp.firstcovreinfduration )
                     {
                         THROW("Reinfection within administrative limit.", break);
                     }
                }
                variantsfound[k].infections++;
                int dateind = ppp.firstdate + infdate - ppp.zerodate;
                if(dateind < variantsfound[k].calendar.size())
                {
                    assert(dateind >= 0);
                    variantsfound[k].calendar[dateind]++;
                }
                if((mode == einfections) ||
                    (mode == eseriouscovidproxy && proxy) ||
                    (mode == ehospitalization && hosp))
                    variantsfound[k].outcomes++;
                infections.push_back({ infdate, v, k, hosp, proxy, false });
            }
            string deathcoviddatestr = data(j,Umrti);
            if(deathcoviddatestr != "")
            {
                GETDATE(deathcoviddate,deathcoviddatestr,break);
            }

            string deathotherdatestr = data(j,DatumUmrtiLPZ);
            if(deathotherdatestr != "")
            {
                GETDATE(deathotherdate,deathotherdatestr,break);
            }

            reldate relevantdate = emptyfound ?
                relevantvaccdate : infdate;
            if(relevantdate > ppp.safetydate)
            {
                THROW("Relevant date beyond safety date", break)
            }
            if(abs(ppp.firstdate-relevantdate) < disttofirst)
            {
                disttofirst = abs(ppp.firstdate-relevantdate);
                relevantrecord = j;
            }

            firstrecord = false;
        }
        if(throwthisid)
            continue;

        bool longcovid = longcoviddate < maxreldate;

//        bool lcassigned = false;
        if(longcovid && infections.size())
        {
            for(int j=infections.size()-1; j>=0; j--)
            {
                if(infections[j].t <= longcoviddate)
                {
                    infections[j].longcovid = true;
//                    lcassigned = true;
                    break;
                }
            }
        }

        for(int j=infections.size()-1; j>=0; j--)
        {
            auto week = (infections[j].t+ppp.firstdate-ppp.zerodate)/7;
            assert(week >= 0);
            assert(week < lccounts.size());
            assert(week < lclengths.size());
            auto lc = infections[j].longcovid;
            lccounts[week].add(lc);
            if(lc)
                lclengths[week].add(longcoviddate-infections[j].t);
        }


        string gstr = data(oldi,pohlavi);

        bool male;
        if(gstr != malestr && gstr != femalestr && gstr != czfemalestr)
        {
            THROWS("Unknown gender ", gstr,continue);
        }
        else
            male = gstr == malestr;


        string agestr = data(relevantrecord,vek);
        unsigned age;
        if(agestr=="")
        {
            THROW("Missing age.",continue);
        }
        else
        {
            try
            {
                age = stoul(agestr);
                if(age < minage || age > maxage)
                {
                   continue;
                }
                if(deathcoviddate >= ppp.firstdate && deathotherdate>= ppp.firstdate)
                {
                    auto correctedage = min(age,lastage);
                    if(male)
                    {
                        assert(!(correctedage < 0 || correctedage > men.size()));
                        men[correctedage]++;
                    }
                    else
                    {
                        assert(!(correctedage < 0 || correctedage > women.size()));
                        women[correctedage]++;
                    }
                }
            }
            catch (...)
            {
                THROWS("Cannot convert age", "'" << agestr << "' to unsigned",continue);
            }
        }

        unsigned agegroup=age2groupnum(age);

        assert(agegroup < enumagegroups);

        struct vaccstatus
        {
            evaccorder order;
            unsigned vaccine;
            unsigned covno;
            reldate vaccdate;
        };

//         eseverity lastinfseverity = enotsevereorunknown;

        vaccstatus currentvaccstatus= { novacc, eunknownvaccine, 0 };

        reldate t1 = ppp.zerodate - ppp.firstdate; // may be negative, I know, I count wizth it

        unsigned nextvaccptr = 0;
        unsigned nextinfptr = 0;
        infectionrecord* lastinfection = 0;

        reldate enddate = T;

        if(mode == elongcovidevent)
            enddate = min(enddate,longcoviddate);

        enddate = min(min(deathcoviddate,deathotherdate),enddate);
        if(enddate <= 0)
        {
            THROW("Censored berore study started", continue);
        }
        enum eimmunitystatus {enoimmunity, efullimmunity, eboostimmunity, esecboostimmunity,
                              einfectedimmunity, einffullimmunity,efullinfimmunity,
                              einfboostimmunity,eboostinfimmunity,
                              einfsecboostimmunity,esecboostinfimmunity,
                              eotherimmunity, enumregularimmunitystatusts = eotherimmunity,
                              esecboostaltimmunity,eboostaltimmunity, enumimunitystauses};
        int currentinfstatus = 0;
//        infectionrecord* lastinfection = 0;

//        reldate lastvaccdate = maxreldate;
//        reldate currentvaccdate = maxreldate;
        reldate lastvaccstatusdate = maxreldate;

        eimmunitystatus currentimmunitystatus = enoimmunity;

        const int nodcciindex = -1;

        int currentdcciindex = dccis.size() ? 0 : nodcciindex;

        throwthisid = false;

        peopleexported++;

//cout << "id " << id << " infs " << infections.size() << " vaccs " << vaccinations.size() << endl ;
//  bool debug = (id % 100) == 0;

        string immunityatinfstring = "";

        for(;;)
        {

             reldate nextdccichangedate = maxreldate;
             int candidatedcciindex = nodcciindex;
             if(dccis.size())
             {
                 for(int k=currentdcciindex+1; k<dccis.size(); k++)
                 {
                     if(dccis[k] != dccis[currentdcciindex])
                     {
                         assert(k < dccidates.size());
                         nextdccichangedate = dccidates[k] - ppp.zerodate;
                         candidatedcciindex = k;
                         break;
                     }
                 }
             }

             unsigned newinfstatus = currentinfstatus;
             reldate nextinfdate = nextinfptr == infections.size()
                     ? maxreldate : infections[nextinfptr].t;

             int nextvaccdate = maxreldate;
             if(nextvaccptr < vaccinations.size())
                 nextvaccdate = vaccinations[nextvaccptr].t;

             int nextinfstatusupdate;
             if(currentinfstatus == 0 || currentinfstatus == ppp.numinfcovariates)
                 nextinfstatusupdate = maxreldate;
             else
             {
                 assert(lastinfection);
                nextinfstatusupdate=lastinfection->t + ppp.firstcovreinfduration;;
                for(;;)
                {
                    if(nextinfstatusupdate > t1)
                        break;
                    nextinfstatusupdate += ppp.covreinfduration;
                }
             }

             vaccstatus newvaccstatus = currentvaccstatus;
             reldate nextvaccstatusdate;
             vaccstatus candidatevaccstatus = currentvaccstatus;

             eimmunitystatus newimmunitystatus = currentimmunitystatus;

             if(currentvaccstatus.order == novacc)
                 nextvaccstatusdate = maxreldate;
             else
             {
                 assert(lastvaccstatusdate < maxreldate);

                 bool last = false;

                 switch(currentvaccstatus.order)
                 {
                     case novacc:
                         assert(0);
                     break;
                     case firstdose:
                         assert(currentvaccstatus.covno <ppp.numpartialcovs);
                         last = currentvaccstatus.covno+1 == ppp.numpartialcovs;
                     break;
                     case finaldose:
                         assert(currentvaccstatus.covno <ppp.numfinalcovs);
                         last = currentvaccstatus.covno+1 == ppp.numfinalcovs;
                     break;
                     case firstbooster:
                         assert(currentvaccstatus.covno <ppp.numboostercovs);
                         last = currentvaccstatus.covno+1 == ppp.numboostercovs;
                     break;
                     case secbooster:
                         assert(currentvaccstatus.covno <ppp.numsecboostercovs);
                         last = currentvaccstatus.covno+1 == ppp.numsecboostercovs;
                     break;
                     case eunknownvaccorder:
                         assert(0);
                         break;
                     default:
                         assert(0);
                         break;
                 }
                 if(last)
                     nextvaccstatusdate = maxreldate;
                 else
                 {
                     candidatevaccstatus = currentvaccstatus;
                     candidatevaccstatus.covno++;
                     nextvaccstatusdate = lastvaccstatusdate + ppp.regularcovvaccduration;
                 }
             }

             int t2 = min(nextvaccdate,
                          min(nextinfdate,
                          min(nextvaccstatusdate,
                          min(nextinfstatusupdate,
                          min(nextdccichangedate,enddate)))));

             int newdcciindex = currentdcciindex;


             if(t2==nextdccichangedate)
             {
                 assert(candidatedcciindex >= 0);
                 newdcciindex = candidatedcciindex;
             }

             if(t2==nextvaccstatusdate)
             {
                 lastvaccstatusdate = t2;
                 newvaccstatus = candidatevaccstatus;
             }

             if(t2==nextinfstatusupdate)
                 newinfstatus++;

             bool infectedont2 = t2 == nextinfdate;

             bool vaccinatedont2 = t2 == nextvaccdate;

             if(vaccinatedont2)
             {
                 vaccinationrecord e = vaccinations[nextvaccptr++];
                 assert(e.vac < vaccines.size());
                 assert(e.vaccorder >= firstdose);
                 if(e.vaccorder == firstdose)
                 {
                     assert(currentvaccstatus.order == novacc);

                     newvaccstatus.covno = 0;
                     newvaccstatus.order = firstdose;
                     newvaccstatus.vaccine = e.vac;
                     newvaccstatus.vaccdate = nextvaccdate;
                     newimmunitystatus = eotherimmunity;
                 }
                 else if(e.vaccorder == finaldose)
                 {
                     newvaccstatus.covno = 0;
                     newvaccstatus.order = finaldose;
                     newvaccstatus.vaccine = e.vac;
                     newvaccstatus.vaccdate = nextvaccdate;
                     if(infectedont2 || currentinfstatus > 0)
                         newimmunitystatus = einffullimmunity;
                     else
                         newimmunitystatus = efullimmunity;

                 }
                 else if(e.vaccorder == firstbooster)
                 {
                     assert(currentvaccstatus.order == finaldose);

                     newvaccstatus.covno = 0;
                     newvaccstatus.order = firstbooster;
                     newvaccstatus.vaccine = e.vac;
                     newvaccstatus.vaccdate = nextvaccdate;
                     if(infectedont2 || currentinfstatus > 0)
                         newimmunitystatus = einfboostimmunity;
                     else
                     {
                         if(ppp.discern23boost)
                         {
                             if(nextvaccdate + ppp.firstdate < ppp.secboosterstartdate - 60)
                                 newimmunitystatus = eboostimmunity;
                             else
                                 newimmunitystatus = eboostaltimmunity;
                         }
                         else if(ppp.discern45booster)
                         {
                              if(vaccines[e.vac].abbrev == ppp.abbrev45)
                                  newimmunitystatus = eboostaltimmunity;
                              else
                                  newimmunitystatus = eboostimmunity;
                         }
                         else
                             newimmunitystatus = eboostimmunity;
                     }

                 }
                 else if(e.vaccorder == secbooster)
                 {
                     assert(currentvaccstatus.order == firstbooster);

                     newvaccstatus.covno = 0;
                     newvaccstatus.order = secbooster;
                     newvaccstatus.vaccine = e.vac;
                     newvaccstatus.vaccdate = nextvaccdate;
                     if(infectedont2 || currentinfstatus > 0)
                         newimmunitystatus = einfsecboostimmunity;
                     else
                     {
                         if(ppp.discernsecboosts)
                         {
                             if(nextvaccdate - currentvaccstatus.vaccdate < ppp.secboostlatedatetreshold)
                                  newimmunitystatus = esecboostimmunity;
                              else
                                  newimmunitystatus = esecboostaltimmunity;
                         }
                         else if(ppp.discern45booster)
                         {
                             if(vaccines[e.vac].abbrev == ppp.abbrev45)
                                 newimmunitystatus = esecboostaltimmunity;
                             else
                                 newimmunitystatus = esecboostimmunity;
                         }
                         else
                             newimmunitystatus = esecboostimmunity;
                     }
                 }
                 else
                     assert(0);
                 lastvaccstatusdate = t2;

//                 lastvaccdate = t2;
//                 currentvaccdate = t2;
             }

             int infected;
             int seriouscovidproxy;
             int hospitalized;
             int longcovidinfection;
             int longcovidevent;

             bool isevent;
//             string variantcompstring;

             infectionrecord* newinfection = 0;


             if(infectedont2)
             {
                 newinfstatus = 1;
                 if(vaccinatedont2 || currentvaccstatus.order > novacc)
                 {
                     auto order = vaccinatedont2 ? newvaccstatus.order : currentvaccstatus.order;
                     switch(order)
                     {
                     case firstdose:
                         newimmunitystatus = eotherimmunity;
                         break;
                     case finaldose:
                         newimmunitystatus = efullinfimmunity;
                         break;
                     case firstbooster:
                         newimmunitystatus = eboostinfimmunity;
                          break;
                     case secbooster:
                         newimmunitystatus = esecboostinfimmunity;
                          break;
                     default:
                         assert(0);
                     }
                 }
                 else
                     newimmunitystatus = einfectedimmunity;


                 newinfection = &(infections[nextinfptr++]);
                 infected = 0;
                 for(unsigned l=0; l<ppp.isoutcome.size();l++)
                    if(ppp.isoutcome[l] && newinfection->variantunadjusted == l)
                    {
                       infected = 1;
                       break;
                    }
                 seriouscovidproxy = infected * newinfection->seriouscovidproxy;
                 hospitalized = infected * newinfection->hospitalized;
                 longcovidinfection = infected * newinfection->longcovid;

                 if(mode == einfections || mode == ecomparison)
                 {
                     isevent = infected;
                 }
                 else if(mode == eseriouscovidproxy)
                     isevent = seriouscovidproxy;
                 else if(mode == ehospitalization)
                     isevent = hospitalized;
                 else if(mode == elongcovidinfection)
                     isevent = longcovidinfection;
             }
             else
             {
                 infected = 0;
                 seriouscovidproxy = 0;
                 hospitalized = 0;
                 longcovidinfection = 0;
                 isevent = false;
             }

             if(mode==elongcovidevent)
             {
                 if(t2==longcoviddate) // == enddate if less than the horizon
                 {
                     isevent = true;
                     longcovidevent = 1;
                 }
                 else
                 {
                     longcovidevent = 0;
                     isevent = false;
                 }
             }

             int deadbycovid = t2==deathcoviddate;
             int deadbyother = t2==deathotherdate;


             string dccistring;
             if(currentdcciindex == nodcciindex)
                 dccistring = nodccilabel;
             else
             {
                 assert(currentdcciindex >= 0);
                 assert(currentdcciindex < dccis.size());
                 ostringstream os;
                 dccistring = dcci2str(dccis[currentdcciindex]);
             }



             string immunitystring;
             if(currentimmunitystatus == enoimmunity)
                immunitystring = noimmunitylabel;
             else
                 immunitystring = "_other";

             string infpriorstr;
             if(currentinfstatus == 0)
                 infpriorstr = uninflabel;
             else
             {
                 ostringstream os;
                 assert(lastinfection);
                 os << "inf_";
                 if(!ppp.groupvariants)
                 {
                     if(ppp.generalvariants)
                         os << variants[lastinfection->variant].supcategory;
                     else
                         os << variants[lastinfection->variant].codeincovariate;
                     os << "_";
                 }
                 int from,to;
                 switch(currentinfstatus)
                 {
                     case 0:
                         assert(0);
                     break;
                     case 1:
                         from = 1;
                         to = ppp.firstcovreinfduration;
                     break;
                     default:
                         assert(currentinfstatus <= ppp.numinfcovariates);
                    {
                         from = ppp.firstcovreinfduration
                               + ppp.covreinfduration * (currentinfstatus-2) + 1;
                         to = from + ppp.covreinfduration-1;
                    }

                 }

                 ostringstream is;
                 bool fillimmunity = true;

                 switch(currentimmunitystatus)
                 {
                 case einfectedimmunity:
                     is << "inf_";
                     break;
                 case efullinfimmunity:
                     is << "hybridfull_";
                     break;
                 case eboostinfimmunity:
                 case esecboostinfimmunity:
                     is <<  "hybridboost_";
                     break;
                 default:
                     fillimmunity = false;
                 }

                 os << threedigits(from);
                 is << threedigits(from);
                 if(currentinfstatus == ppp.numinfcovariates)
                 {
                     os << "+";
                     fillimmunity = false;
                 }
                 else
                 {
                     is << "-" << threedigits(to);
                     os << "-" << threedigits(to);
                 }

                 infpriorstr = os.str();

                 if(fillimmunity)
                     immunitystring = is.str();
             }

             string vaccstring;
             if(currentvaccstatus.order == novacc)
             {
                 vaccstring = unvacclabel;
             }
             else
             {
                 ostringstream os;
                 ostringstream is;
                 bool fillimmunity = true;
                 switch(currentimmunitystatus)
                 {
                 case efullimmunity:
                 case eboostimmunity:
                 case esecboostimmunity:
                 case einffullimmunity:
                 case einfboostimmunity:
                 case einfsecboostimmunity:
                 // sensitivity
                 case esecboostaltimmunity:
                 case eboostaltimmunity:
                     fillimmunity = true;
                     break;
                 default:
                     fillimmunity = false;
                 }
                 unsigned nc;
                 switch(currentvaccstatus.order)
                 {
                 case firstdose:
                     os << "partial";
                     nc = ppp.numpartialcovs;
                     break;
                 case finaldose:
                     os << "full";
                     if(currentimmunitystatus == einffullimmunity)
                        is << "hybridfull";
                     else if(currentimmunitystatus == efullimmunity)
                        is << "full";
                     nc = ppp.numfinalcovs;
                     break;
                 case firstbooster:
                     os << "boost";
                     if(currentimmunitystatus == einfboostimmunity)
                         is << "hybridboost";
                     else if(currentimmunitystatus == eboostimmunity)
                        is << "boost";
                     else if(currentimmunitystatus == eboostaltimmunity)
                         is << (ppp.discern45booster ? "b45":"boost23");

                     nc = ppp.numboostercovs;
                     break;
                 case secbooster:
                     os << "secboost";
                     if(currentimmunitystatus == einfsecboostimmunity)
                         is << "hybridboost";
                     else if(currentimmunitystatus == esecboostimmunity)
                         is << (ppp.discernsecboosts ? "secboostearly":"secboost");
                     else if(currentimmunitystatus == esecboostaltimmunity)
                         is << (ppp.discern45booster ? "secb45":"secboostlate");
                     else
                         fillimmunity = false;
                     nc = ppp.numsecboostercovs;
                     break;
                 default:
                     assert(0);
                 }
                 os << "_";
                 is << "_";
                 if(!ppp.groupvaccs)
                     os << vaccines[currentvaccstatus.vaccine].abbrev << "_";

                 int from, to;
                 from = currentvaccstatus.covno * ppp.regularcovvaccduration + 1;
                 to = from + ppp.regularcovvaccduration - 1;
                 os << threedigits(from);
                 is << threedigits(from);
                 if(currentvaccstatus.covno+1 < nc)
                 {
                     is << "-" << threedigits(to);
                     os << "-" << threedigits(to);
                 }
                 else
                 {
                     os << "+";
                     fillimmunity = false;
                 }
                 vaccstring = os.str();
                 if(fillimmunity)
                     immunitystring = is.str();
             }
             if(t2>0)
             {
                 int t1nonneg = max(0,t1);

                 bool dooutput = true;
                 if(!dostat)
                 {
                     covstatrecord ipr = findcov(infpriorstr,stat.infprior);
                     covstatrecord vsr = findcov(vaccstring,stat.vaccstatus);
                     covstatrecord dccir = findcov(dccistring,stat.dcci);
                     covstatrecord ar = findcov(grouplabel(agegroup) ,stat.agegroup);
                     // the same functionality of Immunity follows

                     if(ipr.events == 0 || vsr.events == 0
                             || dccir.events == 0 ||ar.events == 0)
                         dooutput = false;
                     if(ppp.checkzeroimmunitycovs)
                     {
                         covstatrecord ir = findcov(immunitystring ,stat.immunity);
                         if(ir.events == 0)
                             dooutput = false;
                     }
                     if(mode == elongcovidevent)
                     {
                         covstatrecord ir = findcov(immunityatinfstring ,stat.immunity);
                         if(ir.events == 0)
                             dooutput = false;
                     }
                 }

                 string variantofinfstr = "";


                 if(mode == ecomparison)
                 {
                     if(isevent)
                     {
                         auto v = newinfection->variantunadjusted;
                         if(v==evDelta)
                             variantofinfstr = "_";
                         variantofinfstr += variants[v].codeincovariate;
                     }
                     else
                         dooutput = false;
                 }
                 if(mode == elongcovidevent && immunityatinfstring  == "" )
                     dooutput = false;


                 if(dooutput)
                 {
                     string longcovidstr = "";
                     if(mode == elongcovidevent)
                         longcovidstr = longcovidevent ? "1" : "0";
                     else if(mode == elongcovidinfection || mode == ecomparison)
                         longcovidstr = longcovidinfection ? "1" : "0";


                     ostringstream os;
                     os << idstr << "," << t1nonneg << "," << t2 << ","
                        << infected << ",";
                     if(mode == ehospitalization)
                        os << hospitalized << ",";
                     else
                        os << ",";
                     if(mode == eseriouscovidproxy)
                        os << seriouscovidproxy << ",";
                     else
                        os << ",";
                     if(mode == elongcovidevent || mode == elongcovidinfection)
                        os << longcovidstr << ",";
                     else
                        os << ",";
                     os << deadbycovid  << "," << deadbyother << ",";
                     os << variantofinfstr << "," << infpriorstr << ","
                        << vaccstring << ","
                        << (mode == elongcovidevent ? immunityatinfstring : immunitystring) << ","
                        << dccistring << ","
                        << age << "," << grouplabel(agegroup) << ","
                        << gender2str(male)
                        << ",,";   // tbd add lastvacctime and infpriortime
                     if(os.str().size()>1000)
                     {
                         cout << "Invalid output line" << endl;
                         REPORT("Invalid output line","");
                         throw;
                     }
                     o << os.str() << endl;

                     if(isevent)
                     {
                         oe << os.str();
// tbd kopiruje sem jen první řádek ze zdroje
                         for(unsigned j=0; j<enumlabels; j++)
                             oe <<  "," << data(i,j);
                         oe << endl;
                     }
                 }
                 if(dostat)
                 {
                     recordcov(infpriorstr,isevent, stat.infprior);
                     recordcov(vaccstring,isevent,stat.vaccstatus);
                     recordcov(mode == elongcovidevent ? immunityatinfstring : immunitystring,isevent, stat.immunity);
                     recordcov(dccistring,isevent, stat.dcci);
                     recordcov(grouplabel(agegroup),isevent,stat.agegroup);
                 }


                 if(t2 >= enddate)
                     break;
                 if(t1 >= t2)
                 {
                     cout << "t1 >= t2 id =" << id << endl;
                     throw;
                 }

              }

             if(infected)
                 immunityatinfstring = vaccstring;

              currentinfstatus = newinfstatus;
              if(newinfection)
                lastinfection = newinfection;
//              lastvaccdate = currentvaccdate;
              currentvaccstatus = newvaccstatus;
              currentdcciindex = newdcciindex;
              currentimmunitystatus = newimmunitystatus;
              t1 = t2;
              if(t1 > enddate)
              {
                  cout << "t1 > enddate=" << enddate << endl;
                  throw;
              }

           }
         if(throwthisid)
         {
             for(unsigned j=0; j<is.size(); j++)
             {
                 oe << """" << errorstring << """";
                 for(unsigned k=0; k<enumlabels; k++)
                     oe << "," << data(is[j],k);
                 oe << endl;
             }
             continue;
         }

    }
    // add missing people

    cout << peopleexported << " individuals taken from the UZIS database" << endl;

    assert(ppp.firstdate >= dateoffirstczsohalfyear);

    if(omitczso)
    {
        cout << "Omitting adding CZSO records." << endl;
    }
    else if(mode != ecomparison)
    {

        auto czsohalfyear = (ppp.firstdate - dateoffirstczsohalfyear) / (366 / 2);

        cout << "Adding records from CZSO from " << (czsohalfyear+1)
             << "th half-year" << endl;

        unsigned ng;
        if(ppp.fourages)
            ng = 4;
        else
            ng = enumagegroups;
        vector<unsigned> addedmen(ng,0);
        vector<unsigned> addedwomen(ng,0);

        unsigned i = maxid+1;

        for(unsigned m=0; m<=1; m++)
        {
            vector<unsigned>& vs = m ? men : women;
            for(unsigned a=0; a<=lastage; a++)
            {
                if(a >= minage && a <= maxage)
                {
                    string agelabel;
                    unsigned g;
                    if(ppp.fourages)
                    {
                        agelabel = fourage2group(a);
                        g = fouragegroup(a);
                    }
                    else
                    {
                        agelabel = age2group(a);
                        g = age2groupnum (a);
                    }

                    bool dooutput = true;
                    if(!dostat)
                    {
                        covstatrecord ar = findcov(agelabel ,stat.agegroup);
                        if(ar.events == 0)
                            dooutput = false;
                    }

                    if(dooutput)
                    {
                        int n = numpeopleofage(a,m,czsohalfyear)-vs[a] * ppp.everyn;
                        if(n < 0)
                        {
                            cout << "More of " << (m ? "men" : "women") << " ( " << vs[a]
                                    << ") treated then exist of age " << a
                                    << " (" << numpeopleofage(a,m,czsohalfyear) << ")" << endl;

                        }
                        else
                        {
                          // cout << "Having " << vs[a] << " " << (m ? "men" : "women") << " of age " << a << ", we add " << n << endl;
                           if(m)
                               addedmen[g] += n;
                            else
                               addedwomen[g] += n;
                        }


                        //of course we do note guarantee ids to follow the "true ones". (maybe we should check whether we do not duplicate ids)


                        for(int j=0; j<n; j++,i++)
                        {
                            if(++outputcounter % ppp.everyn)
                                continue;
                            peopleexported++;
    //                        "ID,T1,T2,Infected,hospitalized,seriouscovidproxy,longcovid,DeadByCovid, DeadByOther, VariantComp";
                            o << i << "," << 0 << "," << T << ",0,";
                            if(mode == ehospitalization)
                                o << "0,";
                            else
                                o << ",";
                            if(mode == eseriouscovidproxy)
                                o << "0,";
                            else
                                o << ",";
                            if(mode == elongcovidevent || mode == elongcovidinfection)
                                o << "0,";
                            else
                                o << ",";
                            o << "0,0,,";


    //                         header << " InfPrior,VaccStatus,Immunity,Age,AgeGr,Sex";

                            o << uninflabel << "," << unvacclabel << ","
                              << (mode == elongcovidevent ? "" : noimmunitylabel) << ","
                              << nodccilabel << ","
                              << a << ","
                              << agelabel << "," << gender2str(m)
                              << ",,"<< endl;
                            if(dostat)
                            {
                                recordcov(uninflabel,0, stat.infprior);
                                recordcov(unvacclabel,0,stat.vaccstatus);
                                recordcov(agelabel,0,stat.agegroup);
                            }
                         }
                     }
                 }
            }
        }
        cout << "Added,Men,Women" << endl;
        for(unsigned g=0; g<ng; g++)
        {
            if(ppp.fourages)
                cout << g;
            else
                cout << grouplabel(g);
            cout << "," << addedmen[g] << "," << addedwomen[g] << endl;
        }

    }

    cout << "Total " << peopleexported << " individuals." << endl;


    ofstream vs(output + "_variants.csv");
    if(!vs)
    {
        cout << "Cannot open " << output << "_variants.csv" << endl;
        throw;
    }

    cout << endl << "Variants of infection found during study period" << endl;
    cout << endl << "variant,infections,outcomes,discerned_in_condition,taken_as_outcome" << endl;
    for(unsigned i=0;
        i<variantsfound.size();
        i++)
    {
        cout << variants[i].codeincovariate << ","
             << variantsfound[i].infections << ","
             << variantsfound[i].outcomes << ","
             << (ppp.conditioning[i] ? 1 : 0) << ","
             << (ppp.isoutcome[i] ? 1 : 0)
             << endl;
        vs << variants[i].codeincovariate;
        unsigned chsum = 0;
        for(unsigned j=0; j<variantsfound[i].calendar.size(); j++)
        {
            vs << "," << variantsfound[i].calendar[j];
            chsum += variantsfound[i].calendar[j];
        }
        assert(chsum <= variantsfound[i].infections );
        vs << endl;
    }

    cout << endl << "Untracked variants (joinded into NA)" << endl;
    for(unsigned i=0; i<throwedvariants.size(); i++)
    {
        auto s = throwedvariants[i];
        cout << (s == "" ? "(empty)" : s ) << endl;
    }
    cout << endl;
    cout << "Mode " << mdelabels[mode] << endl;
    cout << "Age filter: " << minage << "-" << maxage << endl;
    cout << endl;
ofstream lc("lc.csv");
lc<<"day,count, ratio,seratio,length,selength"<< endl;
for(unsigned i=0; i<numweeks; i++)
{
    lc << i*7 << "," << lccounts[i].sum << ",";
    if(lccounts[i].num > 0)
       lc << lccounts[i].average() << "," << sqrt(lccounts[i].averagevar()) << ",";
    else
       lc << ",,";
    if(lclengths[i].num > 0)
        lc << lclengths[i].average() << "," << sqrt(lclengths[i].averagevar());
    else
        lc << ",,";
    lc << endl;
}
}


void displaystat(const covstat& stat, ostream& os, bool filter)
{
    unsigned infpriorcovs;
    unsigned infpriorevents;
    output("infprior",os,stat.infprior,filter,
           infpriorcovs, infpriorevents);
    unsigned vaccstatuscovs;
    unsigned vaccstatusevents;
    output("vaccstatus",os,stat.vaccstatus,filter,
           vaccstatuscovs, vaccstatusevents);
    unsigned immunitycovs;
    unsigned immunityevents;
    output("immunity",os,stat.immunity,filter,
           immunitycovs, immunityevents);
    unsigned dccicovs;
    unsigned dccievents;
    output("dcci",os,stat.dcci,filter,
           dccicovs, dccievents);
    unsigned agegrcovs;
    unsigned agegrevents;
    output("agegr",os,stat.agegroup,filter,
           agegrcovs, agegrevents);

    assert(infpriorevents == agegrevents);
    assert(vaccstatusevents == agegrevents);

    unsigned totalcovs = infpriorcovs + vaccstatuscovs + agegrcovs + dccicovs;
    cout << "covs (immunity version)="
         << totalcovs << " (" << immunitycovs + agegrcovs + dccicovs << ")"
         << ", events="
         << infpriorevents << ", eventspercov="
         << (static_cast<double>(infpriorevents) + 0.5) / totalcovs
         << "(" << (static_cast<double>(infpriorevents) + 0.5) / (immunitycovs + agegrcovs)
         << ")"
         << endl;
}


int _main(int argc, char *argv[], bool testrun = false)
{
//for(unsigned i=0; i<argc; i++)
//    cout << argv[i] << endl;
//    bool onlyfirst = argv[2][0] == '-';
    cout << "version 2.0" << endl;
    cout << "Usage convertool input output whattodo(DVRW) firstdate(rrrr-mm-dd) lastdate(rrrr-mm-dd) [minage maxage everyn]" << endl;
    cout << "D=i-infection/x-covidproxy/h-hospitalization/l-longcovid" << endl;
    cout << "V=!-all variants/O-all Omicrons/E-O+Delta/x-variant x" << endl;
    cout << "R=-variants... --not discerned in InfPrior/a all discerned/g general discerned" << endl;
    cout << "W=i-intervals uded for variant determination/--not used" << endl;
    cout << endl;

    if(argc < 6)
        throw "at least five arguments must be given";
    if(strlen(argv[3]) < 2)
        throw "whattodo has to have at least two letters";

    string firstdatestr = argv[4];
    string lastdatestr = argv[5];
    string everynstr = "";
    if(argc>8)
        everynstr = argv[8];

    o2rmodes mode;
    preprocessparams ppp;

    switch(argv[3][0])
    {
    case 'i':
        mode = einfections;
        cout << "infections" << endl;
        break;
    case 'x':
        mode = eseriouscovidproxy;
        cout << "serious covid proxy" << endl;
        break;
    case 'h':
        mode = ehospitalization;
        cout << "hospitalization" << endl;
        break;
    case 'l':
        mode = elongcovidinfection;
        cout << "long covid as infection" << endl;
        break;
    case 'e':
        mode = elongcovidevent;
        cout << "long covid as event" << endl;
        break;
    case 'c':
        mode = ecomparison;
        cout << "Comparison" << endl;
        break;
    default:
       cout << "Unknonn option " << argv[3][0] << endl;
       throw;
    }

    if(strlen(argv[3]) < 2 || argv[3][1] == '!')
    {
         ppp.isoutcome = vector<bool>(enumvariants,true);
         cout << "All variants as outcomes" << endl;
    }
    else
    {
        ppp.isoutcome = vector<bool>(enumvariants,false);
        if(argv[3][1] == 'O')
        {
            ppp.isoutcome[evBA12] = true;
            ppp.isoutcome[evBA45] = true;
            ppp.isoutcome[evOmicron] = true;
            ppp.isoutcome[ev2023] = true;
            cout << "All Omicrons as outcomes" << endl;
        }
        else if(argv[3][1] == 'E')
        {
            ppp.isoutcome[evDelta] = true;
            ppp.isoutcome[evBA12] = true;
            ppp.isoutcome[evBA45] = true;
            ppp.isoutcome[evOmicron] = true;
            ppp.isoutcome[ev2023] = true;
            cout << "Delta and Omicrons as outcomes" << endl;
        }
        else if(argv[3][1] == 'F')
        {
            ppp.isoutcome[evBA45] = true;
            ppp.isoutcome[ev2023] = true;
            cout << "Omicrons since BA45 dominance" << endl;
        }
        else
        {
            for(unsigned v=0; v<variants.size(); v++)
            {
                if(argv[3][1]==variants[v].commandlineid[0])
                {
                    ppp.isoutcome[v] = true;
                    cout << variants[v].codeinoutput << " as outcome." << endl;
                    break;
                }
            }
        }
    }

    ppp.generalvariants = false;

    if(strlen(argv[3]) < 3 || argv[3][2] == '-')
    {
        ppp.conditioning = vector<bool>(enumvariants,false);
        cout << "Variants not discerned in InfPrior" << endl;
    }
    else
    {
        ppp.conditioning = vector<bool>(enumvariants,true);
        cout << "Variants discerned in InfPrior ";
        if(argv[3][2] == 'g')
        {
            ppp.generalvariants = true;
            cout << "(general versions)";
        }
        else if(argv[3][2] != 'a')
        {
            cout << "Unknown option of conditioning!" << endl;
            throw;
        }
        cout << endl;
    }

    if(strlen(argv[3]) < 4 || argv[3][3] == '-')
    {
        ppp.useintervalsofdominance = false;
        cout << "Not using intervals of dominance" << endl;
    }
    else if(argv[3][3] == 'i')
    {
        ppp.useintervalsofdominance = true;
        cout << "Using intervals of dominance" << endl;
    }
    else
    {
        cout << "Unknown W option " << argv[3][3] << endl;
        throw;
    }

    for(unsigned i=4; i<strlen(argv[3]); i++)
    {
        switch(argv[3][i])
        {
        case 'l':
            ppp.discernsecboosts = true;
            ppp.checkzeroimmunitycovs = true;
            cout << "secboost previous vacc time discerned" << endl;
            break;
        case '2':
            ppp.discern23boost = true;
            ppp.checkzeroimmunitycovs = true;
            cout << "Boosts simultaneous to secboosts discerned" << endl;
            break;
        case 'c':
            ppp.checkzeroimmunitycovs = true;
            cout << "Checking immunity covariates for zeros" << endl;
            break;
        case 'n':
            ppp.ignorehospcovidflag = true;
            cout << "Not using confirmation of covid as a reason for hospitalization" << endl;
            break;
        case 'b':
            ppp.discern45booster = true;
            cout << "Discerning BA45 booster" << endl;
            break;
        default:
            cout << "Unknown option " << argv[3][i] << endl;
            throw;
        }
    }

    try
    {
        ppp.firstdate = date2int(firstdatestr);
    }
    catch(...)
    {
        cout << "Error converting first date string "  << firstdatestr << endl;
        throw;
    }

    try
    {
        ppp.lastdate = date2int(lastdatestr);
    }
    catch(...)
    {
        cout << "Error converting last date string " << lastdatestr << endl;
        throw;
    }

    if(everynstr != "")
    {
        try
        {
            ppp.everyn = stoul(everynstr);
            if(ppp.everyn==0)
                throw "Everyn cannot be zero";
        }
        catch(...)
        {
            cout << "Error converting everyn string " << everynstr << endl;
            throw;
        }
    }

    if(mode == eseriouscovidproxy || mode == ehospitalization)
        ppp.lastdate -= ppp.hosplimit;

    cout << "Input " << argv[1] << endl;



    ppp.numinfcovariates = max(0,(ppp.lastdate-ppp.reinfstartdate)/ppp.covreinfduration + 3);
    ppp.numinfcovariates = min(ppp.numinfcovariates, ppp.maxinfcovs );
    cout << "Number of InfPrior covariates: " << ppp.numinfcovariates << endl;


    ppp.numfinalcovs = max(0,(ppp.lastdate-ppp.vaccstartdate)/ppp.regularcovvaccduration + 3);
    ppp.numfinalcovs = min(ppp.numfinalcovs, ppp.maxvacccovs );
    cout << "Number of VaccStatusFull covariates: " << ppp.numfinalcovs  << endl;

    ppp.numboostercovs = max(0,(ppp.lastdate-ppp.boosterstartdate)/ppp.regularcovvaccduration + 3);
    ppp.numboostercovs = min(ppp.numboostercovs, ppp.maxvacccovs );
    cout << "Number of VaccStatusBoost covariates: " << ppp.numboostercovs  << endl;

    ppp.numsecboostercovs = max(0,(ppp.lastdate-ppp.secboosterstartdate)/ppp.regularcovvaccduration + 3);
    ppp.numsecboostercovs = min(ppp.numboostercovs, ppp.maxvacccovs );
    cout << "Number of VaccStatusSecBoost covariates: " << ppp.numsecboostercovs  << endl;

    csv<';'> data(argv[1]);
    covstat stat;

    string firststageoutputfn = "temporary";

    cout << "Output " << argv[2] << endl;

    cout << "Date range " << firstdatestr
         << " - " << lastdatestr << endl << endl;

    if(ppp.everyn > 1)
        cout << "Only every " << ppp.everyn << "th record." << endl;

    ockodata2R(data, firststageoutputfn, true, stat,
               mode,
               argc > 6 ? atoi(argv[6]) : 0,
               argc > 7 ? atoi(argv[7]) : 333,
               ppp, true);



    displaystat(stat,cout,false);

    bool onlyfirst = false; // TBD
    if(onlyfirst)
    {
        cout << "Not running final conversion" << endl;
    }
    else
    {
        ockodata2R(data, argv[2], false, stat,
                       mode,
                       argc > 6 ? atoi(argv[6]) : 0,
                       argc > 7 ? atoi(argv[7]) : 333,
                       ppp, testrun);

        displaystat(stat,cout,true);
    }

    return 0;

}


int main(int argc, char *argv[])
{
    double startt = time(0);

    for(unsigned i=0; i<argc; i++)
        cout << argv[i] << " ";
    cout << endl;

    try
    {
        int testno = 0;
        if(argc == 1)
            testno = 1;
        if(argc == 2)
        {
            testno = argv[1][0] - '1' + 1;
        }

        if(testno == 0)
            _main(argc,argv);
        else if(testno == 1)
        {
            char *as[6] ={"foo", "test_input_long_mod.csv","test1_output.csv","eO-ib",
                          "2020-01-01", "2023-02-20"};
            _main(6,as,true);
        }
        else if(testno == 2)
        {
            char *as[9] ={"foo", "../data/data_20230329.csv","test2_output.csv","hF-i",
                          "2022-07-01", "2023-02-20", "0", "333", "10" };
            _main(6,as,true);
        }
        else if(testno == 3)
        {
            char *as[6] ={"foo", "test_input_long_1.csv","test3_output.csv","e!-ic",
                          "2020-01-01", "2022-09-30"};
            _main(6,as,true);
        }
        else if(testno == 4)
        {
            char *as[6] ={"foo", "test_input_long_1.csv","test4_output.csv","iOgi",
                          "2021-12-01","2023-02-20"};
            _main(6,as,true);
        }
        else if(testno == 5)
        {
            char *as[6] ={"foo", "test_input_long_1.csv","test5_output.csv","cE",
                          "2021-12-01","2022-02-13"};
            _main(6,as,true);
        }

    }
    catch (std::exception& e) {
        std::cout << e.what() << endl;
        return 1;
    }
    catch (const char* m)
    {
           cout << m << endl;
           return 1;
    }
    catch (const string& m)
    {
           cout << m << endl;
           return 1;
    }
    catch(...)
    {
        cout << "unknown exception" << endl;
        return 1;
    }


    cout << "time: " << time(0)-startt << " seconds." << endl;

    return 0;

}




