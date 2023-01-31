#include "tooltools.hpp"
#include <fstream>


string malestr = "M";
string femalestr = "F";
string czfemalestr = "Z";

string unvacclabel = "_unvaccinated";
string uninflabel = "_uninfected";

string gender2str(bool male)
{
    return male ? "M" : "F";
}

enum eagegroups { v0011, v1215, v1617, v1824, v2529, v3034, v3539, v4044,v4549, v5055,v5559, v6054,v6559, v7074,v7579, v80plus, enumagegroups };




constexpr unsigned lastage = 100;

reldate dateoffirstczsohalfyear = date2int("2020-01-01");

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
    static vector<string> g = {		"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"0-11",	"12-15",	"12-15",	"12-15",	"12-15",	"16-17",	"16-17",	"18-24",	"18-24",	"18-24",	"18-24",	"18-24",	"18-24",	"18-24",	"25-29",	"25-29",	"25-29",	"25-29",	"25-29",	"30-34",	"30-34",	"30-34",	"30-34",	"30-34",	"35-39",	"35-39",	"35-39",	"35-39",	"35-39",	"_40-44",	"_40-44",	"_40-44",	"_40-44",	"_40-44",	"45-49",	"45-49",	"45-49",	"45-49",	"45-49",	"50-54",	"50-54",	"50-54",	"50-54",	"50-54",	"55-59",	"55-59",	"55-59",	"55-59",	"55-59",	"60-64",	"60-64",	"60-64",	"60-64",	"60-64",	"65-69",	"65-69",	"65-69",	"65-69",	"65-69",	"70-74",	"70-74",	"70-74",	"70-74",	"70-74",	"75-79",	"75-79",	"75-79",	"75-79",	"75-79",	"80+"	};
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
};

enum evariant {evNA,evDelta,evBA45,evBA12, evOmicron, evAlpha, evBeta, enumvariants};

vector<variantrecord> variants
{
    { "", "NA_Other","NA","n","NA"},
    { "Delta", "Delta","Delta", "d", "DELTA"},
    { "Omikron BA.4/5", "BA45", "BA45", "4", "OMICRON"},
    { "Omikron BA.1/2", "BA12", "BA12", "1", "OMICRON"},
    { "Omikron", "Omicron", "Omicron", "o", "OMICRON"},
    { "Alfa", "Alpha", "Alpha", "a", "ALPHA"},
    { "Beta", "Beta", "Beta", "b", "BETA"},
};

auto navariant = 0;

struct preprocessparams
{
    /// least date possible (others reported as errors
    int zerodate = date2int("2020-01-01");

    /// delay after the first, second, third and fourth shot takes effect
    int regulardelay = 14;
    int boostdelay = 7;

    /// lenght of the window after infection within which hospitalization is examined
    /// (after this limit, the subject is censored in h analysis TBD CHECK THIS!!)
    int outcomelimit = 15;

    reldate safetydate = date2int("2022-12-01");

//    /// time delay during which new positive tests are not regarded as reinfections
//    int infectionsgap = 60;

    /// number of the last postinf covariate (0 = not infected, 1 - first ...)
    int numinfcovariates = 6;

    /// duration of 1st  postinf covariate (event not regarded as reinfections)
    int firstcovreinfduration = 60;

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


    /// time after the test for the treatment to be assigned to the test
    ///
    int hosplimit = 30;


    /// if true, then only four age categories are used instead of the finer division
    bool fourages = false;

    /// if true then vaccines are grouped in the VaccStatus field
    bool groupvaccs = false;

    bool groupvariants = false;


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


enum o2rmodes { einfections, ecovidproxy };



vector<string> mdelabels = { "infections", "covidproxy" };

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
    cerr << "Cannot find covariate label " << cov << " in list" << endl;
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

    string output = dostat ? (outputlabel + "_full.csv") : outputlabel;

    cout << "Writing to " << output << endl;

    ofstream o(output);
    if(!o)
    {
        cerr << "Cannot open " + output << endl;
        throw;
    }

    ofstream oe(output + ".events.csv");
    if(!oe)
    {
        cerr << "Cannot open " + output + ".events.csv" << endl;
        throw;
    }

    ofstream ou(output + ".unprocessed.csv");
    if(!ou)
    {
        cerr << "Cannot open " + output + ".unprocessed.csv" << endl;
        throw;
    }

    ou << "error";


/*    ofstream tex(output + ".tex");
    if(!tex)
    {
       cerr << "Cannot open " + output + ".tex" << endl;
       throw;
    }*/

    ostringstream header;

    header << "ID,T1,T2,Infected,Covidproxy,DeadByCovid,DeadByOther,";
//    for(unsigned i=0; i<variants.size(); i++)
//        header << variants[i].codeincovariate << "Inf,";
    header << "InfPrior,VaccStatus,Age,AgeGr,Sex";
    header << ",InfPriorTime,LastVaccTime";

    o << header.str() << endl;
    oe << header.str();


    enum egender { emale, efemale, enumgenders };

    enum evaccorder { novacc, firstdose, finaldose, firstbooster, secbooster, enumvaccorders, eunknownvaccorder = enumvaccorders };




    cout << "Importing from input, total " << data.r()-1 <<
         " records." << endl;

    static vector<string> labels = {
        "ID",	"infekce",	"pohlavi",	"vek",	"Kraj_bydliste",	"ORP_Bydliste",	"Datum_pozitivity",	"DatumVysledku",	"Vylecen",	"Umrti",	"symptom",	"typ_testu",	"PrvniDavka",	"DruhaDavka",	"Ukoncene_ockovani",	"Extra_davka",	"Druha_extra_davka",	"OckovaciLatkaKod1",	"OckovaciLatkaKod2",	"OckovaciLatkaKod3",	"OckovaciLatkaKod4",	"bin_Hospitalizace",	"min_Hospitalizace",	"dni_Hospitalizace",	"max_Hospitalizace",	"bin_JIP",	"min_JIP",	"dni_JIP",	"max_JIP",	"bin_STAN",	"min_STAN",	"dni_STAN",	"max_STAN",	"bin_Kyslik",	"min_Kyslik",	"dni_Kyslik",	"max_Kyslik",	"bin_HFNO",	"min_HFNO",	"dni_HFNO",	"max_HFNO",	"bin_UPV_ECMO",	"min_UPV_ECMO",	"dni_UPV_ECMO",	"max_UPV_ECMO",	"Mutace",	"DatumUmrtiLPZ"
    };


    enum elabels {ID,	infekce,	pohlavi,	vek,	Kraj_bydliste,	ORP_Bydliste,	Datum_pozitivity,	DatumVysledku,	Vylecen,	Umrti,	symptom,	typ_testu,	PrvniDavka,	DruhaDavka,	Ukoncene_ockovani,	Extra_davka,	Druha_extra_davka,	OckovaciLatkaKod1,	OckovaciLatkaKod2,	OckovaciLatkaKod3,	OckovaciLatkaKod4,	bin_Hospitalizace,	min_Hospitalizace,	dni_Hospitalizace,	max_Hospitalizace,	bin_JIP,	min_JIP,	dni_JIP,	max_JIP,	bin_STAN,	min_STAN,	dni_STAN,	max_STAN,	bin_Kyslik,	min_Kyslik,	dni_Kyslik,	max_Kyslik,	bin_HFNO,	min_HFNO,	dni_HFNO,	max_HFNO,	bin_UPV_ECMO,	min_UPV_ECMO,	dni_UPV_ECMO,	max_UPV_ECMO,	Mutace,	DatumUmrtiLPZ, enumlabels};

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
        cerr << "Wrong number of columns. Expected "
             << enumlabels << " found " << data.c(0) << endl;
        openerror = true;
    }
    else
    {
        for(unsigned i=0; i<enumlabels; i++)
        {
            if(data(0,i) != labels[i])
            {
                cerr << "Wrong format: " << labels[i]
                     << " expected, " << data(0,i) << " found."
                     << endl;
                openerror = true;
            }
        }
    }

    if(openerror)
    {
        cerr << "Expected header is:" << endl;
        for(unsigned i=0; i<enumlabels; i++)
            cerr << " " << labels[i];
        cerr << endl;
        throw;
    }


    vector<unsigned> men(lastage+1,0);
    vector<unsigned> women(lastage+1,0);




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
        bool covidproxy = false;
    };

    unsigned firstnext;
    unsigned maxid = 0;
    int T = ppp.lastdate - ppp.firstdate;
    unsigned outputcounter = 0;
    unsigned peopleexported = 0;
    for(unsigned i=1;
        i<data.r(); i=firstnext )
    {
//if(i%100000 == 0)
//        cout << i << endl;

        reldate deathcoviddate = maxreldate;
        reldate deathotherdate = maxreldate;

        vector<infectionrecord> infections;

        vector<vaccinationrecord> vaccinations;

// when used, all these macros have to be enclused in braces

#define REPORT(X) for(unsigned j=0; j<is.size(); j++) \
        { \
            ou << "\"" << X << "\""; \
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
            cerr << "Cannot convert ID '" << idstr << "' to unsigned" << endl;
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
                    cerr << "row " << j << " two empty lines" << endl;
                    throw;
                }
                if(j!=i)
                {
                    cerr << "row" << j << "empty line on non-first place" << endl;
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
                    cerr << "field infekce = " << data(j,infekce) << endl;
                    throw;
                }
                is[infindex] = j;
            }
        }
        for(unsigned k=0; k<is.size(); k++)
            if(is[k] == norecord)
            {
                cerr << "invalid record group starting at " << i << endl;
                throw;
            }

        bool throwthisid = false;
        string errorstring = "";

    #define THROW(X,W) { throwthisid = true; REPORT(X) ; W;  }
    #define THROWS(X,W) { ostringstream o; o<< X; REPORT(o.str()); throwthisid = true; W; }
    #define GETDATE(Y,X,W) { bool error; Y=date2int(X,ppp.zerodate, ppp.safetydate, error, errorstring)-ppp.firstdate; if(error) { throwthisid = true; REPORT(errorstring)  W; }   }
    #define GETVACC(V,S,L,W) { V = vacc2vacc(data(j,L)); S = vaccines[V].numshots == 1; if(V==eunknownvaccine) { throwthisid = true; errorstring = data(j,L) + " is an unknonw vaccine code."; REPORT(errorstring); W; } }


        firstnext = i + is.size();
        if(++outputcounter % ppp.everyn )
            continue;


        bool firstrecord = true;
        bool isdead = false;
        reldate disttofirst = maxreldate;
        unsigned relevantrecord;

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

            }

            if(infdate < maxreldate)
            {
                string variantstr = data(j,Mutace);
                unsigned k = 1; // zero is novariant
                for(; k<variants.size(); k++)
                    if(variantstr==variants[k].codeindata)
                        break;
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
                bool proxy =
                        (data(i,bin_Kyslik) == "1" && oxygendate - infdate <= ppp.hosplimit)
                                   || (data(j,bin_UPV_ECMO) == "1" && upvecmodate - infdate <= ppp.hosplimit) ;
                if(infections.size() && infdate <= infections[infections.size()-1].t)
                {
                    THROW("Wrong ordering of infections", break);
                }
                variantsfound[k].infections++;
                int dateind = ppp.firstdate + infdate - ppp.zerodate;
                if(dateind < variantsfound[k].calendar.size())
                {
                    assert(dateind >= 0);
                    variantsfound[k].calendar[dateind]++;
                }
                if(mode == einfections || proxy)
                    variantsfound[k].outcomes++;
                infections.push_back({ infdate, v, k, proxy });
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

        string gstr = data(oldi,pohlavi);

        bool male;
        if(gstr != malestr && gstr != femalestr && gstr != czfemalestr)
        {
            THROWS("Unknown gender " << gstr,continue);
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
                    THROWS("Age "<< agestr << " out of range",continue);
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
                THROWS("Cannot convert age '" << agestr << "' to unsigned",continue);
            }
        }

        unsigned agegroup=age2groupnum(age);

        assert(agegroup < enumagegroups);

        struct vaccstatus
        {
            evaccorder order;
            unsigned vaccine;
            unsigned covno;
        };

//         eseverity lastinfseverity = enotsevereorunknown;

        vaccstatus currentvaccstatus= { novacc, eunknownvaccine, 0 };

        reldate t1 = ppp.zerodate - ppp.firstdate; // may be negative, I know, I count wizth it

        unsigned nextvaccptr = 0;
        unsigned nextinfptr = 0;
        infectionrecord* lastinfection = 0;

        reldate enddate = T;

        if(mode == ecovidproxy)
            enddate = min(enddate,T-ppp.hosplimit);

        enddate = min(min(deathcoviddate,deathotherdate),enddate);
        if(enddate <= 0)
        {
            THROW("Censored berore study started", continue);
        }
        int currentinfstatus = 0;
//        infectionrecord* lastinfection = 0;

//        reldate lastvaccdate = maxreldate;
//        reldate currentvaccdate = maxreldate;
        reldate lastvaccstatusdate = maxreldate;

        throwthisid = false;

        peopleexported++;

//cout << "id " << id << " infs " << infections.size() << " vaccs " << vaccinations.size() << endl ;
//  bool debug = (id % 100) == 0;
         for(;;)
         {
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
                          min(nextinfstatusupdate,enddate))));

             if(t2==nextvaccstatusdate)
             {
                 lastvaccstatusdate = t2;
                 newvaccstatus = candidatevaccstatus;
             }


             if(t2==nextinfstatusupdate)
                 newinfstatus++;

             if(t2==nextvaccdate)
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
                 }
                 else if(e.vaccorder == finaldose)
                 {
                     newvaccstatus.covno = 0;
                     newvaccstatus.order = finaldose;
                     newvaccstatus.vaccine = e.vac;
                 }
                 else if(e.vaccorder == firstbooster)
                 {
                     assert(currentvaccstatus.order == finaldose);

                     newvaccstatus.covno = 0;
                     newvaccstatus.order = firstbooster;
                     newvaccstatus.vaccine = e.vac;
                 }
                 else if(e.vaccorder == secbooster)
                 {
                     assert(currentvaccstatus.order == firstbooster);

                     newvaccstatus.covno = 0;
                     newvaccstatus.order = secbooster;
                     newvaccstatus.vaccine = e.vac;
                 }
                 else
                     assert(0);
                 lastvaccstatusdate = t2;

//                 lastvaccdate = t2;
//                 currentvaccdate = t2;
             }

             int infected;
             int covidproxy;
             bool isevent;

             infectionrecord* newinfection = 0;

             if(t2 == nextinfdate)
             {
                 newinfstatus = 1;
//                 lastinfdate = t2;

                 newinfection = &(infections[nextinfptr++]);
                 infected = 0;
                 for(unsigned l=0; l<ppp.isoutcome.size();l++)
                    if(ppp.isoutcome[l] && newinfection->variantunadjusted == l)
                    {
                       infected = 1;
                       break;
                    }
                 covidproxy = infected * newinfection->covidproxy;

                 if(mode == einfections)
                     isevent = infected;
                 else
                     isevent = covidproxy;
             }
             else
             {
                 infected = 0;
                 covidproxy = 0;
                 isevent = false;
             }

             int deadbycovid = t2==deathcoviddate;
             int deadbyother = t2==deathotherdate;

             if(t2>0)
             {
                 int t1nonneg = max(0,t1);

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
                         os << variants[lastinfection->variant].codeincovariate << "_";
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
                     os << threedigits(from);
                     if(currentinfstatus == ppp.numinfcovariates)
                         os << "+";
                     else
                         os << "-" << threedigits(to);
                     infpriorstr = os.str();
                 }

                 string vaccstring;
                 if(currentvaccstatus.order == novacc)
                 {
                     vaccstring = unvacclabel;
                 }
                 else
                 {
                     ostringstream os;
                     unsigned nc;
                     switch(currentvaccstatus.order)
                     {
                     case firstdose:
                         os << "partial";
                         nc = ppp.numpartialcovs;
                         break;
                     case finaldose:
                         os << "full";
                         nc = ppp.numfinalcovs;
                         break;
                     case firstbooster:
                         os << "boost";
                         nc = ppp.numboostercovs;
                         break;
                     case secbooster:
                         os << "secboost";
                         nc = ppp.numsecboostercovs;
                         break;
                     default:
                         assert(0);
                     }
                     os << "_";
                     if(!ppp.groupvaccs)
                         os << vaccines[currentvaccstatus.vaccine].code << "_";

                     int from, to;
                     from = currentvaccstatus.covno * ppp.regularcovvaccduration + 1;
                     to = from + ppp.regularcovvaccduration - 1;
                     os << threedigits(from);
                     if(currentvaccstatus.covno+1 < nc)
                         os << "-" << threedigits(to);
                     else
                         os << "+";
                     vaccstring = os.str();
                 }


                 bool dooutput = true;
                 if(!dostat)
                 {
                     covstatrecord ipr = findcov(infpriorstr,stat.infprior);
                     covstatrecord vsr = findcov(vaccstring,stat.vaccstatus);
                     covstatrecord ar = findcov(grouplabel(agegroup) ,stat.agegroup);
                     if(ipr.events == 0 || vsr.events == 0 || ar.events == 0)
                         dooutput = false;
                 }
                 if(dooutput)
                 {
                     ostringstream os;
                     os << idstr << "," << t1nonneg << "," << t2 << ","
                        << infected << "," << covidproxy << ","
                        << deadbycovid  << "," << deadbyother << "," ;
    //                 for(unsigned k=0; k<variants.size(); k++)
    //                     os << (k==infvariant ? infected : 0) << ",";
                     os << infpriorstr << "," << vaccstring << ","
                        << age << "," << grouplabel(agegroup) << ","
                        << gender2str(male)
                        << ",,";   // tbd add lastvacctime and infpriortime
                     if(os.str().size()>1000)
                     {
                         cerr << "Invalid output line" << endl;
                         REPORT("Invalid output line");
                         throw;
                     }
                     o << os.str() << endl;

                     if(isevent)
                     {
                         oe << os.str();

                         for(unsigned j=0; j<enumlabels; j++)
                             oe <<  "," << data(i,j);
                         oe << endl;
                     }
                 }
                 if(dostat)
                 {

                     recordcov(infpriorstr,isevent, stat.infprior);
                     recordcov(vaccstring,isevent,stat.vaccstatus);
                     recordcov(grouplabel(agegroup),isevent,stat.agegroup);
                 }


                 if(t2 >= enddate)
                     break;
                 if(t1 >= t2)
                 {
                     cerr << "t1 >= t2 id =" << id << endl;
                     throw;
                 }

              }
              currentinfstatus = newinfstatus;
              if(newinfection)
                lastinfection = newinfection;
//              lastvaccdate = currentvaccdate;
              currentvaccstatus = newvaccstatus;
              t1 = t2;
              if(t1 > enddate)
              {
                  cerr << "t1 > enddate=" << enddate << endl;
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
    else
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
                            cerr << "More of " << (m ? "men" : "women") << " ( " << vs[a]
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
    //                        "ID,T1,T2,Infected,Covidproxy,DeadByCovid, DeadByOther,";
                            o << i << "," << 0 << "," << T << ",";

    //                             for(unsigned i=0; i<variants.size(); i++)
    //                                 header << variants[i].codeinoutput << "Inf,";

    //                        for(unsigned k=0; k<variants.size(); k++)
    //                             o << "0,";

    //                         header << "InfPrior,VaccStatus,Age,AgeGr,Sex";

                            o << "0,0,0,0," << uninflabel << "," << unvacclabel << ","
                              << a << ","
                              << agelabel << "," << gender2str(m)
                              << ",,;"<< endl;
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
        cerr << "Cannot open " << output << "_variants.csv" << endl;
        throw;
    }

    cout << endl << "Variants of infection found during study period" << endl;
    cout << endl << "variant,outcomes,infections,discerned_in_condition,taken_as_outcome" << endl;
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
    unsigned agegrcovs;
    unsigned agegrevents;
    output("agegr",os,stat.agegroup,filter,
           agegrcovs, agegrevents);

    assert(infpriorevents == agegrevents);
    assert(vaccstatusevents == agegrevents);

    unsigned totalcovs = infpriorcovs + vaccstatuscovs + agegrcovs;
    cout << "covs="
         << totalcovs
         << ", events="
         << infpriorevents << ", eventspercov="
         << (static_cast<double>(infpriorevents) + 0.5) / totalcovs
         << endl;


}


int _main(int argc, char *argv[], bool testrun = false)
{
//for(unsigned i=0; i<argc; i++)
//    cout << argv[i] << endl;
    bool onlyfirst = argv[2][0] == '-';
    cout << "version 2.0" << endl;
    cout << "Usage convertool input output whattodo(ix) firstdate(rrrr-mm-dd) lastdate minage maxage" << endl;
    if(!testrun  && argc < 4)
        throw "at least three arguments must be given";
    {
        o2rmodes mode;
        preprocessparams ppp;

        switch(argv[3][0])
        {
        case 'i':
            mode = einfections;
            cout << "infections" << endl;
            break;
        case 'x':
            mode = ecovidproxy;
            cout << "covid proxy" << endl;
            break;
        default:
           cerr << "Unknonn option " << argv[4][0] << endl;
           throw;
        }

        ppp.groupvaccs = true;
        string firstdatestr, lastdatestr;

        if(testrun || (argv[3][1] >= 'a' && argv[3][1] <= 'z'))
        {
            if(argc < 6)
            {
                cerr << "firstdate and lastdate have to be specified." << endl;
                throw;
            }
            firstdatestr = argv[4];
            lastdatestr = argv[5];
            for(unsigned v=0; v<variants.size(); v++)
            {
                ppp.isoutcome[v] = argv[3][1]==variants[v].commandlineid[0];
            }
        }
        else
        {
            ppp.isoutcome = vector<bool>(enumvariants,false);
            ppp.conditioning = vector<bool>(enumvariants,false);
            switch(argv[3][1])
            {
            case '1':
                ppp.isoutcome[evBA12] = true;
                ppp.conditioning[evBA12] = true;
                ppp.conditioning[evDelta] = true;
                firstdatestr = "2021-10-01";
                lastdatestr = "2022-09-30";
//ppp.conditioning = vector<bool>(enumvariants,true);
//                ppp.everyn = 10;
                break;
            case '2':
                ppp.isoutcome[evBA45] = true;
                ppp.conditioning[evBA12] = true;
                ppp.conditioning[evDelta] = true;
                ppp.conditioning[evBA45] = true;
                firstdatestr = "2022-06-30";
                lastdatestr = "2022-12-01";
//ppp.conditioning = vector<bool>(enumvariants,true);
//                ppp.everyn = 10;
                break;
            case 'A': // replicating
                ppp.isoutcome[evDelta] = true;
                firstdatestr = "2021-12-10";
                lastdatestr = "2022-02-13";
//ppp.conditioning = vector<bool>(enumvariants,true);
                ppp.everyn = 5;
                break;
            case '9':
                ppp.isoutcome[evOmicron] = true;
                ppp.isoutcome[evBA12] = true;
                firstdatestr = "2021-12-10";
                lastdatestr = "2022-02-13";
//ppp.conditioning = vector<bool>(enumvariants,true);
                ppp.everyn = 1;
                break;
            default:
                ppp.isoutcome = vector<bool>(enumvariants,true);
            }
        }

        if(strlen(argv[3]) > 2)
        {
            if(argv[3][2] == '+')
                ppp.groupvaccs = false;
        }

        try
        {
            ppp.firstdate = date2int(firstdatestr);
        }
        catch(...)
        {
            cerr << "Error converting first date string "  << firstdatestr << endl;
            throw;
        }

        try
        {
            ppp.lastdate = date2int(lastdatestr);
        }
        catch(...)
        {
            cerr << "Error converting last date string " << lastdatestr << endl;
            throw;
        }

        cout << "Input " << argv[1]<< endl;



        csv<';'> data(argv[1]);
        covstat stat;

        string firststageoutputfn;
        if(testrun)
        {
            ostringstream os;
            os << argv[2] << "_firstrun.csv";
            firststageoutputfn = os.str();
        }
        else
        {
            firststageoutputfn =  "temporary";
        }

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

     }
    return 0;

}


int main(int argc, char *argv[])
{
    double startt = time(0);

    try
    {
        int testno = 0;
        if(argc == 1)
            testno = 4;
        if(argc == 2)
        {
            testno = argv[1][0] - '1' + 1;
        }

        if(testno == 0)
            return _main(argc,argv);
        else if(testno == 1)
        {
            char *as[6] ={"foo", "test_input_1.csv","test1_output.csv","i",
                          "2022-01-01","2022-11-01"};
            return _main(6,as,true);
        }
        else if(testno == 2)
        {
            char *as[6] ={"foo", "test_input_2.csv","test2_output.csv","io",
                          "2021-10-01","2022-06-30"};
            return _main(6,as,true);
        }
        else if(testno == 3)
        {
            char *as[6] ={"foo", "test_input_2.csv","test3_output.csv","xd",
                          "2021-06-01","2022-11-30"};
            return _main(6,as,true);
        }
        else if(testno == 4)
        {
            char *as[6] ={"foo", "test_input_1.csv","test4_output.csv","id",
                          "2021-12-01","2022-02-13"};
            return _main(6,as,true);
        }

    }
    catch (std::exception& e) {
        std::cerr << e.what() << endl;
        return 1;
    }
    catch (const char* m)
    {
           cerr << m << endl;
           return 1;
    }
    catch (const string& m)
    {
           cerr << m << endl;
           return 1;
    }
    catch(...)
    {
        cerr << "unknown exception" << endl;
        return 1;
    }


    cout << "time: " << time(0)-startt << " seconds." << endl;

    return 0;

}




