function WMStress_prepareForR
%Data: STRESS; WMAG task
%% TO DO: Fix correct for 101 - 113
clear all
clc
base = pwd;

datadir = 'M:\C_PhD\Stress\data';
cd(datadir)

% when stress?
%NoStressFirst = [101 103:104 113 115 117 119 121 123 125 127 129 131 135:136 139 143:144];
%StressFirst = [102 105:106 108:111 114 116 120 122 124 126 128 130 132 134 137 141 145];
Stress1 = [0 1 0 0 1 1 1 1 1 1 0 1 0 1 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 0 1 0 1 0 0 1];
Stress2 = abs(Stress1 -1);

%load DrugDecoding_all 
load PSS14-Dec-2015
load LSpan14-Dec-2015
load gender
load CORT14-Dec-2015

% cortisol
log_cort_stress =log(stress_resp);
log_cort_nostress = log(nostress_resp);
stress_log = mean(log_cort_stress(2:3,:))-log_cort_stress(1,:);
nostress_log = mean(log_cort_nostress(2:3,:))-log_cort_nostress(1,:);

CortisolDifDif = stress_log-nostress_log;
%% Start loop on defined pps
%subjects = 31;
subjects = [101:106, 108:111, 113:117, 119:132, 134:137, 139, 141, 143:145]; %DrugDecoding.AnalysisWM(:,1);
nsubjects = length(subjects);

%Data = NaN(nsubjects,2,96,19);
Data = zeros(nsubjects*96*2,13);

k=1; %counter
for subject = 1:nsubjects
    for day = 1:2
        
        filename = fullfile(datadir, sprintf('WMAG_FMRI_data_s%d_session_%d.mat', subjects(subject),day));
        load(filename);
        
        %check whether correct NaN
%          if sum(sum(isnan(correct)))>0
             lnumber = 110; rnumber = 109;
            for bl = 1:numconditions
                for i = 1:tl
                    switch probe_type_vector_mem(i,bl)
                        
                          case {0 1} % left is correct, right is wrong 
                              
                          if resp(i,bl) == lnumber
                             correct(i,bl) = 1;
                          elseif resp(i,bl) == rnumber;
                             correct(i,bl)= 0;
                          elseif resp(i,bl) == 666 
                             correct(i,bl)= 0;
                          else
                             correct(i,bl)= 0;
                          end   
                          
                          case 2 %right is correct but left is wrong.
                              
                         if resp(i,bl) == lnumber
                             correct(i,bl) = 0;
                         elseif resp(i,bl) == rnumber 
                             correct(i,bl)= 1;
                         elseif resp(i,bl) == 666 
                             correct(i,bl)= 0;
                         else
                             correct(i,bl)= 0;
                         end  
                    end
                end
%             end
           end    

        
        Trial = 1:(tl*numconditions); Trial = Trial';
        Block = repmat([1:4],tl*numconditions/4,1); Block = Block(:);
        SubNo = repmat(subjects(subject),length(Trial),1);
        Condition = trialmem(:);
        RTs = rt(:);
        Misses = resp(:); Misses(Misses==666)=0; Misses(Misses==109)=1; Misses(Misses==110)=2; %0 for no response; 1 for left, 2 for right
        ACC = correct(:);
        
        if day == 1
            Stress_condi = Stress1(subject); %letter 0 is no stress; 1 is stress
        elseif day == 2
            Stress_condi = Stress2(subject);
        end
        Stress = repmat(Stress_condi,length(Trial),1);
        Day = repmat(day, length(Trial),1);
        
        %% Questionnaires
        PSS = repmat(pss(pss(:,1)==subjects(subject),2),length(Trial),1);
        lSpan = repmat(lspan(lspan(:,1)==subjects(subject),2),length(Trial),1);
        Sex = repmat(gender(subject),length(Trial),1);
        
        Cortisol = repmat(CortisolDifDif(subject),length(Trial),1);

        TrialStruct = [SubNo Stress Day Trial Block Condition ACC RTs Misses PSS lSpan Cortisol Sex];
        Data(1+(k-1)*96:96*k,:) = TrialStruct;
        k=k+1;
    end
end
%take premature responses out (<200ms):
Data = Data(Data(:,8)>0.2,:); %1 excluded
%select Data for 2 conditions only?
Data_withoutMiss = Data(Data(:,9)>0,:);
Data_IG_UP = Data(Data(:,6)==0|Data(:,6)==2,:);
Data_IG_UP_withoutMiss = Data_IG_UP(Data_IG_UP(:,9)>0,:);
%%Save
save('WMStress_DataForR','Data','Data_IG_UP','Data_withoutMiss', 'Data_IG_UP_withoutMiss')

%% prepare for R output

%% how are variables called:
header = ('SubNo, Stress, Day, Trial, Block, Condition, ACC,RTs, Misses, PSS, lSpan, Cortisol, Sex');
mycsvwrite('WMStress_MixedModel.csv',Data,header)
mycsvwrite('WMStress_MixedModel_IGUP.csv',Data_IG_UP,header)
mycsvwrite('WMStress_MixedModel_MissesOut.csv',Data_withoutMiss,header)
mycsvwrite('WMStress_MixedModel_IGUP_MissesOut.csv',Data_IG_UP_withoutMiss,header)
end
