%==========Script to run all scripts!============
% Created by Katherine McDonald 7.20.2022

%% pilot data
%1Back
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_PRE_1B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_PRE_1B_Lures');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_POST_1B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_POST_1B_Lures');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_PRE_1B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_PRE_1B_Lures');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_POST_1B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_POST_1B_Lures');

%2Back 
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_PRE_2B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_PRE_2B_Lures');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_POST_2B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_POST_2B_Lures');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_PRE_2B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_PRE_2B_Lures');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_POST_2B_Targets');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_POST_2B_Lures');


%Flanker
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_PRE_FC');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_PA_CON_POST_FC');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_PRE_FC');
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Script_SITless_SB_CON_POST_FC');

%Prepare output for R plot script
cd '/Users/katherinemcdonald/Library/CloudStorage/Box-Box/Data_Reduction (pindus@illinois.edu)/EEG/Scripts'
run('Prepare_for_R');

%Run R Script for plots