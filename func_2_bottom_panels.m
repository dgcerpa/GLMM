%% 3D plot - Group Difference Panels (B, C, D)
% Shows only the between-group difference for Self, Other, and Other-Self
% Adapted for data_for_3d_matlab.csv (3 reward x 4 effort)

clear all; close all; clc;

%% ========== CONFIGURATION ==========
% Adjust group labels to your study:
groupID = {'Grupo 1', 'Grupo 0'};        % {grupo==1, grupo==0}
agentLabel = {'Self', 'Other'};           % {agent==0, agent==1} — CHANGE if reversed

% Title prefix for between-group comparison:
diffLabel = 'Low-High Hypocritical Blame';

%% ========== LOAD DATA ==========
T = readtable('data_for_3d_matlab.csv');
T.Properties.VariableNames{'c_reward'} = 'reward';
T.Properties.VariableNames{'c_effort'} = 'effort';

%% ========== GET UNIQUE LEVELS ==========
effortLevels = sort(unique(T.effort));   % 4 levels
rewardLevels = sort(unique(T.reward));   % 3 levels
nEffort = length(effortLevels);
nReward = length(rewardLevels);

%% ========== SPLIT BY GROUP ==========
dat = {T(T.grupo == 1, :), T(T.grupo == 0, :)};

%% ========== COMPUTE GROUP MAPS ==========
Z = cell(2,2); % Z{group, agent}: group 1-2, agent 1=self 2=other

for group = 1:2

    groupData = dat{group};

    % Individual means per sub/effort/reward/agent
    indMeans = grpstats(groupData, {'sub','effort','reward','agent'}, 'mean', 'DataVars', 'decision');

    selfData  = indMeans(indMeans.agent == 0, :);
    otherData = indMeans(indMeans.agent == 1, :);

    % Group means
    gpSelf  = grpstats(selfData,  {'effort','reward'}, 'mean', 'DataVars', 'mean_decision');
    gpOther = grpstats(otherData, {'effort','reward'}, 'mean', 'DataVars', 'mean_decision');

    Z{group,1} = 100 .* reshape(gpSelf.mean_mean_decision,  [nReward, nEffort])';   % Self
    Z{group,2} = 100 .* reshape(gpOther.mean_mean_decision, [nReward, nEffort])';    % Other
end

%% ========== PLOT: 3 PANELS (B, C, D) ==========
figure('Position', [100 200 1400 400]);

panelLabels = {'B', 'C', 'D'};

for ag = 1:3

    subplot(1, 3, ag);

    if ag == 1
        % Panel B: Group difference in SELF choices
        diffZ = Z{1,1} - Z{2,1};
        panelTitle = [diffLabel ': ' agentLabel{1} ' Effort Choices'];

    elseif ag == 2
        % Panel C: Group difference in OTHER choices
        diffZ = Z{1,2} - Z{2,2};
        panelTitle = [diffLabel ': ' agentLabel{2} ' Effort Choices'];

    else
        % Panel D: Group difference in (Other - Self)
        group1_otherMinusSelf = Z{1,2} - Z{1,1};
        group2_otherMinusSelf = Z{2,2} - Z{2,1};
        diffZ = group1_otherMinusSelf - group2_otherMinusSelf;
        panelTitle = [diffLabel ': Other-Self Effort Choices'];
    end

    b = bar3(diffZ, 0.7);
    colormap(jet);
    set(b, 'FaceAlpha', 0.7);
    set(b, 'LineWidth', 1);

    for k = 1:length(b)
        zdata = b(k).ZData;
        b(k).CData = zdata;
        b(k).FaceColor = 'interp';
    end

    xlabel('Reward level', 'FontSize', 12);
    xticklabels(arrayfun(@(x) sprintf('%.2f', x), rewardLevels, 'UniformOutput', false));
    ylabel('Effort level', 'FontSize', 12);
    yticklabels(arrayfun(@(x) sprintf('%.2f', x), effortLevels, 'UniformOutput', false));
    zlabel('Decision (%)', 'FontSize', 12);

    view([130 20]);
    zlim([-15 15]);
    caxis([-15 15]);
    title([panelLabels{ag} '   ' panelTitle], 'FontSize', 11, 'FontWeight', 'bold');
end