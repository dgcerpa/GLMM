%% 3D plot of acceptance rate - Adapted for data_for_3d_matlab.csv
% Reads CSV with columns: sub, decision, c.reward, c.effort, agent, grupo
% Grid: 3 reward levels x 4 effort levels
% Groups: grupo 1 vs grupo 0
% Agent: 0 (self?) vs 1 (other?) — adjust labels below if needed

clear all; close all; clc;

%% ========== CONFIGURATION ==========
% Adjust these labels to match your study design:
groupID = {'Grupo 1', 'Grupo 0'};        % {grupo==1, grupo==0}
agentLabel = {'Self', 'Other'};           % {agent==0, agent==1} — CHANGE if reversed

%% ========== LOAD DATA ==========
T = readtable('data_for_3d_matlab.csv');

% Rename centered variables for clarity
T.Properties.VariableNames{'c_reward'} = 'reward';
T.Properties.VariableNames{'c_effort'} = 'effort';

%% ========== GET UNIQUE LEVELS ==========
effortLevels = sort(unique(T.effort));   % 4 levels
rewardLevels = sort(unique(T.reward));   % 3 levels
nEffort = length(effortLevels);
nReward = length(rewardLevels);

%% ========== SPLIT BY GROUP ==========
% grupo==1 -> group 1 (first in groupID), grupo==0 -> group 2
dat = {T(T.grupo == 1, :), T(T.grupo == 0, :)};

%% ========== COMPUTE AND PLOT ==========
figure('Position', [50 50 1400 900]);

Z = cell(2,3);

for group = 1:2

    groupData = dat{group};

    %% Step 1: Individual means per sub/effort/reward/agent
    indMeans = grpstats(groupData, {'sub','effort','reward','agent'}, 'mean', 'DataVars', 'decision');

    %% Step 2: Self (agent==0) and Other (agent==1) maps
    selfData  = indMeans(indMeans.agent == 0, :);
    otherData = indMeans(indMeans.agent == 1, :);

    %% Step 3: Individual difference maps (self - other)
    % Match rows by sub/effort/reward
    [~, iS, iO] = intersect( ...
        [selfData.sub, selfData.effort, selfData.reward], ...
        [otherData.sub, otherData.effort, otherData.reward], 'rows');
    indDiff = selfData(iS, :);
    indDiff.mean_decision = selfData.mean_decision(iS) - otherData.mean_decision(iO);

    %% Step 4: Group means
    gpSelf  = grpstats(selfData,  {'effort','reward'}, 'mean', 'DataVars', 'mean_decision');
    gpOther = grpstats(otherData, {'effort','reward'}, 'mean', 'DataVars', 'mean_decision');
    gpDiff  = grpstats(indDiff,   {'effort','reward'}, 'mean', 'DataVars', 'mean_decision');

    %% Step 5: Reshape into grid (effort x reward)
    Z{group,1} = 100 .* reshape(gpSelf.mean_mean_decision,  [nReward, nEffort])';  % Self
    Z{group,2} = 100 .* reshape(gpOther.mean_mean_decision, [nReward, nEffort])';  % Other
    Z{group,3} = 100 .* reshape(gpDiff.mean_mean_decision,  [nReward, nEffort])';  % Self-Other

    %% Step 6: Plot
    for ag = 1:3

        subplot(3, 3, (group-1)*3 + ag);

        b = bar3(Z{group, ag}, 0.7);
        colormap(jet);
        set(b, 'FaceAlpha', 0.7);
        set(b, 'LineWidth', 1);

        for k = 1:length(b)
            zdata = b(k).ZData;
            b(k).CData = zdata;
            b(k).FaceColor = 'interp';
        end

        % Labels: rows = effort, columns = reward
        xlabel('Reward level');
        xticklabels(arrayfun(@(x) sprintf('%.2f', x), rewardLevels, 'UniformOutput', false));
        ylabel('Effort level');
        yticklabels(arrayfun(@(x) sprintf('%.2f', x), effortLevels, 'UniformOutput', false));
        zlabel('Decision (%)');

        view([130 20]);

        switch ag
            case 1
                title([groupID{group} ': ' agentLabel{1} ' Effort Choices']);
                zlim([0 100]); caxis([0 100]);
            case 2
                title([groupID{group} ': ' agentLabel{2} ' Effort Choices']);
                zlim([0 100]); caxis([0 100]);
            case 3
                title([groupID{group} ': ' agentLabel{1} ' - ' agentLabel{2}]);
                zlim([-50 100]); caxis([-50 100]);
        end
        colorbar;
    end
end

%% ========== ROW 3: GROUP DIFFERENCES ==========
for ag = 1:3

    subplot(3, 3, 6 + ag);

    if ag <= 2
        % Direct group difference for self or other
        diffZ = Z{1, ag} - Z{2, ag};
    else
        % (Other-Self) difference between groups
        % Group1: other - self
        group1_diff = Z{1,2} - Z{1,1};
        % Group2: other - self
        group2_diff = Z{2,2} - Z{2,1};
        diffZ = group1_diff - group2_diff;
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
    caxis([-20 20]);
    zlim([-20 20]);

    switch ag
        case 1
            title([groupID{1} ' - ' groupID{2} ': ' agentLabel{1} ' Choices']);
        case 2
            title([groupID{1} ' - ' groupID{2} ': ' agentLabel{2} ' Choices']);
        case 3
            title([groupID{1} ' - ' groupID{2} ': Other-Self Choices']);
    end
    colorbar;
end

sgtitle('3D Acceptance Rate by Effort x Reward x Agent x Group', 'FontSize', 14, 'FontWeight', 'bold');
