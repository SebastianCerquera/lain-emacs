import { test, expect } from '../fixtures/containerFixture';

test.describe('Lain-Emacs Priority Task Clicking Repro', () => {

    test.beforeEach(async ({ basePage, scrumPage }) => {
        await basePage.goto();
        await basePage.authenticate();
        await scrumPage.accessScrumView();
    });

    test('should click task with [#A] priority', async ({ scrumPage, taskViewPage }) => {
        const taskName = 'dolore, 2025-04, 1, lorem, RESPONSABILITIES';
        await scrumPage.clickTask(taskName);
        await taskViewPage.verifyTaskContent(taskName);
    });

    test('should click task with [#B] priority', async ({ scrumPage, taskViewPage }) => {
        const taskName = 'Priority B Task, 2025-04, 1, lorem, RESPONSABILITIES';
        await scrumPage.clickTask(taskName);
        await taskViewPage.verifyTaskContent(taskName);
    });

    test('should click task with [#C] priority', async ({ scrumPage, taskViewPage }) => {
        const taskName = 'Priority C Task, 2025-04, 1, lorem, RESPONSABILITIES';
        await scrumPage.clickTask(taskName);
        await taskViewPage.verifyTaskContent(taskName);
    });

});
